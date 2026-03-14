# Pacotes -----------------------------------------------------------------

library(vars)
library(svars)
library(tidyverse)
library(zoo)
library(boot)
library(quantmod)
library(dplyr)
library(lubridate)
library(tsibble)
library(GetBCBData)
library(dynlm)
library(purrr)



# Dados -------------------------------------------------------------------

# Price Brent
tickers_fred = c("DCOILBRENTEU") 
raw_fred <- purrr::map(
  .x = tickers_fred, # Brent
  .f = ~quantmod::getSymbols(
    Symbols     = .x,
    src         = "FRED",
    auto.assign = FALSE
  )
)

df_fred <- raw_fred  |> 
  purrr::map(.f = timetk::tk_tbl)  |> 
  purrr::reduce(.f = dplyr::full_join, by = "index")  |> 
  dplyr::rename(
    dplyr::all_of(
      c("date"="index",
        "brent"="DCOILBRENTEU")
    )
  ) |> 
  dplyr::arrange(.data$date) |> 
  #dplyr::filter(date >= init_date) |> 
  tidyr::fill(dplyr::everything(), .direction = 'updown')  |> 
  dplyr::mutate(date = tsibble::yearmonth(date)) |> 
  dplyr::group_by(date) |> 
  dplyr::summarise(brent = mean(brent)) |> 
  dplyr::ungroup()

ggplot2::ggplot(df_fred, aes(x=as.Date(date), y=brent))+
  geom_line()+
  labs(title = "Brent Crude Oil")

ggplot2::ggplot(df_fred, aes(x=as.Date(date), y=log(brent)))+
  geom_line()+
  labs(title = "Log Brent Crude Oil")


# produção mundial de petróleo

oil_prod = read_csv(file = "supply.csv")

oil_prod = oil_prod |> 
  dplyr::mutate(date = mdy(date)) |> 
  dplyr::mutate(date = tsibble::yearmonth(date)) |> 
  dplyr::group_by(date) |> 
  dplyr::summarise(volume = mean(volume)) |> 
  dplyr::ungroup()

ggplot2::ggplot(oil_prod, aes(x=as.Date(date), y=volume))+
  geom_line()

# índice de atividade global (Kilian index)

activity <- readxl::read_excel(
  "igrea.xlsx") |> 
  dplyr::mutate(date = tsibble::yearmonth(date)) |> 
  dplyr::mutate(global_activity = as.numeric(global_activity))

ggplot2::ggplot(activity, aes(x=as.Date(date), y=global_activity))+
  geom_line()
                  
# Juntar dados

df_raw = purrr::reduce(
  .x = list(activity, df_fred, oil_prod),
  .f = full_join,
  by = 'date'
) |> drop_na() |> filter(date >= tsibble::yearmonth("2003 Jan"))

df = df_raw |> 
  dplyr::mutate(
    d_oilprod = log(volume) - log(dplyr::lag(volume)),
    rop = log(brent),
    rea = (global_activity - mean(global_activity) / sd(global_activity))
  ) |> # Dados de jun 87 a fev 2026
  drop_na()

df_ts = df_raw |> 
  dplyr::mutate(
    d_oilprod = log(volume) - log(dplyr::lag(volume)),
    rop = log(brent),
    rea = (global_activity - mean(global_activity) / sd(global_activity))
  ) |> # Dados de jun 87 a fev 2026
  drop_na() |> 
  dplyr::select(
    d_oilprod, rea, rop
  )

# VAR ---------------------------------------------------------------------

## Estimando o VAR
var_model <- vars::VAR(
  df_ts |> dplyr::select(d_oilprod, rea, rop),
  p = 12,
  type = "const"
)

eps = residuals(var_model)

## Decomposição via Cholesky
svar_model = id.chol(var_model)
svar_model

# matriz B estimada
B <- svar_model$B

# Funções de Resposta ao Impulso

irf_oil <- irf(
  svar_model,
  impulse = "rop",
  response = c("d_oilprod", "rea", "rop"),
  n.ahead = 24,
  boot = TRUE,
  ci = 0.95
)

plot(irf_oil)

# choques estruturais
oil_shocks <- t(solve(B) %*% t(eps))

colnames(oil_shocks) <- c("oil_supply",
                          "oil_demand",
                          "oil_spec_demand")

oil_shocks = oil_shocks |> as_tibble()

head(oil_shocks)

# Choques não correlacionados
cor(oil_shocks)


# Coletar dados BCB -------------------------------------------------------

# Coleta de dados


# Definir data inicial
init_date <- lubridate::as_date("2002-01-01")

# Buscar dados no SGS/BCB
raw_dados_bcb = GetBCBData::gbcbd_get_series(
  id = c("ipca" = 433, "ibc_br" = 24364, "selic" = 432, "m2" = 27810,
         "reef" = 11752),
  first.date = init_date,
  format.data = "wide",
  use.memoise = FALSE
) |> 
  dplyr::rename("date" = "ref.date") |> 
  dplyr::mutate(date = as.Date(date))

# Salvar dados brutos para reprodução
readr::write_rds(x = mget(ls(pattern = "raw_")), file = "./data/raw_data.rds")

dados_bcb <- raw_dados_bcb |> 
  dplyr::mutate(date = tsibble::yearmonth(date)) |> 
  group_by(date) |> 
  summarise(
    dplyr::across(
      c(ipca, ibc_br, selic, m2, reef),
      ~ mean(.x, na.rm = TRUE)
  )) |> 
  ungroup() |> 
  # Retira a última linha de abril pois estava sem data
  dplyr::slice(-dplyr::n()) |> 
  # Preenche as colunas com NA para aplicar o fill
  dplyr::mutate(
    dplyr::across(-date, ~ na_if(.x, NaN))
  ) |> 
  # Preenche as últimas linhas repetindo as últimas observações
  # Forward Fill
  tidyr::fill(-date, .direction = 'down') |> 
  # Retira as primeiras observações com NA
  drop_na() |> 
  dplyr::slice(-(1:13))


### Juntar dados

oil_shocks <- oil_shocks |>
  mutate(date = dados_bcb$date) |>
  as_tsibble(index = date)

oil_shocks$date <- tsibble::yearmonth(oil_shocks$date)

dataset <- dados_bcb |>
  left_join(oil_shocks, by = "date") |> as_tsibble(index = 'date')

####### Estimar impulse responses


# Criar defasagens
dataset_lags <- dataset |>
  mutate(
    d_ibc = ibc_br - lag(ibc_br)
  ) |>
  bind_cols(
    map_dfc(1:12, ~lag(dataset$oil_supply, .x)) |>
      setNames(paste0("oil_supply_lag", 1:12)),
    map_dfc(1:12, ~lag(dataset$oil_demand, .x)) |>
      setNames(paste0("oil_demand_lag", 1:12)),
    map_dfc(1:12, ~lag(dataset$oil_spec_demand, .x)) |>
      setNames(paste0("oil_spec_demand_lag", 1:12))
  ) |>
  tidyr::drop_na()


model_y <- lm(
  d_ibc ~
    oil_supply_lag1 + oil_supply_lag2 + oil_supply_lag3 +
    oil_supply_lag4 + oil_supply_lag5 + oil_supply_lag6 +
    oil_supply_lag7 + oil_supply_lag8 + oil_supply_lag9 +
    oil_supply_lag10 + oil_supply_lag11 + oil_supply_lag12 +
    oil_demand_lag1 + oil_demand_lag2 + oil_demand_lag3 +
    oil_demand_lag4 + oil_demand_lag5 + oil_demand_lag6 +
    oil_demand_lag7 + oil_demand_lag8 + oil_demand_lag9 +
    oil_demand_lag10 + oil_demand_lag11 + oil_demand_lag12 +
    oil_spec_demand_lag1 + oil_spec_demand_lag2 + oil_spec_demand_lag3 +
    oil_spec_demand_lag4 + oil_spec_demand_lag5 + oil_spec_demand_lag6 +
    oil_spec_demand_lag7 + oil_spec_demand_lag8 + oil_spec_demand_lag9 +
    oil_spec_demand_lag10 + oil_spec_demand_lag11 + oil_spec_demand_lag12,
  data = dataset_lags
)

# IRF Cumulativa

coef_supply <- coef(model_y)[grep("oil_supply", names(coef(model_y)))]
coef_demand <- coef(model_y)[grep("oil_demand", names(coef(model_y)))]
coef_spec   <- coef(model_y)[grep("oil_spec_demand", names(coef(model_y)))]

irf_supply <- cumsum(coef_supply)
irf_demand <- cumsum(coef_demand)
irf_spec   <- cumsum(coef_spec)

plot(irf_supply, type="l")
plot(irf_demand, type="l")
plot(irf_spec, type="l")


# Inflação


model_pi <- lm(
  ipca ~
    oil_supply_lag1 + oil_supply_lag2 + oil_supply_lag3 +
    oil_supply_lag4 + oil_supply_lag5 + oil_supply_lag6 +
    oil_supply_lag7 + oil_supply_lag8 + oil_supply_lag9 +
    oil_supply_lag10 + oil_supply_lag11 + oil_supply_lag12 +
    oil_demand_lag1 + oil_demand_lag2 + oil_demand_lag3 +
    oil_demand_lag4 + oil_demand_lag5 + oil_demand_lag6 +
    oil_demand_lag7 + oil_demand_lag8 + oil_demand_lag9 +
    oil_demand_lag10 + oil_demand_lag11 + oil_demand_lag12 +
    oil_spec_demand_lag1 + oil_spec_demand_lag2 + oil_spec_demand_lag3 +
    oil_spec_demand_lag4 + oil_spec_demand_lag5 + oil_spec_demand_lag6 +
    oil_spec_demand_lag7 + oil_spec_demand_lag8 + oil_spec_demand_lag9 +
    oil_spec_demand_lag10 + oil_spec_demand_lag11 + oil_spec_demand_lag12,
  data = dataset_lags
)

# IRF Cumulativa

coef_supply <- coef(model_pi)[grep("oil_supply", names(coef(model_pi)))]
coef_demand <- coef(model_pi)[grep("oil_demand", names(coef(model_pi)))]
coef_spec   <- coef(model_pi)[grep("oil_spec_demand", names(coef(model_pi)))]

irf_supply <- cumsum(coef_supply)
irf_demand <- cumsum(coef_demand)
irf_spec   <- cumsum(coef_spec)

plot(irf_supply, type="l")
plot(irf_demand, type="l")
plot(irf_spec, type="l")

    

