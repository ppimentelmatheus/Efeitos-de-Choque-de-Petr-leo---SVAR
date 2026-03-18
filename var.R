#######################################################################
################ VAR 

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
library(writexl)

# Dados -------------------------------------------------------------------


# Ler dados diretamente ---------------------------------------------------

raw_oil <- readr::read_rds("data/raw_data.rds")

raw_fred = raw_oil[["raw_fred"]]


# Coleta ------------------------------------------------------------------

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

# Exportar dados para Excel
#writexl::write_xlsx(x=df_raw, "df_raw.xlsx")

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

# Exportar dados de choques


# Coletar dados BCB -------------------------------------------------------

# Coleta de dados

raw_dados_bcb = raw_oil[["raw_dados_bcb"]]

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

plot(irf_supply, type="l") # Impacto do aumento de oferta
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

# Coletar coeficientes
coef_supply <- coef(model_pi)[grep("oil_supply", names(coef(model_pi)))]
coef_demand <- coef(model_pi)[grep("oil_demand", names(coef(model_pi)))]
coef_spec   <- coef(model_pi)[grep("oil_spec_demand", names(coef(model_pi)))]


irf_supply <- c(0, irf_supply)
irf_demand <- c(0, irf_demand)
irf_spec <- c(0, irf_spec)

plot(irf_supply, type="l")
plot(irf_demand, type="l")
plot(irf_spec, type="l")

irf_df <- tibble(
  horizon = 0:(length(irf_supply)-1),
  supply = irf_supply,
  demand = irf_demand,
  spec = irf_spec
) |>
  pivot_longer(
    -horizon,
    names_to = "shock",
    values_to = "irf"
  )

irf_df$shock <- recode(
  irf_df$shock,
  supply = "Oil Supply Shock",
  demand = "Global Demand Shock",
  spec   = "Oil-Specific Demand Shock"
)

ggplot(irf_df, aes(x = horizon, y = irf)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  facet_wrap(~shock, ncol = 3) +
  labs(
    x = "Meses",
    y = "Resposta acumulada da inflação",
    title = "Impacto dos choques do petróleo sobre a inflação"
  ) +
  theme_minimal(base_size = 14)+
  geom_hline(yintercept = 0, linetype = "dashed")


# Construir intervalos de confiança ---------------------------------------

coefs <- summary(model_y)$coefficients

supply_rows <- grep("oil_supply", rownames(coefs))
demand_rows <- grep("oil_demand_lag", rownames(coefs))
spec_rows   <- grep("oil_spec_demand", rownames(coefs))

coef_supply <- coefs[supply_rows, "Estimate"]
se_supply   <- coefs[supply_rows, "Std. Error"]

coef_demand <- coefs[demand_rows, "Estimate"]
se_demand   <- coefs[demand_rows, "Std. Error"]

coef_spec <- coefs[spec_rows, "Estimate"]
se_spec   <- coefs[spec_rows, "Std. Error"]


irf_supply <- cumsum(coef_supply)
irf_demand <- cumsum(coef_demand)
irf_spec   <- cumsum(coef_spec)

irf_supply <- c(irf_supply)
irf_demand <- c(irf_demand)
irf_spec <- c(irf_spec)

ci_supply <- 1.96 * sqrt(cumsum(se_supply^2))
ci_demand <- 1.96 * sqrt(cumsum(se_demand^2))
ci_spec   <- 1.96 * sqrt(cumsum(se_spec^2))

lower_supply <- irf_supply - ci_supply
upper_supply <- irf_supply + ci_supply

lower_demand <- irf_demand - ci_demand
upper_demand <- irf_demand + ci_demand

lower_spec <- irf_spec - ci_spec
upper_spec <- irf_spec + ci_spec

irf_df <- tibble(
  horizon = 1:length(irf_supply),
  
  supply = irf_supply,
  lower_supply = lower_supply,
  upper_supply = upper_supply,
  
  demand = irf_demand,
  lower_demand = lower_demand,
  upper_demand = upper_demand,
  
  spec = irf_spec,
  lower_spec = lower_spec,
  upper_spec = upper_spec
)

# Transformar para formato longo
irf_long <- bind_rows(
  tibble(
    horizon = irf_df$horizon,
    irf = irf_df$supply,
    lower = irf_df$lower_supply,
    upper = irf_df$upper_supply,
    shock = "Oil Supply Shock"
  ),
  tibble(
    horizon = irf_df$horizon,
    irf = irf_df$demand,
    lower = irf_df$lower_demand,
    upper = irf_df$upper_demand,
    shock = "Global Demand Shock"
  ),
  tibble(
    horizon = irf_df$horizon,
    irf = irf_df$spec,
    lower = irf_df$lower_spec,
    upper = irf_df$upper_spec,
    shock = "Oil-Specific Demand Shock"
  )
)

# Incluir zeros
irf_long <- bind_rows(
  tibble(
    horizon = 0,
    irf = 0,
    lower = 0,
    upper = 0,
    shock = unique(irf_long$shock)
  ),
  irf_long
)
# Ordenar
irf_long <- irf_long |>
  arrange(shock, horizon)

ggplot(irf_long, aes(horizon, irf)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "steelblue",
              alpha = 0.2) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~shock, ncol = 3) +
  labs(
    x = "Meses",
    y = "Resposta acumulada da inflação",
    title = "Impacto dos choques do petróleo sobre a inflação Brasileira",
    caption = "Elaboração: Matheus Porto Pimentel,
    Dados: EIA, FRED, FED Dallas e BCB"
  ) +
  theme_minimal(base_size = 14)


### Magnitude dos choaque

sd(oil_shocks$oil_supply)
sd(oil_shocks$oil_demand)
sd(oil_shocks$oil_spec_demand)
B
