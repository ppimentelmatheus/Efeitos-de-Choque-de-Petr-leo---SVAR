# Efeitos de Choque de Petróleo sobre - SVAR
Efeitos de Choque de Petróleo - SVAR sobre a economia brasileira

Resumo da metodologia do artigo:

- SVAR do mercado de petróleo (primeira etapa)
Vetor de variáveis:

Δ produção mundial de petróleo

preço real do petróleo

atividade econômica global

- Esse SVAR identifica três choques:

choque de oferta de petróleo

choque de demanda global

choque de demanda específica do mercado de petróleo

- Segunda etapa:

Os choques identificados são usados em regressões para estimar impulse responses cumulativas sobre:

atividade econômica

inflação

taxa de juros

taxa de câmbio

oferta monetária

As IRFs são estimadas com 12 defasagens mensais e intervalos de confiança.

Pipeline:

1) Constrir base de dados Mensal;
2) Estimar SVAR do mercado de trabalho através da decomposição de Cholesky;
3) Extrair choques estruturais;
4) Regressão dos choques em variáveis macro do Brasil;
5) Calcular IRFs.

Dados:

| variável                                  |    fonte         |
| ----------------------------------------- | ---------------- |
| produção mundial de petróleo              | EIA              |
| preço Brent                               | FRED/EIA         |
| índice de atividade global (Kilian index) | Kilian dataset   |
| atividade econômica Brasil                | IBC-Br           |
| inflação                                  | IPCA             |
| juros                                     | Selic            |
| câmbio real efetivo                       | Banco Central    |
| M2                                        | Banco Central    |
