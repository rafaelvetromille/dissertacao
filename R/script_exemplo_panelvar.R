############################################################################
############################################################################
###                                                                      ###
###                      PACOTE 'PANELVAR' EXEMPLOS                      ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)
library(panelvar)

# 1. EXEMPLO PACOTE 'PANELVAR' (BY ARELLANO AND BOND - 1991) --------------

data("abdata", package = "panelvar")

dados <- abdata %>%
  select(!c1 & !ind & !rec & !yearm1) %>%
  relocate(id, .before = everything()) %>%
  relocate(year, .after = id) %>%
  arrange(id, year)

arellano_bond_a1 <- pvargmm(
  dependent_vars = c("n"), lags = 2,
  exog_vars = c(
    "w", "wL1", "k", "kL1", "kL2", "ys", "ysL1", "ysL2",
    "yr1979", "yr1980", "yr1981", "yr1982", "yr1983", "yr1984"),
  transformation = "fd",
  data = dados,
  panel_identifier = c("id", "year"),
  steps = c("onestep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = FALSE)

summary(arellano_bond_a1)

arellano_bond_a2 <- pvargmm(
  dependent_vars = c("n"), lags = 2,
  exog_vars = c(
    "w", "wL1", "k", "kL1", "kL2", "ys", "ysL1", "ysL2",
    "yr1979", "yr1980", "yr1981", "yr1982", "yr1983", "yr1984"),
  transformation = "fd",
  data = dados,
  panel_identifier = c("id", "year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = FALSE)

arellano_bond_b <- pvargmm(
  dependent_vars = c("n"), lags = 2,
  exog_vars = c(
    "w", "wL1", "k", "ys", "ysL1",
    "yr1979", "yr1980", "yr1981", "yr1982", "yr1983", "yr1984"),
  transformation = "fd",
  data = dados,
  panel_identifier = c("id", "year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = FALSE)

summary(arellano_bond_a2)

arellano_bond_a2_coll_fod <- pvargmm(
  dependent_vars = c("n"),
  lags = 2,
  exog_vars = c(
    "w", "wL1", "k", "ys", "ysL1",
    "yr1979", "yr1980", "yr1981", "yr1982", "yr1983", "yr1984"),
  transformation = "fod",
  data = abdata,
  panel_identifier = c("id", "year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  max_instr_predet_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = TRUE)

summary(arellano_bond_a2_coll_fod)

# 2. EXEMPLO PACOTE 'PANELVAR' (BY BLUNDELL AND BOND - 1998) --------------

###  Our code is not designed for instrumenting each predetermined variables
###  with a different number of lagged instruments. Furthermore, please note
###  that the results in Blundell and Bond (1998) are misleading, as they do
###  not use the correct weighting matrix, in particular Eq. (25).

blundell_bond_table4 <- pvargmm(
  dependent_vars = c("n"),
  lags = 1,
  predet_vars = c("w", "wL1", "k", "kL1"),
  exog_vars = c("yr1978", "yr1979", "yr1980", "yr1981", "yr1982", "yr1983", "yr1984"),
  transformation = "fd",
  data = dados,
  panel_identifier = c("id", "year"),
  steps = c("twostep"),
  system_instruments = TRUE,
  max_instr_dependent_vars = 99,
  max_instr_predet_vars = 99,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE)

summary(blundell_bond_table4)

###########################################################################
###########################################################################
###                                                                     ###
###             VETORES AUTOREGRESSIVOS COM DADOS EM PAINEL             ###
###                                                                     ###
###########################################################################
###########################################################################

# 1. EXEMPLO PACOTE 'PANELVAR' - PVAR BY DAHLBERG AND JOHANSSON (2000) ----

data("Dahlberg", package = "panelvar")

dados <- Dahlberg %>%
  arrange(id, year)

dados %>%
  tidyr::gather(expenditures:grants, key = 'variavel', value = 'valor') %>%
  as_tibble()

dados %>%
  pivot_longer(
    cols = expenditures:grants,
    names_to = "variavel",
    values_to = "valor"
  )

ex1_dahlberg_data <- pvargmm(
  dependent_vars = c("expenditures", "revenues", "grants"),
  lags = 1,
  transformation = "fod",
  data = dados,
  panel_identifier = c("id", "year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  max_instr_predet_vars = 99,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE)

summary(ex1_dahlberg_data)

# 2. SELECIONAR O LAG ÓTIMO -----------------------------------------------

lag_selection <- function(x){

  teste <- suppressWarnings(pvargmm(
    dependent_vars = c("expenditures", "revenues", "grants"),
    lags = x,
    transformation = "fod",
    data = dados,
    panel_identifier = c("id", "year"),
    steps = c("twostep"),
    system_instruments = FALSE,
    max_instr_dependent_vars = 99,
    max_instr_predet_vars = 99,
    min_instr_dependent_vars = 2L,
    min_instr_predet_vars = 1L,
    collapse = FALSE))

  suppressWarnings(Andrews_Lu_MMSC(teste))

}

## Selação de Lag - Lag(1)
lag_selection(1)

## Selação de Lag - Lag(2)
lag_selection(2)

## Selação de Lag - Lag(3)
lag_selection(3)

## Selação de Lag - Lag(4)
lag_selection(4)

## Estabilidade do PVAR

stab_ex1_dahlberg_data <- stability(ex1_dahlberg_data)

print(stab_ex1_dahlberg_data)

plot(stab_ex1_dahlberg_data) +
  theme_bw()

## Funções de Impulso Resposta

ex1_dahlberg_data_oirf <- oirf(ex1_dahlberg_data, n.ahead = 36)

ex1_dahlberg_data_bs <- bootstrap_irf(ex1_dahlberg_data, typeof_irf = c("OIRF"),
                                      n.ahead = 8,
                                      nof_Nstar_draws = 5,
                                      confidence.band = 0.95)

ex1_dahlberg_data_girf <- girf(ex1_dahlberg_data, n.ahead = 36, ma_approx_steps= 36)

ex1_dahlberg_data_bs <- bootstrap_irf(ex1_dahlberg_data, typeof_irf = c("GIRF"),
                                      n.ahead = 36,
                                      nof_Nstar_draws = 500,
                                      confidence.band = 0.95)

plot(ex1_dahlberg_data_oirf, ex1_dahlberg_data_bs)


