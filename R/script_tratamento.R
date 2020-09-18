
# 1. PACOTES UTILIZADOS ---------------------------------------------------

library(tidyverse)
library(readxl)

# 2. EXCEL SHEETS ---------------------------------------------------------

readxl::excel_sheets('./data-raw/dados_raw.xlsx')

# VARIÁVEL 1: CÓDIGOS DOS PAÍSES DA BASE DE DADOS (ISO) -------------------

codes <- readxl::read_excel(path = './data-raw/dados_raw.xlsx',
                            sheet = "country codes") %>%
  mutate(countries = ifelse(countries == "south korea", "korea", countries))

# VARIÁVEL 2: TAXA DE DESEMPREGO (MENSAL) ---------------------------------

unem_rate <- readxl::read_excel(
  path = './data-raw/dados_raw.xlsx',
  skip = 3,
  sheet = "unemployment rate",
  col_types = c("date", rep("numeric", 26))) %>%
  janitor::clean_names() %>%
  dplyr::rename(date = name) %>%
  dplyr::mutate(date = as.Date(lubridate::floor_date(date, unit = "month"))) %>%
  dplyr::rename_at(vars(-date),
                   funs(
                     stringr::str_sub(., end = 2) %>%
                       stringr::str_to_upper(.) %>%
                       stringr::str_c(., "_UR")
                   )) %>%
  dplyr::filter(date >= "1985-01-01" & date <= "2020-07-01") %>%
  tidyr::pivot_longer(ends_with("UR"),
                      names_to = 'countries',
                      values_to = 'un') %>%
  dplyr::mutate(countries = stringr::str_remove(countries, "\\_UR$")) %>%
  dplyr::relocate(countries, .before = date) %>%
  dplyr::arrange(countries, date)


# VARIÁVEL 3: PRODUÇÃO INDUSTRIAL (MENSAL) --------------------------------

ind_prod <- readxl::read_excel(
  path = './data-raw/dados_raw.xlsx',
  skip = 3,
  sheet = "industrial production",
  col_types = c("date", rep("numeric", 26))) %>%
  janitor::clean_names() %>%
  dplyr::rename(date = name) %>%
  dplyr::mutate(date = as.Date(lubridate::floor_date(date, unit = "month"))) %>%
  dplyr::rename_at(vars(-date),
                   funs(
                     stringr::str_sub(., end = 2) %>%
                       stringr::str_to_upper(.) %>%
                       stringr::str_c(., "_IP")
                   )) %>%
  dplyr::filter(date >= "1985-01-01" & date <= "2020-07-01") %>%
  tidyr::pivot_longer(ends_with("IP"),
                      names_to = 'countries',
                      values_to = 'ip') %>%
  dplyr::mutate(countries = stringr::str_remove(countries, "\\_IP$")) %>%
  dplyr::relocate(countries, .before = date) %>%
  dplyr::arrange(countries, date)

# VARIÁVEL 4: EPU INDEX (MENSAL) ------------------------------------------

epu_index <- readxl::read_excel(
  path = './data-raw/All_Country_Data.xlsx',
  col_types = rep("numeric", 28)) %>%
  rename_all(tolower) %>%
  rename(usa = us) %>%
  mutate(date = lubridate::make_date(year = year, month = month)) %>%
  select(!month & !year) %>%
  relocate(date, .before = where(is.numeric)) %>%
  tidyr::pivot_longer(!date,
                      names_to = 'countries',
                      values_to = 'epu') %>%
  dplyr::arrange(countries, date) %>%
  left_join(codes, by = 'countries') %>%
  select(countries = codes, date, epu)

# VARIÁVEL 5: COUNTRY STOCKS (MENSAL) -------------------------------------

country_stocks <- readxl::read_excel(
    path = './data-raw/dados_raw.xlsx',
    skip = 3,
    sheet = "country stocks msci local",
    col_types = c("date", rep("numeric", 26))) %>%
  dplyr::rename(date = Name) %>%
  group_by(year = lubridate::year(date), month = lubridate::month(date)) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = T))) %>%
  ungroup() %>%
  mutate(date = as.Date(lubridate::make_date(year = year, month = month)),
         across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x))) %>%
  dplyr::select(!month & !year) %>%
  dplyr::relocate(date, .before = where(is.numeric)) %>%
  dplyr::rename_at(vars(-date),
                   funs(
                     stringr::str_sub(., end = 2) %>%
                       stringr::str_to_upper(.) %>%
                       stringr::str_c(., "_STOCK"))) %>%
  dplyr::filter(date >= "1985-01-01" & date <= "2020-07-01") %>%
  tidyr::pivot_longer(ends_with("STOCK"),
                      names_to = 'countries',
                      values_to = 'stock') %>%
  dplyr::mutate(countries = stringr::str_remove(countries, "\\_STOCK$")) %>%
  dplyr::relocate(countries, .before = date) %>%
  dplyr::arrange(countries, date)

# 3. ARRUMANDO DADOS EM FORMATO DE PAINEL ---------------------------------

dados <- list(ind_prod, unem_rate, country_stocks) %>%
  reduce(full_join, by = c('countries', 'date')) %>%
  full_join(epu_index, by = c('countries', 'date')) %>%
  filter(date >= "2000-01-01")

# 4. SALVANDO BASE DE DADOS -----------------------------------------------

xlsx::write.xlsx(x = dados,
                 file = './data-raw/dados.xlsx',
                 sheetName = 'painel')
