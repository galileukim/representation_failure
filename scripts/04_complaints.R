# analyze data from falabr on citizen complaints
# available in: https://www.gov.br/cgu/pt-br/acesso-a-informacao/dados-abertos/arquivos/ouvidoria
library(readr)
library(dplyr)
library(purrr)
library(here)
library(lubridate)

# read-in data -----------------------------------------------------------
complaints_files <- list.files(
  here("data", "input", "falabr"),
  full.names = TRUE
)

complaints <- complaints_files |> 
  set_names(
    2014:2018
  ) |> 
  map(
    \(f) read_csv2(f, locale = readr::locale(encoding = "windows-1252"))
  ) |>
  bind_rows(
    .id = "year"
  ) |> 
  janitor::clean_names()

municipios <- read_csv(
  here("data", "input", "municipal", "municipios.csv")
)

estados <- read_csv(
  here("data", "input", "municipal", "state_id.csv")
)

# clean data -------------------------------------------------------------
complaints <- complaints |> 
  filter(
    esfera == "Municipal",
    tipo_manifestacao %in% c("Denúncia", "Reclamação"),
    year <= 2018,
    dmy(data_registro) <= ymd("2018-07-20")
  ) 

complaints_municipal <- complaints |> 
  count(
    municipio_do_orgao,
    uf_do_orgao,
    year,
    name = "total_complaints"
  ) |> 
  select(
    nome_municipio = municipio_do_orgao,
    uf = uf_do_orgao,
    year,
    total_complaints
  )

# recover cod_ibge_6
municipios <- municipios |> 
  left_join(
    estados,
    by = c("codigo_uf" = "state_id")
  ) |> 
  transmute(
    cod_ibge_6,
    nome_municipio = stringr::str_to_upper(nome_municipio),
    codigo_uf,
    uf = state
  )

complaints_municipal <- complaints_municipal |> 
  # fix two municipality name errors
  mutate(
    nome_municipio = case_when(
      nome_municipio == "ELDORADO DOS CARAJÁS" ~ "ELDORADO DO CARAJÁS",
      nome_municipio == "TRAJANO DE MORAIS" ~ "TRAJANO DE MORAES", 
      T ~ nome_municipio
    )
  ) |>
  left_join(
    municipios,
    by = c("nome_municipio", "uf")
  ) |> 
  select(
    cod_ibge_6,
    municipality_name = nome_municipio,
    cod_uf = codigo_uf,
    uf,
    year,
    total_complaints
  )

# write-out --------------------------------------------------------------
complaints_municipal |> 
  write_csv(
    here("data", "output", "complaints", "municipal_complaints.csv")
  )