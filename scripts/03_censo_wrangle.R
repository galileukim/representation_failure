# set-up ------------------------------------------------------------------
library(dplyr)
library(readr)
library(here)
library(stringr)
library(data.table)
library(purrr)

# municipal table ---------------------------------------------------------
# read-in
censo <- list.files(
  here("data", "municipal", "input"),
  "censo_mun_[20]\\d+",
  full.names = T
) %>%
  map_dfr(
    function(x) {
      fread(x) %>%
        select(
          cod_ibge_6,
          year,
          pop,
          median_wage,
          employment,
          lower_school,
          middle_school,
          high_school,
          higher_education,
          water,
          female,
          black_or_brown,
          domestic_light = light,
          sanitation,
          garbage,
          lit_rate,
          rural_pop,
          age,
          female
        ) %>%
        mutate_if(
          is.double,
          \(col) {col * 100}
        ) %>%
        mutate(
          median_wage = median_wage / 100,
          age = age / 100
        )
    }
  )

# combine -----------------------------------------------------------------
# join state id's
state_id <- read_csv(
  here("data", "municipal", "input", "state_id.csv")
) %>%
  mutate(
    state_id = as.character(state_id),
    state = str_to_lower(state)
  )

censo <- censo %>%
  mutate(
    state_id = str_sub(cod_ibge_6, 1, 2)
  ) %>%
  left_join(
    state_id,
    by = c("state_id")
  ) %>%
  select(
    -state_id
  )

# budget
finbra <- list.files(
  here("data", "municipal", "input"),
  "^receita_mun.csv$",
  full.names = T
) %>%
  fread(
    colClasses = c("cod_ibge_6" = "character")
  ) %>%
  filter(
    year %in% seq(2002, 2016, 4),
    transf_intergov_da_uniao > 0
  ) %>%
  transmute(
    cod_ibge_6,
    year,
    log_budget = log(rec_orcamentaria),
    log_fed_transfer = log(transf_intergov_da_uniao)
  )

# cast cols
finbra <- as.data.table(finbra) %>%
  dcast(
    cod_ibge_6 ~ year,
    value.var = c("log_budget", "log_fed_transfer")
  )

# join data
municipal <- censo %>%
  left_join(
    finbra %>%
      mutate(
        cod_ibge_6 = as.integer(cod_ibge_6)
      ),
    by = "cod_ibge_6"
  ) %>%
  select(
    order(colnames(.))
  ) %>%
  rename(
    employment_rate = employment
  ) %>% 
  select(
    cod_ibge_6,
    state,
    everything()
  )

# write-out
municipal %>%
  fwrite(
    here("data", "output", "municipal", "municipal_data.csv")
  )