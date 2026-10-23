# set-up ------------------------------------------------------------------
lapply(
  c(
    "tidyverse",
    "data.table",
    "here",
    "lubridate",
    "stringi",
    "R.utils"
  ),
  require,
  character.only = T
)

# electoral data ----------------------------------------------------------
# candidate data
candidate <- fread(
    here("data", "input", "tse", "candidate.csv.gz"),
    nThread = parallel::detectCores() - 1
  ) %>%
  filter(
    election_year >= 2002 & election_year <= 2014
  )

party_membership <- fread(
  here("data", "input", "tse", "filiado.csv.gz")
)

municipio <- read_csv(
  here("data", "input", "municipal", "municipios.csv"),
  col_select = c(nome_municipio, cod_ibge_6, codigo_uf)
)

state <- read_csv(
  here("data", "input", "municipal", "state_id.csv")
)

# select vars
candidate <- candidate %>%
  select(
    cpf_candidate,
    elec_title,
    state,
    candidate_num,
    candidate_name,
    position,
    election_year,
    coalition,
    birthyear,
    candidate_status,
    edu,
    gender,
    race,
    incumbent,
    elected,
    mun_birth_code,
    mun_birth_name,
    state_birth_name = state_birth_code,
    outcome,
    party,
    starts_with("times"),
    position_previous,
    occupation,
    occupation_code
  ) %>%
  mutate(
    age = election_year - as.integer(birthyear)
  )

# identify candidates in the party membership data
# we recover 679.3 thousand records
# we verify that 84.7% of records match the name exactly
candidate_party_membership <- party_membership %>%
  rename(
    elec_title = electoral_title
  ) %>%
  # only retain party members who appear in our candidate data
  inner_join(
    candidate %>% distinct(elec_title),
    by = c("elec_title")
  ) |>
  # add start year of party membership 
  mutate(
    year_start = year(
      ymd(date_start)
    )
  ) |> 
  # remove measurement error
  filter(
    # the majority of start dates are after 1980 and prior to 2019 which suggests
    # measurement error prior to that year
    year_start >= 1980 &
    year_start <= 2019
  ) |> 
  select(-matches("candidate"))

# by candidate and electoral cycle, identify the latest party membership record
# there are 973.4 thousand distinct electoral titles in the candidate database
# we recover 518.8 thousand from the party membership data, or 53.3%
# note: (1) this dataset only includes candidates whose party membership is available
# (2) there are candidates whose party registration when a candidate
# does not match the party membership data (19.4%). we drop those.
candidate_last_membership <- candidate |> 
  select(
    cpf_candidate,
    elec_title,
    election_year,
    party_candidate = party
  ) |> 
  left_join(
    candidate_party_membership,
    by = "elec_title",
    relationship = "many-to-many"
  ) |> 
  arrange(
    desc(date_start)
  ) |> 
  filter(
    year_start <= election_year &
    party_candidate == party
  )  |> 
  group_by(elec_title, election_year) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  select(-starts_with("party")) |> 
  rename(
    cod_ibge_6_party_membership = mun_name
  )

# coarsen occupation
candidate <- candidate %>%
  mutate(
    occupation = case_when(
      occupation == "empresario" | occupation == "comerciante" ~ "business",
      str_detect(occupation, "servidor publico") ~ "government",
      str_detect(occupation, "vereador|prefeito|deputado|senador|governador|membros do poder|presidente") ~ "politician",
      str_detect(occupation, "economista|engenheiro|^tecnico") ~ "technician",
      str_detect(occupation, "advogado|admin") ~ "white-collar",
      str_detect(occupation, "professor") ~ "teacher",
      T ~ "other"
    )
  )

# coarsen education
candidate <- candidate %>%
  mutate(
    edu = case_when(
      between(edu, 1, 3) ~ "lower school",
      between(edu, 4, 5) ~ "middle school",
      between(edu, 6, 7) ~ "high school",
      edu >= 8 ~ "higher education",
      T ~ NA_character_
    )
  )

# fix race: race is self-identified and only available post-2014
# this has been cross-validated with the original race data
# due to inconsistencies in coding of race of candidates
candidate <- candidate %>%
  mutate(
    race = as.character(race),
    race = case_when(
      race == 3 ~ "white",
      race == 1 ~ "black_or_brown",
      race == 9 ~ "black_or_brown",
      race %in% c(5, 7) ~ "other",
      race == -4 ~ NA,
      T ~ race
    )
  )

# fix birth municipality and ensure that we use cod_ibge_6
# combine municipio data with state data
municipio <- municipio |> 
  left_join(
    state,
    by = c("codigo_uf" = "state_id")
  ) |> 
  # remove special characters and convert string to lower
  transmute(
    cod_ibge_6_birth = cod_ibge_6,
    mun_birth_name = stri_trans_general(
        nome_municipio, "latin-ascii"
      ) |> 
      str_to_lower(),
    state_birth_name = str_to_lower(state)
  )

# join cod_ibge_6_birth: municipality of birth
# we fail to join 81693/1358159 = 6% of our sample
candidate_clean <- candidate |> 
  left_join(
    municipio,
    by = c("mun_birth_name", "state_birth_name")
  )

# write-out all federal candidates
candidate_clean %>%
  filter(election_year %in% seq(2002, 2014, 4)) %>%
  fwrite(
    here(
      "data", "output", "candidate", "cfscore_estimation", "candidate_federal_state.csv"
    )
  )

# write-out candidates
candidate_clean <- candidate %>%
  filter(
    election_year >= 2002
  )

for (i in unique(candidate_clean$election_year)) {
  if (i %in% seq(2002, 2014, 4)) {
    # only deputados
    candidate_clean %>%
      filter(election_year == i, position == "deputado federal") %>%
      fwrite(
        here(
          paste0(
            "data/output/candidate/fed_state/candidate_", i, ".csv"
          )
        )
      )
  } else {
    candidate_clean %>%
      filter(election_year == i) %>%
      fwrite(
        here(
          paste0(
            "data/output/candidate/local/candidate_", i, ".csv"
          )
        )
      )
  }
}

# write-out party membership
# 665.0 thousand records found
candidate_last_membership |>
  fwrite(
    here("data", "output", "candidate", "candidate_party_membership.csv.gz")
  )

# ---------------------------------------------------------------------------- #
# electoral data
election <- list.files(
  here("data", "input", "tse"),
  pattern = "^election.csv.gz$",
  full.names = T
) %>%
  fread(
    nThread = parallel::detectCores() - 1
  ) %>%
  filter(
    election_year %in% seq(2002, 2014, 4),
    position == "deputado federal"
  )

# select vars
election <- election %>%
  transmute(
    cod_ibge_6,
    state,
    party,
    election_year,
    cpf_candidate,
    candidate_name,
    coalition,
    age = election_year - as.integer(birthyear),
    candidate_status,
    state,
    outcome,
    vote
  )

# write-out election
for (i in seq(2002, 2014, 4)) {
  election %>%
    filter(election_year == i) %>%
    fwrite(
      here(
        paste0(
          "data/output/election/election_", i, ".csv"
        )
      )
    )
}

# blank votes
vote <- list.files(
  here("data", "input", "tse"),
  "vote_count",
  full.names = T
) %>%
  fread()

# extract deputado federais
vote <- vote %>%
  filter(
    position == "deputado federal"
  )

# write-out vote
for (i in unique(vote$election_year)) {
  vote %>%
    filter(election_year == i) %>%
    fwrite(
      here(
        paste0(
          "data/output/election/vote_count_", i, ".csv"
        )
      )
    )
}

# vote for legend
coalition <- list.files(
  here("data", "input", "tse"),
  "party.csv.gz",
  full.names = T
) %>%
  fread()

# extract deputados federais
coalition <- coalition %>%
  filter(
    position == "deputado federal"
  )

# drop boa esperanca do norte: non-installed municipality
coalition <- coalition %>%
  filter(
    mun_name != "boa esperanca do norte"
  )

# aggregate by coalition
coalition <- coalition %>%
  group_by(
    election_year,
    cod_ibge_6,
    coalition,
    coalition_type
  ) %>%
  summarise(
    vote_legend = sum(vote_legend)
  ) %>%
  ungroup()

# write out votes for coalition
for (i in unique(coalition$election_year)) {
  coalition %>%
    filter(election_year == i) %>%
    fwrite(
      here(
        paste0(
          "data/output/election/coalition_vote_", i, ".csv"
        )
      )
    )
}
