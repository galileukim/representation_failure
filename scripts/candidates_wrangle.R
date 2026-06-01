# set-up ------------------------------------------------------------------
lapply(
  c(
    "tidyverse",
    "data.table",
    "Matrix",
    "here",
    "rprojroot",
    "lubridate",
    "foreach",
    "stringi",
    "doParallel"
  ),
  require,
  character.only = T
)

# electoral data ----------------------------------------------------------
# candidate data
candidate <- fread(
    here("data", "tse", "candidate.csv.gz"),
    nThread = parallel::detectCores() - 1
  ) %>%
  filter(
    election_year >= 2002 & election_year <= 2014
  )

party_membership <- fread(
  here("data", "tse", "filiado.csv.gz")
)

municipio <- read_csv(
  here("data", "municipal", "municipios.csv"),
  col_select = c(nome_municipio, cod_ibge_6, codigo_uf)
)

state <- read_csv(
  here("data", "municipal", "input", "state_id.csv")
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
      paste0(
        "data/candidate/cfscore_estimation/candidate_federal_state.csv"
      )
    )
  )

# write-out candidates
candidate_clean <- candidate %>%
  filter(
    election_year >= 2004
  )

for (i in unique(candidate_clean$election_year)) {
  if (i %in% seq(2002, 2014, 4)) {
    # only deputados
    candidate_clean %>%
      filter(election_year == i, position == "deputado federal") %>%
      fwrite(
        here(
          paste0(
            "data/candidate/fed_state/candidate_", i, ".csv"
          )
        )
      )
  } else {
    candidate_clean %>%
      filter(election_year == i) %>%
      fwrite(
        here(
          paste0(
            "data/candidate/local/candidate_", i, ".csv"
          )
        )
      )
  }
}

# write-out party membership
# 665.0 thousand records found
candidate_last_membership |>
  fwrite(
    here("data", "candidate", "candidate_party_membership.csv.gz")
  )

# ---------------------------------------------------------------------------- #
# electoral data
election <- list.files(
  "~/princeton/R/data/tse/data/wrangle",
  pattern = "^election.csv.gz$",
  full.names = T
) %>%
  gunzip(
    remove = F,
    overwrite = T,
    temporary = T
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
          "data/election/election_", i, ".csv"
        )
      )
    )
}

# blank votes
vote <- list.files(
  "~/princeton/R/data/tse/data/wrangle/",
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
          "data/election/vote_count_", i, ".csv"
        )
      )
    )
}

# vote for legend
coalition <- list.files(
  "~/princeton/R/data/tse/data/wrangle/",
  "party.csv.gz",
  full.names = T
) %>%
  gunzip(
    remove = F,
    temporary = T,
    overwrite = T
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
    coalition_name,
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
          "data/election/coalition_vote_", i, ".csv"
        )
      )
    )
}

# cfscore -----------------------------------------------------------------
# extract candidates by level (federal/state and local)
# note that we use all candidates (make sure we are not using a subset)
foreach(i = seq(1, 2)) %do% {
  candidate <- list.files(
    paste0(
      here("data/candidate/"),
      c("cfscore_estimation", "local")[i]
    ),
    pattern = "^candidate",
    full.names = T
  ) %>%
    map_dfr(
      . %>%
        fread(
          colClasses = "character",
          integer64 = "character",
          nThread = parallel::detectCores() - 1
        )
    )

  # contribution data
  contributors <- list.files(
    here("data/contribution/"),
    pattern = c("campaign_fed_state", "campaign_local")[i],
    full.names = T
  ) %>%
    fread(
      integer64 = "character",
      nThread = parallel::detectCores() - 1
    )

  # candidate id's
  candidate_id <- candidate %>%
    distinct(cpf_candidate) %>%
    pull(cpf_candidate)

  # remove defective cpf_cnpj_donor entries
  contributors <- contributors %>% 
    filter(
      !(cpf_cnpj_donor %in% c(
        "00000000000000", "99999999999999", "---", "0", "0000000000000"
        ))
    )

  # create projected col (following Bonica 2014)
  # projected if a donation by candidates/committees
  # times donated
  contributors <- contributors %>%
    mutate(
      is_projected = if_else(
        type_resource == "recursos de outros candidatos/comites" | 
        type_resource == "recursos de partido politico" |
        type_resource == "recursos proprios",
        1, 0
      )
    )

  # note that 69% of politicians receive donations
  candidate_in_contributor <- candidate_id[candidate_id %in% unique(contributors$cpf_candidate)]
  
  message(
    "there are ",
    100*round(length(candidate_in_contributor)/length(candidate_id), 3),
    " percent of candidates with contribution data."
  )

  contributors <- contributors %>%
    filter(
      cpf_candidate %in% candidate_id
    )

  candidate <- candidate %>%
    filter(
      cpf_candidate %in% candidate_in_contributor
    )

  # construct total donation by candidate
  # filter out projected and company donations:
  # individual donors have only 11 digit cpf_cnpj_donor ids
  # note that around 58 percent of the donations are individual donations
  contribution <- contributors %>%
    filter(
      is_projected != 1
    ) %>%
    arrange(
      cpf_cnpj_donor,
      cpf_candidate
    ) %>%
    group_by(
      cpf_cnpj_donor,
      cpf_candidate
    ) %>%
    summarise(
      total_contrib = sum(value_receipt)
    ) %>%
    ungroup()

  contributor_ids <- unique(contribution$cpf_cnpj_donor)
  candidate_ids <- unique(contribution$cpf_candidate)
  # create contribution matrix
  contrib_matrix <- sparseMatrix(
    i = contribution$cpf_cnpj_donor %>% factor(contributor_ids) %>% as.numeric(),
    j = contribution$cpf_candidate %>% factor(levels = candidate_ids) %>% as.numeric(),
    x = contribution$total_contrib,
    dimnames = list(
      unique(contribution$cpf_cnpj_donor),
      unique(contribution$cpf_candidate)
    )
  )

  # construct list
  cm <- list(
    candidate = candidate,
    contribution = contribution,
    contrib_matrix = contrib_matrix
  )

  # save contribution matrix
  save(
    cm,
    file = paste0(
      here("data/ideology/contrib_matrix_"), c("fed_state", "local")[i], ".RData"
    )
  )
}

# ideal point -------------------------------------------------------------
# retrieved from Zucco and Lauderdale (2011)
legislative <- list.files(
  here("data/ideology/"),
  pattern = "^legislative_200[3|7].*csv$",
  full.names = T
) %>%
  map(
    fread
  )

# add electoral years
for (i in 1:2) {
  legislative[[i]] <- legislative[[i]] %>%
    mutate(
      election_year = c(2002, 2006)[i]
    )
}

legislative <- rbindlist(legislative)

# fix cols
legislative <- legislative %>%
  transmute(
    candidate_name = namelegis,
    election_year,
    party = party,
    state = state,
    ideology = x1
  ) %>%
  mutate_if(
    is.character,
    str_to_lower
  )

# write out
legislative %>%
  fwrite(
    here("data/ideology/legislative_ideology.csv")
  )