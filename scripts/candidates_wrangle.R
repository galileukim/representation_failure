# set-up ------------------------------------------------------------------
lapply(
  c(
    "tidyverse",
    "data.table",
    "Matrix",
    "R.utils",
    "here",
    "rprojroot",
    "foreach",
    "doParallel"
  ),
  require,
  character.only = T
)

# electoral data ----------------------------------------------------------
# candidate data
candidate <- list.files(
  "~/gdrive/princeton/R/data/tse/data/wrangle",
  pattern = "^candidate.csv.gz$",
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
    election_year > 2002 & election_year <= 2014
  )

candidate_deprecated <- list.files(
  here("data/candidate"),
  ".*feb4.csv$",
  recursive = T,
  full.names = T
) %>%
  map(
    . %>% fread(integer64 = "character")
  ) %>%
  rbindlist(fill = T)

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
    incumbent,
    elected,
    mun_birth_code,
    mun_birth_name,
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


# write-out all federal candidates
candidate %>%
  filter(election_year %in% seq(2002, 2014, 4)) %>%
  fwrite(
    here(
      paste0(
        "data/candidate/cfscore_estimation/candidate_federal_state.csv"
      )
    )
  )

# write-out candidates
for (i in unique(candidate$election_year)) {
  if (i %in% seq(2002, 2014, 4)) {
    # only deputados
    candidate %>%
      filter(election_year == i, position == "deputado federal") %>%
      fwrite(
        here(
          paste0(
            "data/candidate/fed_state/candidate_", i, ".csv"
          )
        )
      )
  } else {
    candidate %>%
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

# municipal table ---------------------------------------------------------
censo <- list.files(
  "~/princeton/R/data/censo_br/data/wrangle/ibge/",
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
          employed = worked,
          lower_school,
          middle_school,
          high_school,
          higher_education,
          water,
          domestic_light = light,
          sanitation,
          garbage,
          lit_rate,
          rural,
          age,
          female
        ) %>%
        mutate_if(
          is.double,
          funs(. * 100)
        ) %>%
        mutate(
          median_wage = median_wage / 100,
          age = age / 100
        )
    }
  )

# join state id's
state_id <- election %>%
  mutate(
    state_id = str_sub(cod_ibge_6, 1, 2)
  ) %>%
  filter(
    !is.na(state_id)
  ) %>%
  distinct(
    state_id, state
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
  "~/princeton/R/data/finbra/data/wrangle",
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
  select(
    cod_ibge_6,
    state,
    everything()
  )

# write-out
municipal %>%
  fwrite(
    here("data/municipal/municipal_data.csv")
  )

# state table -------------------------------------------------------------
state <- list.files(
  "~/princeton/R/data/censo_br/data/wrangle/",
  "^censo_state",
  recursive = T,
  full.names = T
) %>%
  map_dfr(
    . %>%
      fread()
  )

state %>%
  fwrite(
    here("data/state/census_state.csv")
  )

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

  # create projected col (following Bonica 2014)
  # projected if a donation by candidates/committees
  # times donated
  contributors <- contributors %>%
    mutate(
      is_projected = if_else(
        type_resource == "recursos de outros candidatos/comites" | type_resource == "recursos de partido politico",
        1, 0
      )
    )

  # note that 69% of politicians receive donations
  candidate_in_contributor <- candidate_id[candidate_id %in% unique(contributors$cpf_candidate)]
  print(
    length(candidate_in_contributor)/length(candidate_id)
  )

  # construct total donation by candidate
  # filter out projected and company donations:
  # individual donors have only 11 digit cpf_cnpj_donor ids
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

  # create categories of donations
  # contribution <- contribution %>%
  #   mutate(
  #     total_contrib = case_when(
  #       between(total_contrib, 0, 1000) ~ 1,
  #       between(total_contrib, 1001, 2000) ~ 2,
  #       between(total_contrib, 2001, 3000) ~ 3,
  #       between(total_contrib, 3001, 4000) ~ 4,
  #       between(total_contrib, 4001, 5000) ~ 5,
  #       between(total_contrib, 5001, 6000) ~ 6,
  #       between(total_contrib, 6001, 7000) ~ 7,
  #       between(total_contrib, 7001, 8000) ~ 8,
  #       between(total_contrib, 8001, 9000) ~ 9,
  #       T ~ 10
  #     )
  #   )

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