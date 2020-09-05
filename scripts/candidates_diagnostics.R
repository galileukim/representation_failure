# set-up ------------------------------------------------------------------
library(tidyverse)
library(data.table)

# read-in -----------------------------------------------------------------
# electoral data
election <- list.files(
  "~/princeton/R/data/tse/data/wrangle",
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

# blank votes
vote <- list.files(
  "~/princeton/R/data/tse/data/wrangle/",
  "vote_count",
  full.names = T
) %>% 
  fread

# extract deputado federais
vote <- vote %>% 
  filter(
    position == "deputado federal"
  )

# validation --------------------------------------------------------------
# vote count election
election_mun <- election %>% 
  group_by(
    state,
    cod_ibge_6,
    election_year
  ) %>% 
  summarise(
    vote_nominal = sum(vote)
  ) %>% 
  ungroup()

election_mun <- election_mun %>% 
  left_join(
    vote %>% 
      select(
        cod_ibge_6,
        state_2 = state,
        election_year,
        starts_with("vote")
      ) %>% 
      rename(
        vote_nominal_2 = vote_nominal
      ), 
    by = c("cod_ibge_6", "election_year")
  )

vote_state <- vote %>% 
  group_by(
    state,
    election_year
  ) %>% 
  summarise(
    vote_nominal = sum(vote_nominal),
    vote_turnout = sum(vote_turnout)
  ) %>% 
  ungroup()
