# set-up ------------------------------------------------------------------
lapply(
  c(
    "tidyverse",
    "data.table",
    "Matrix",
    "here",
    "R.utils",
    "sqldf"
  ),
  require, character.only = T
)

source(here("scripts/functions.R"))
set.seed(1789)

# read-in -----------------------------------------------------------------
# contribution matrix (federal)
load(
  here("data/ideology/contrib_matrix_fed_state.RData")
)

# decompose contribution
contrib_matrix <- cm$contrib_matrix
cands <- cm$candidate %>% 
  mutate(
    party = if_else(party == "dem"| party == "pfl", "dem", party)
  )
contributors <- cm$contribution

# remove single entries
MM <- ceiling(contrib_matrix/1e12)
contrib_matrix <- contrib_matrix[rowSums(MM) >= 2,]

# filter candidates present in restricted contrib. matrix
# remove multiple terms for candidates
cands.in <- cands %>% 
  filter(
    cpf_candidate %in% colnames(contrib_matrix)
  ) %>% 
  distinct(
    cpf_candidate,
    .keep_all = T
  )

# cfscore estimation
cfscore_fed_state <- awm(
  cands = cands.in,
  cm = contrib_matrix,
  iters = 8
)

summary(cfscore_fed_state$cands)

# local estimation --------------------------------------------------------
# contribution matrix (local)
load(
  here("data/ideology/contrib_matrix_local.RData")
)

# decompose contribution
contrib_matrix <- cm$contrib_matrix
cands <- cm$candidate %>% 
  mutate(
    party = if_else(party == "dem"| party == "pfl", "dem", party)
  )
contributors <- cm$contribution

# remove single entries
MM <- ceiling(contrib_matrix/1e12)
contrib_matrix <- contrib_matrix[rowSums(MM) >= 2,]

# filter candidates present in restricted contrib. matrix
# remove multiple terms for candidates
cands.in <- cands %>% 
  filter(
    cpf_candidate %in% colnames(contrib_matrix)
  ) %>% 
  distinct(
    cpf_candidate,
    .keep_all = T
  )

# cfscore estimation
cfscore_local <- awm(
  cands = cands.in,
  cm = contrib_matrix,
  iters = 8
)

# contribution matrix (local)
summary(cfscore_local$cands)

# write-out ---------------------------------------------------------------
fed_level_positions <- c("presidente|senador|federal|distrital")

cfscore_fed_state$cands %>% 
  transmute(
    cpf_candidate,
    candidate_name,
    birthyear = election_year - age,
    edu,
    edu_desc,
    gender,
    mun_birth_name,
    occupation,
    occupation_code,
    cfscore
  ) %>% 
  fwrite(
    here("data/ideology/candidate_ideology_fed_state.csv")
  )

cfscore_local$cands %>% 
  transmute(
    cpf_candidate,
    candidate_name,
    birthyear = election_year - age,
    edu,
    edu_desc,
    gender,
    mun_birth_name,
    occupation,
    occupation_code,
    cfscore
  ) %>% 
  fwrite(
    here("data/ideology/candidate_ideology_local.csv")
  )

save(cfscore_fed_state, file = here("data/ideology/cfscore_fed_state.RData"))
save(cfscore_local, file = here("data/ideology/cfscore_local.RData"))