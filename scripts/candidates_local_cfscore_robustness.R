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

top_5_percentile <- rowSums(contrib_matrix) %>%
  quantile(0.95)


# restrict to individual donors
# note that only individual donors have 11 digits in their id
contrib_matrix <- contrib_matrix[str_count(rownames(contrib_matrix)) == 11, ]
contrib_matrix <- contrib_matrix[rowSums(contrib_matrix) <= top_5_percentile,]

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

# ---------------------------------------------------------------------------- #
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
    here("data/ideology/candidate_ideology_local_robustness.csv")
  )

save(cfscore_local, file = here("data/ideology/cfscore_local_robustness.RData"))