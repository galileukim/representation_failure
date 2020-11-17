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

# filter out top 5 percent contributors
top_5_percentile <- rowSums(contrib_matrix) %>%
  quantile(0.95)

top_10_percentile <- rowSums(contrib_matrix) %>%
  quantile(0.9)


contrib_matrix_5_percentile <- contrib_matrix[rowSums(contrib_matrix) <= top_5_percentile,]
contrib_matrix_10_percentile <- contrib_matrix[rowSums(contrib_matrix) <= top_10_percentile,]

# filter candidates present in restricted contrib. matrix
# remove multiple terms for candidates
cands.in_5_percentile <- cands %>% 
  filter(
    cpf_candidate %in% colnames(contrib_matrix_5_percentile)
  ) %>% 
  distinct(
    cpf_candidate,
    .keep_all = T
  )

cands.in_10_percentile <- cands %>% 
  filter(
    cpf_candidate %in% colnames(contrib_matrix_10_percentile)
  ) %>% 
  distinct(
    cpf_candidate,
    .keep_all = T
  )

# cfscore estimation
cfscore_fed_state <- map2(
  list(cands.in_5_percentile, cands.in_10_percentile),
  list(contrib_matrix_5_percentile, contrib_matrix_10_percentile),
  ~awm(cands = .x, cm = .y, iters = 8)
)

# write-out ---------------------------------------------------------------
cfscore_fed_state_cands <- cfscore_fed_state %>% 
  map(pluck, "cands") %>%
  modify(
    ~transmute(
      .,
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
      ) 
    )

file_output <- sprintf(
  here("data/ideology/candidate_ideology_fed_state_%s_percentile.csv"),
  c("5", "10")
)

pwalk(
    list(cfscore_fed_state_cands, file_output),
    fwrite
  )

save(cfscore_fed_state, file = here("data/ideology/cfscore_fed_state_robustness.RData"))