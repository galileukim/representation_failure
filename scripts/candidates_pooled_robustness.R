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
  here("data/ideology/contrib_matrix_pooled.RData")
)

# setting priors for ideology scores of parties
# using power and rodrigues silveira survey of brazilian legislators (2018)
ideology_survey <- fread(
  here("data/ideology/legislative_survey_ideology_party.csv")
)

party_ideology_survey <- ideology_survey %>%
  janitor::clean_names() %>%
  mutate(
    party = str_to_lower(sigla_partido),
    election_year = bls_year,
    ideology = coalesce(ideology, ideo_imputed)
  ) %>%
  filter(
    between(election_year, 2002, 2014) # retain election years
  ) %>%
  group_by(party) %>%
  summarise(
    cfscore = mean(ideology, na.rm = TRUE),
    .groups = "drop"
  )

# decompose contribution
contrib_matrix <- cm$contrib_matrix
cands <- cm$candidate %>% 
  mutate(
    party = if_else(party == "dem"| party == "pfl", "dem", party)
  )
contributors <- cm$contribution

# restrict to individual donors
# note that only individual donors have 11 digits in their id
contrib_matrix <- contrib_matrix[str_count(rownames(contrib_matrix)) == 11, ]
MM <- ceiling(contrib_matrix / 1e15)
contrib_matrix <- contrib_matrix[rowSums(MM) > 1, ]
contrib_matrix <- contrib_matrix[, colSums(contrib_matrix) > 0]

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
cfscore_robust <- map2(
  list(cands.in_5_percentile, cands.in_10_percentile),
  list(contrib_matrix_5_percentile, contrib_matrix_10_percentile),
  ~awm(cands = .x, cm = .y, iters = 8, party_ideology = party_ideology_survey)
)

# write-out ---------------------------------------------------------------
cfscore_robust_cands <- cfscore_robust %>% 
  map(pluck, "cands") %>%
  modify(
    ~transmute(
      .,
      cpf_candidate,
      candidate_name,
      # birthyear = election_year - age,
      edu,
      # edu_desc,
      gender,
      mun_birth_name,
      occupation,
      occupation_code,
      cfscore
      ) 
    )

file_output <- sprintf(
  here("data/ideology/candidate_ideology_pooled_%s_percentile.csv"),
  c("5", "10")
)

pwalk(
    list(cfscore_robust_cands, file_output),
    fwrite
  )

save(cfscore_robust, file = here("data/ideology/cfscore_pooled_robustness.RData"))
