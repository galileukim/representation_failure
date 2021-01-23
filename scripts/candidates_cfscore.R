# to-do
# 2) subset to only federal level candidates
# 3) try pooling local candidates as well
# 4) make sure that the raw data and code are a-ok, verify contrib matrix
# 5) get a sense of the distribution of no. of donors per candidate and vice
# versa
# 6) power and zucco (legislative surveys): can we get more ideology points
# for all parties, or even voter surveys, anything we can use to anchor

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
theme_set(theme_bw())

# read-in -----------------------------------------------------------------
# contribution matrix (federal)
load(
  here("data/ideology/contrib_matrix_fed_state_new.RData")
)

ideology_survey <- fread(
  here("data/ideology/legislative_survey_ideology_party.csv")
)

# setting priors for ideology scores of parties
# using power and rodrigues silveira survey of brazilian legislators (2018)
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

# one possible solution to the sparsity issue:
# only retain donors that donate to multiple candidates
# potentially use candidates that receive only one donor
MM <- ceiling(contrib_matrix/1e15)
contrib_matrix <- contrib_matrix[rowSums(MM) > 1,]
contrib_matrix <- contrib_matrix[, colSums(contrib_matrix) > 0]

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
  iters = 12,
  party_ideology = party_ideology_survey
)

summary(cfscore_fed_state$cands)
cfscore_fed_state %>%
  pluck("cands") %>%
  ggplot() +
  geom_histogram(
    aes(cfscore),
    fill = "white",
    col = "black"
  ) +
  ggsave(
    here("figs/histogram_fed_state_cfscore.png")
  )

cfscore <- cfscore_fed_state$cands %>%
  group_by(party) %>% 
  summarise(
    cfscore_bonica = mean(cfscore, na.rm = TRUE),
    .groups = "drop"
  )

cfscore %>%
  inner_join(party_ideology_survey, by = "party") %>%
  ggplot() +
  geom_point(
    aes(cfscore_bonica, cfscore)
  ) +
  ggsave(
    here("figs/cfscore_campaign_vs_survey.png")
  )

cfscore_fed_state$cands %>%
  ggplot() +
  geom_boxplot(
    aes(forcats::fct_rev(party), cfscore)
  ) +
  coord_flip() +
  labs(x = "party") +
  ggsave(
    here("figs/cfscore_fed_state_by_party.png")
  )

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

# restrict to individual donors
# note that only individual donors have 11 digits in their id
contrib_matrix <- contrib_matrix[str_count(rownames(contrib_matrix)) == 11, ]

# one possible solution to the sparsity issue:
# only retain donors that donate to multiple candidates
# MM <- ceiling(contrib_matrix/1e12)
# contrib_matrix <- contrib_matrix[rowSums(MM) >= 2,]

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
  iters = 12
)

# contribution matrix (local)
summary(cfscore_local$cands)
hist(cfscore_local$cands$cfscore, breaks = 30)  

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