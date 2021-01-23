# to-do
# 1) can we come up with reasonable priors for smaller parties
# 2) subset to only federal level candidates
# 3) try pooling local candidates as well
# 4) make sure that the raw data and code are a-ok, verify contrib matrix
# 5) get a sense of the distribution of no. of donors per candidate and vice
# versa
# 6) power and zucco (legislative surveys): can we get more ideology points
# for all parties, or even voter surveys, anything we can use to anchor
# 7) attribute to the smaller parties in coalitions: for the parties
# we don't have information. use the initial coalition they participated in

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

ideology_rowcall <- fread(
  here("data/ideology/legislative_ideology.csv")
)

# setting priors for ideology scores of parties
ideology_party <- ideology_rowcall %>%
  filter(election_year == 2006) %>%
  group_by(party) %>%
  summarise(
    ideology = mean(ideology, na.rm = TRUE),
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
MM <- ceiling(contrib_matrix/1e12)
contrib_matrix <- contrib_matrix[rowSums(MM) >= 2, colSums(MM) >= 2]

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
  iters = 12
)

summary(cfscore_fed_state$cands)
hist(cfscore_fed_state$cands$cfscore, breaks = 100)

cfscore <- cfscore_fed_state$cands %>%
  group_by(party) %>% 
  summarise(
    cfscore = mean(cfscore, na.rm = TRUE),
    .groups = "drop"
  )

cfscore %>%
  inner_join(ideology_party, by = "party") %>%
  ggplot() +
  geom_point(
    aes(cfscore, ideology)
  )

cfscore_fed_state$cands %>%
  ggplot() +
  geom_boxplot(
    aes(as.factor(party), cfscore)
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