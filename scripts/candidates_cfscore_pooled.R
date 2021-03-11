
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
  require,
  character.only = T
)

source(here("scripts/functions.R"))
set.seed(1789)
theme_set(theme_bw())

# read-in -----------------------------------------------------------------
# contribution matrix (federal)
load(
  here("data/ideology/contrib_matrix_pooled.RData")
)

ideology_survey <- fread(
  here("data/ideology/legislative_survey_ideology_party.csv")
)

candidate_fed_state <- fread(
  here("data/candidate/cfscore_estimation/candidate_federal_state.csv"),
  integer64 = "character"
)

candidate_local <- list.files(
  here("data/candidate/local"),
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
    party = if_else(party == "dem" | party == "pfl", "dem", party)
  )
contributors <- cm$contribution

# restrict to individual donors
# note that only individual donors have 11 digits in their id
contrib_matrix <- contrib_matrix[str_count(rownames(contrib_matrix)) == 11, ]

# one possible solution to the sparsity issue:
# only retain donors that donate to multiple candidates
# potentially use candidates that receive only one donor
# note that 494 thousand politicians receive personal donations
# but only 280 thousand politicians receive personal donations
# from donors who donate to multiple candidates
MM <- ceiling(contrib_matrix / 1e15)
contrib_matrix <- contrib_matrix[rowSums(MM) > 1, ]
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
cfscore_pooled <- awm(
  cands = cands.in,
  cm = contrib_matrix,
  iters = 12,
  party_ideology = party_ideology_survey
)

summary(cfscore_pooled$cands)
cfscore_pooled %>%
  pluck("cands") %>%
  ggplot() +
  geom_histogram(
    aes(cfscore),
    fill = "white",
    col = "black"
  ) +
  ggsave(
    here("figs/histogram_pooled_cfscore.png")
  )

cfscore <- cfscore_pooled$cands %>%
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
    here("figs/cfscore_pooled_campaign_vs_survey.png")
  )

cfscore_pooled$cands %>%
  filter(
    position == "deputado federal"
  ) %>%
  ggplot() +
  geom_boxplot(
    aes(forcats::fct_rev(party), cfscore)
  ) +
  coord_flip() +
  labs(x = "party") +
  ggsave(
    here("figs/cfscore_pooled_by_party.png")
  )

# ---------------------------------------------------------------------------- #
message("generate local and federal candidate tables")

candidate_fed_state_cpf <- candidate_fed_state %>%
  distinct(cpf_candidate)

candidate_fed_state_cfscore <- candidate_fed_state_cpf %>%
  inner_join(
    cfscore_pooled$cands %>%
      select(cpf_candidate, cfscore),
    by = c("cpf_candidate")
  )

candidate_local_cpf <- candidate_local %>%
  distinct(cpf_candidate)

candidate_local_cfscore <- candidate_local_cpf %>%
  inner_join(
    cfscore_pooled$cands %>%
      select(cpf_candidate, cfscore),
    by = c("cpf_candidate")
  )

list(
  x = list(candidate_fed_state_cfscore, candidate_local_cfscore),
  file = sprintf(
    here("data/ideology/candidate_ideology_pooled_%s.csv"), c("fed_state", "local")
  )
) %>%
  pwalk(fwrite)

# export candidates for only federal and state level and local level
# p[roduce a detailed documentation of the construction of contrib_matrix