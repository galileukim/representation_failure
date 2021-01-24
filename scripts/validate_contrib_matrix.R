# ==============================================================================
# validate contribution matrix for fed-state level
# ==============================================================================
library(tidyverse)
library(Matrix)
library(here)

# ---------------------------------------------------------------------------- #
load(
  here("data/ideology/contrib_matrix_fed_state.RData")
)

candidate <- fread(
    here("data/candidate/cfscore_estimation/candidate_federal_state.csv")
)

contrib_matrix <- cm$contrib_matrix
contribution <- cm$contribution
# restrict to individual donors
# note that only individual donors have 11 digits in their id
n_individual <- nrow(contrib_matrix[str_count(rownames(contrib_matrix)) == 11, ])
n_corporate <- nrow(contrib_matrix[str_count(rownames(contrib_matrix)) != 11, ])

message(
    "there are ",
    n_individual,
    " individual donors and ",
    n_corporate,
    " corporate donors in the federal-state level dataset."
)

# restricting to individual
contributor_digits <- str_count(rownames(contrib_matrix))

contrib_matrix_individual <- contrib_matrix[contributor_digits == 11, ]
contrib_matrix_corporate <- contrib_matrix[contributor_digits != 11, ]

# number of politicians
n_politician_individual <- ncol(
    contrib_matrix_individual[, colSums(contrib_matrix_individual) > 0]
)

n_politician_corporate <- ncol(
    contrib_matrix_corporate[, colSums(contrib_matrix_corporate) > 0]
)

message(
    "there are ",
    n_politician_individual,
    " politicians who receive from individual donors and ",
    n_politician_corporate,
    " politicians who receive from corporate donors in the federal-state."
)

# number of entries
contrib_entries_individual <- ceiling(contrib_matrix_individual/1e15)
contrib_entries_corporate <- ceiling(contrib_matrix_corporate/1e15)

# number of donations per contributor (individual)
rowSums(contrib_entries_individual) %>%
    as_tibble() %>%
    count(value, sort = TRUE)

rowSums(contrib_entries_corporate) %>%
    as_tibble() %>%
    count(value, sort = TRUE)

# number of donations per politician (individual)
colSums(contrib_entries_individual) %>%
    as_tibble() %>%
    count(value, sort = TRUE)

colSums(contrib_entries_corporate) %>%
    as_tibble() %>%
    count(value, sort = TRUE)