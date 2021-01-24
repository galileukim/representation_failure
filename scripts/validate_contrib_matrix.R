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
    " corporate donors in the federal-state level dataset"
)

# restricting to individual
contributor_digits <- str_count(rownames(contrib_matrix))

contrib_matrix_individual <- contrib_matrix[contributor_digits == 11, ]
contrib_matrix_corporate <- contrib_matrix[contributor_digits != 11, ]

# one possible solution to the sparsity issue:
# only retain donors that donate to multiple candidates
# potentially use candidates that receive only one donor
contrib_entries_individual <- ceiling(contrib_matrix_individual/1e15)
contrib_entries_corporate <- ceiling(contrib_matrix_corporate/1e15)

# number of donations per contributor (individual)
rowSums(contrib_entries_individual) %>%
    as_tibble() %>%
    ggplot() +
    geom_histogram(aes(value), binwidth = 1) +
    coord_cartesian(xlim = c(0, 10)) 
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