# set-up ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(Matrix)
library(here)
library(foreach)
library(R.utils)

# cfscore -----------------------------------------------------------------
# extract candidates by level (federal/state and local)
# note that we use all candidates (make sure we are not using a subset)
foreach(i = seq(1, 2)) %do% {
  candidate <- list.files(
    paste0(
      here("data", "output", "candidate"),
      "/", c("cfscore_estimation", "local")[i]
    ),
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

  # contribution data
  contributors <- list.files(
    here("data/input/contribution/"),
    pattern = c("campaign_fed_state", "campaign_local")[i],
    full.names = T
  ) %>%
    fread(
      integer64 = "character",
      nThread = parallel::detectCores() - 1
    )

  # candidate id's
  candidate_id <- candidate %>%
    distinct(cpf_candidate) %>%
    pull(cpf_candidate)

  # remove defective cpf_cnpj_donor entries
  contributors <- contributors %>% 
    filter(
      !(cpf_cnpj_donor %in% c(
        "00000000000000", "99999999999999", "---", "0", "0000000000000"
        ))
    )

  # create projected col (following Bonica 2014)
  # projected if a donation by candidates/committees
  # times donated
  contributors <- contributors %>%
    mutate(
      is_projected = if_else(
        type_resource == "recursos de outros candidatos/comites" | 
        type_resource == "recursos de partido politico" |
        type_resource == "recursos proprios",
        1, 0
      )
    )

  # note that 69% of politicians receive donations
  candidate_in_contributor <- candidate_id[candidate_id %in% unique(contributors$cpf_candidate)]
  
  message(
    "there are ",
    100*round(length(candidate_in_contributor)/length(candidate_id), 3),
    " percent of candidates with contribution data."
  )

  contributors <- contributors %>%
    filter(
      cpf_candidate %in% candidate_id
    )

  candidate <- candidate %>%
    filter(
      cpf_candidate %in% candidate_in_contributor
    )

  # construct total donation by candidate
  # filter out projected and company donations:
  # individual donors have only 11 digit cpf_cnpj_donor ids
  # note that around 58 percent of the donations are individual donations
  contribution <- contributors %>%
    filter(
      is_projected != 1
    ) %>%
    arrange(
      cpf_cnpj_donor,
      cpf_candidate
    ) %>%
    group_by(
      cpf_cnpj_donor,
      cpf_candidate
    ) %>%
    summarise(
      total_contrib = sum(value_receipt)
    ) %>%
    ungroup()

  contributor_ids <- unique(contribution$cpf_cnpj_donor)
  candidate_ids <- unique(contribution$cpf_candidate)
  # create contribution matrix
  contrib_matrix <- sparseMatrix(
    i = contribution$cpf_cnpj_donor %>% factor(contributor_ids) %>% as.numeric(),
    j = contribution$cpf_candidate %>% factor(levels = candidate_ids) %>% as.numeric(),
    x = contribution$total_contrib,
    dimnames = list(
      unique(contribution$cpf_cnpj_donor),
      unique(contribution$cpf_candidate)
    )
  )

  # construct list
  cm <- list(
    candidate = candidate,
    contribution = contribution,
    contrib_matrix = contrib_matrix
  )

  # save contribution matrix
  save(
    cm,
    file = paste0(
      here("data/output/ideology/contrib_matrix_"), c("fed_state", "local")[i], ".RData"
    )
  )

  # save contribution by candidate and election year, from all non-projected
  # donors (companies included), unlike the cfscore donor counts
  contributors %>%
    filter(
      is_projected != 1
    ) |>
    summarise(
      contrib_all_donors = sum(value_receipt),
      n_all_donors = n_distinct(cpf_cnpj_donor),
      .by = c("cpf_candidate", "election_year")
    ) |>
    fwrite(
      file = paste0(
        here("data/output/contribution/contribution_by_candidate_"), c("fed_state", "local")[i], ".csv"
      )
    )
}

# ideal point -------------------------------------------------------------
# retrieved from Zucco and Lauderdale (2011)
legislative <- list.files(
  here("data/input/ideology/"),
  pattern = "^legislative_200[3|7].*csv$",
  full.names = T
) %>%
  map(
    fread
  )

# add electoral years
for (i in 1:2) {
  legislative[[i]] <- legislative[[i]] %>%
    mutate(
      election_year = c(2002, 2006)[i]
    )
}

legislative <- rbindlist(legislative)

# fix cols
legislative <- legislative %>%
  transmute(
    candidate_name = namelegis,
    election_year,
    party = party,
    state = state,
    ideology = x1
  ) %>%
  mutate_if(
    is.character,
    str_to_lower
  )

# write out
legislative %>%
  fwrite(
    here("data/output/ideology/legislative_ideology.csv")
  )
