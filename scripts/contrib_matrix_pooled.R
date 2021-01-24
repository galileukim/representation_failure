# set-up ------------------------------------------------------------------
lapply(
    c(
        "tidyverse",
        "data.table",
        "Matrix",
        "R.utils",
        "here",
        "rprojroot",
        "foreach",
        "doParallel"
    ),
    require,
    character.only = T
)

# cfscore -----------------------------------------------------------------
# extract candidates by level (federal/state and local)
# note that we use all candidates (make sure we are not using a subset)

# import all candidates
candidate <- paste0(
    here("data/candidate/"),
    c("cfscore_estimation", "local")
) %>%
    map(
        ~ list.files(
            path = .,
            pattern = "^candidate",
            full.names = T
        )
    ) %>%
    unlist() %>%
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
    here("data/contribution/"),
    pattern = "^campaign.*\\.csv.gz$",
    full.names = T
) %>%
    map_dfr(
        ~ fread(
            .,
            integer64 = "character",
            nThread = parallel::detectCores() - 1
        )
    )

# candidate id's
candidate_id <- candidate %>%
    distinct(cpf_candidate) %>%
    pull(cpf_candidate)

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

# filter out candidates in contributor data
# note that 35% of politicians receive no donations
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
    file =  here("data/ideology/contrib_matrix_pooled.RData")
)
