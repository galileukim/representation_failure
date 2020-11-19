library(data.table)
library(tidyverse)

candidate <- fread(
    here("data/candidate/cfscore_estimation/candidate_federal_state.csv"),
    colClasses = "character"
)

missing_2010 <- fread(
    here("data/candidate/missing/missing2010.csv"),
    colClasses = "character"
) %>%
    distinct(candidate_name)

missing_2014 <- fread(
    here("data/candidate/missing/missing2014.csv"),
    colClasses = "character"
) %>%
    distinct(candidate_name) %>%
    pull()

missing_names_2010 <- word(missing_2010, 1, 3)

# only retain elected officials
missing_names_2014 <- word(missing_2014, 1, 3)[1:2]

candidate_missing_2010 <- candidate %>%
    filter(
        str_detect(candidate_name, missing_names_2010)
    ) %>%
    select(cpf_candidate, candidate_name)

candidate_missing_2014 <- candidate %>%
    filter(
        election_year == 2014 &
            str_detect(candidate_name, missing_names_2014[1]) |
            str_detect(candidate_name, missing_names_2014[2])
    ) %>%
    filter(
        candidate_name != "joao marcelo santos silva"
    ) %>%
    arrange(desc(candidate_name)) %>%
    mutate( # fix names
        candidate_name = c("joao marcelo santos sousa" , "joaquim passarinho pinto de souza")
    ) %>%
    select(cpf_candidate, candidate_name)

# fix election tables
election_2010 <- fread(here("data/election/election_2010.csv"), colClasses = "character")
election_2014 <- fread(here("data/election/election_2014.csv"), colClasses = "character")

election_2010[candidate_missing_2010, on = c("candidate_name"), cpf_candidate := i.cpf_candidate]
election_2014[candidate_missing_2014, on = c("candidate_name"), cpf_candidate := i.cpf_candidate]

election_2010 %>%
    fwrite(here("data/election/election_2010.csv"))

election_2014 %>%
    fwrite(here("data/election/election_2014.csv"))