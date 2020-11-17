# input: candidate level data for all years
# output: candidate level data for 2006-2014, only including state-level candidates
# plus ideology score
library(data.table)
library(tidyverse)
library(here)

candidate <- fread("/home/gali/gdrive/princeton/R/data/tse/data/wrangle/candidate.csv.gz")

cfscore <- fread(
    here("data/ideology/candidate_ideology_fed_state.csv"),
    integer64 = "character"
) %>%
    select(cpf_candidate, cfscore)

fed_level_positions <- c("presidente|senador|federal|distrital")

candidate_state_level <- candidate %>%
    filter(
        election_year %in% seq(2006, 2014, 4) &
            !str_detect(position, fed_level_positions)
    )

# join with ideology scores
candidate_state_level <- candidate_state_level %>%
    left_join(
        cfscore,
        by = c("cpf_candidate")
    ) %>%
    transmute(
        cpf_candidate,
        election_year,
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

candidate_state_level %>%
    fwrite(
        here("data/ideology/candidate_ideology_only_state.csv")
    )