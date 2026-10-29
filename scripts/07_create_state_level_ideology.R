# input: cleaned candidate level data for all years (01a) and pooled cfscores
# output: candidate level data for 2006-2014, only including state-level candidates
# plus ideology score
library(data.table)
library(tidyverse)
library(here)

# the cleaned data already has corrected cpfs, outcomes and incumbency,
# coarsened occupation and education, and age computed from the birth year
candidate <- fread(
    here("data/output/candidate/cfscore_estimation/candidate_federal_state.csv"),
    colClasses = list(character = c("cpf_candidate", "elec_title"))
)

cfscore <- fread(
    here("data/output/ideology/candidate_ideology_pooled_fed_state.csv"),
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
        state,
        cpf_candidate,
        election_year,
        candidate_name,
        party,
        age,
        edu,
        gender,
        incumbent,
        mun_birth_name,
        occupation,
        occupation_code,
        cfscore
    )

candidate_state_level %>%
    fwrite(
        here("data/output/ideology/candidate_ideology_only_state.csv")
    )
