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

candidate_state_level <- candidate_state_level %>%
  mutate(
    occupation = case_when(
      occupation == "empresario" | occupation == "comerciante" ~ "business",
      str_detect(occupation, "servidor publico") ~ "government",
      str_detect(occupation, "vereador|prefeito|deputado|senador|governador|membros do poder|presidente") ~ "politician",
      str_detect(occupation, "economista|engenheiro|^tecnico") ~ "technician",
      str_detect(occupation, "advogado|admin") ~ "white-collar",
      str_detect(occupation, "professor") ~ "teacher",
      T ~ "other"
    )
  )

# coarsen education
candidate_state_level <- candidate_state_level %>%
  mutate(
    edu = case_when(
      between(edu, 1, 3) ~ "lower school",
      between(edu, 4, 5) ~ "middle school",
      between(edu, 6, 7) ~ "high school",
      edu >= 8 ~ "higher education",
      T ~ NA_character_
    )
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
        birthyear = election_year - age,
        edu,
        edu_desc,
        gender,
        incumbent,
        mun_birth_name,
        occupation,
        occupation_code,
        cfscore
    )

candidate_state_level %>%
    fwrite(
        here("data/ideology/candidate_ideology_only_state.csv")
    )