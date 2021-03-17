library(tidyverse)
library(here)

corruption <- read_csv(
    here("data/corruption/corrupt_candidate_2018.csv")
) %>%
    distinct(cpf_candidate, candidate_name) %>%
    transmute(
        cpf_candidate,
        candidate_name,
        corrupt = 1
    )

candidate <- read_csv(
    here("data/candidate/fed_state/candidate_2014.csv")
)

unobserved_valence <- read_csv(
    here("../estimation/unobserved_valence.csv")
) %>%
    rename(
        cpf_candidate = candidate
    )

candidate_corruption <- candidate %>%
    left_join(
        corruption,
        by = c("cpf_candidate")
    ) %>%
    left_join(
        unobserved_valence,
        by = c("cpf_candidate")
    ) %>%
    filter(elected == 1) %>%
    mutate(
        female = if_else(gender == 4, 1, 0),
        # higher_edu = if_else(edu == "higher education", 1, 0),
        edu = ifelse(edu == "middle school", "middle education", edu) %>%
            fct_relevel("lower school"),
        corrupt = ifelse(is.na(corrupt), 0, corrupt),
        occupation = if_else(
            occupation %in% c("government", "business", "technician", "white-collar"),
            occupation, 
            "other"
        ) %>%
            fct_relevel("other")
    )

logit <- glm(
    corrupt ~ log(unobserved_valence) + age + female + occupation + edu + incumbent,
    data = candidate_corruption
)

stargazer::stargazer(
    logit,
    omit = "Constant",
    title = "Logistic Regression: Unobserved and Observed Valence Characteristics",
    dep.var.labels = "Corruption Indictment",
    covariate.labels = c(
        "Unobserved Valence",
        "Age",
        "Female",
        "Business",
        "Government",
        "Technician",
        "White-Collar",
        "Higher Education",
        "Middle School",
        "Incumbency Status"
    ),
    type = 'latex')
