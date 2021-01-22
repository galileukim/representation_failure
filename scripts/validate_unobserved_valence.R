library(tidyverse)
library(here)

corruption <- read_csv(
    here("data/corruption/corrupt_candidate_2018.csv")
) %>%
    transmute(
        cpf_candidate,
        candidate_name,
        corrupt = 1
    )

candidate <- read_csv(
    here("data/candidate/fed_state/candidate_2014.csv")
)

candidate_corruption <- candidate %>%
    left_join(
        corruption,
        by = c("cpf_candidate")
    ) %>%
    filter(elected == 1) %>%
    mutate(
        female = if_else(gender == 4, 1, 0),
        # higher_edu = if_else(edu == "higher education", 1, 0),
        edu = fct_relevel(edu, "lower school"),
        corrupt = ifelse(is.na(corrupt), 0, corrupt),
        occupation = if_else(
            occupation %in% c("government", "business", "technician", "white-collar"),
            occupation, 
            "other"
        ) %>%
            fct_relevel("other")
    )

logit <- glm(
    corrupt ~ age + female + occupation + edu + incumbent,
    data = candidate_corruption
)

stargazer::stargazer(
    logit,
    title = "Logistic Regression: Unobserved and Observed Valence Characteristics",
    dep.var.labels = "Corruption Indictment",
    covariate.labels = c(
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
