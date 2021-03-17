library(tidyverse)
library(here)
library(data.table)

corruption <- read_csv(
    here("data/corruption/corrupt_candidate_2018.csv")
) %>%
    distinct(cpf_candidate, candidate_name) %>%
    transmute(
        cpf_candidate,
        candidate_name,
        corrupt = 1
    )

candidate <- fread(
    here("data/candidate/fed_state/candidate_2014.csv"),
    integer64 = "character"
)

unobserved_valence <- read_csv(
    here("../estimation/unobserved_valence.csv")
) %>%
    select(
        cpf_candidate = candidate,
        state,
        year,
        unobserved_valence
    )

candidate_corruption <- candidate %>%
    left_join(
        corruption,
        by = c("cpf_candidate")
    ) %>%
    left_join(
        unobserved_valence,
        by = c("cpf_candidate", "state", "election_year" = "year")
    ) %>%
    filter(elected == 1) %>% # we only filter elected because these are the ones who can be indicted
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
    corrupt ~ unobserved_valence + age + female + occupation + edu + incumbent,
    family = "binomial",
    data = candidate_corruption
)

linear <- lm(
    corrupt ~ unobserved_valence + age + female + occupation + edu + incumbent,
    data = candidate_corruption
)

stargazer::stargazer(
    logit,
    omit = "Constant|as.factor",
    title = "Logistic Regression: Observed and Unobserved Valence Characteristics",
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

stargazer::stargazer(
    linear,
    omit = "Constant|as.factor",
    title = "Linear Probability Model: Observed and Unobserved Valence Characteristics",
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
