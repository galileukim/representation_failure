library(tidyverse)
library(data.table)
library(here)

corruption_files <- list.files(
    here("data/corruption"),
    pattern = "^motivo_cassacao",
    full.names = TRUE
)

corruption <- fread(
    corruption_files,
    encoding = "Latin-1",
    integer64 = "character"
)

candidate_2018 <- fread(
        here('data/corruption/candidate_2018.csv'),
        integer64 = "character"
    )

candidate_2014 <- fread(
    here("data/candidate/fed_state/candidate_2014.csv")
)

candidate_2018 <- candidate_2018 %>%
    select(
        election_year = ANO_ELEICAO,
        state = SIGLA_UF,
        candidate_seq = SEQUENCIAL_CANDIDATO,
        cpf_candidate = CPF_CANDIDATO,
        candidate_name = NOME_CANDIDATO
    )

corruption <- corruption %>%
    transmute(
        election_year = ANO_ELEICAO,
        state = SG_UF,
        candidate_seq = SQ_CANDIDATO,
        cause_indictment = DS_MOTIVO_CASSACAO
    )
    
# extract candidates who committed one of:
# 1) in trial (ficha limpa)
# 2) abuso de poder (embezzlement)
# 3) illicit resources
# 4) illicit behavior
# 5) votebuying
corrupt_candidate <- corruption %>%
    filter(
        str_detect(cause_indictment, "Ficha limpa|Abuso|Gasto|Conduta|Compra")
    ) %>%
    inner_join(
        candidate_2018,
        on = "candidate_seq"
    )

corrupt_candidate <- corrupt_candidate %>%
    mutate(
        across(
            where(is.character),
            str_to_lower
        )
    )

corrupt_candidate %>%
    fwrite(
        here("data/corruption/corrupt_candidate_2018.csv")
    )
