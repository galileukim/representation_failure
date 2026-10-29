# set-up ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(stringi)
library(R.utils)

source(here("scripts", "funs.R"))

general_years <- seq(2002, 2014, 4)
winner_outcomes <- c("eleito", "media", "eleito por media", "eleito por qp")

# read-in -----------------------------------------------------------------
# candidate data, all years (1998-2016)
candidate_input <- fread(
  here("data", "input", "tse", "candidate.csv.gz"),
  colClasses = list(character = c("cpf_candidate", "elec_title")),
  nThread = parallel::detectCores() - 1,
  na.strings = ""
) |>
  clean_ids() |>
  mutate(candidate_name = clean_name(candidate_name))

# election data by candidate and municipality, all offices
election_all <- fread(
  here("data", "input", "tse", "election.csv.gz"),
  colClasses = list(character = c("cpf_candidate", "elec_title")),
  nThread = parallel::detectCores() - 1
) |>
  filter(between(election_year, 2002, 2014)) |>
  clean_ids() |>
  mutate(candidate_name = clean_name(candidate_name))

party_membership <- fread(
  here("data", "input", "tse", "filiado.csv.gz")
)

# party-level election data (nominal and legend votes)
party_vote <- fread(
  here("data", "input", "tse", "party.csv.gz")
) |>
  fix_ibge()

municipio <- read_csv(
  here("data", "input", "municipal", "municipios.csv"),
  col_select = c(nome_municipio, cod_ibge_6, codigo_uf)
)

state <- read_csv(
  here("data", "input", "municipal", "state_id.csv")
)

# candidate data ----------------------------------------------------------
# recover missing cpfs from records with the same electoral title and name in
# other years. in 2002, this recovers 22/208 federal deputy candidates without
# cpf. we drop title-name pairs that map to more than one cpf
cpf_lookup <- candidate_input |>
  filter(!is.na(cpf_candidate) & !is.na(elec_title)) |>
  distinct(elec_title, candidate_name, cpf_recovered = cpf_candidate) |>
  filter(n() == 1, .by = c(elec_title, candidate_name))

candidate <- candidate_input |>
  filter(between(election_year, 2002, 2014)) |>
  select(
    cpf_candidate,
    elec_title,
    state,
    candidate_num,
    candidate_name,
    position,
    election_year,
    coalition,
    coalition_type,
    coalition_name,
    birthyear,
    candidate_status,
    edu,
    gender,
    race,
    incumbent,
    elected,
    # the tse birth municipality code is -1 or -3 (not informed) in every year,
    # so we match the ibge code by name below
    mun_birth_name,
    state_birth_name = state_birth_code,
    outcome,
    party,
    starts_with("times"),
    position_previous,
    occupation,
    occupation_code,
    # used to match candidacies in the election data
    cod_tse,
    round,
    election_type
  ) |>
  left_join(cpf_lookup, by = c("elec_title", "candidate_name")) |>
  mutate(
    cpf_candidate = coalesce(cpf_candidate, cpf_recovered),
    age = election_year - as.integer(birthyear),
    # coarsen occupation and education
    occupation = case_when(
      occupation == "empresario" | occupation == "comerciante" ~ "business",
      str_detect(occupation, "servidor publico") ~ "government",
      str_detect(occupation, "vereador|prefeito|deputado|senador|governador|membros do poder|presidente") ~ "politician",
      str_detect(occupation, "economista|engenheiro|^tecnico") ~ "technician",
      str_detect(occupation, "advogado|admin") ~ "white-collar",
      str_detect(occupation, "professor") ~ "teacher",
      T ~ "other"
    ),
    edu = case_when(
      between(edu, 1, 3) ~ "lower school",
      between(edu, 4, 5) ~ "middle school",
      between(edu, 6, 7) ~ "high school",
      edu >= 8 ~ "higher education",
      T ~ NA_character_
    ),
    # race is self-identified and only available post-2014. this has been
    # cross-validated with the original race data
    race = as.character(race),
    race = case_when(
      race == 3 ~ "white",
      race == 1 ~ "black_or_brown",
      race == 9 ~ "black_or_brown",
      race %in% c(5, 7) ~ "other",
      race == -4 ~ NA,
      T ~ race
    )
  ) |>
  select(-cpf_recovered)

# coalitions --------------------------------------------------------------
# in 2006 and 2010, the raw tse data lumps all parties that ran alone in a state
# into one coalition label (e.g. 16 parties for deputado federal in sp, 2006).
# since a party joins a single coalition per office and electoral unit (the
# municipality for mayor and city council, the state otherwise), we take labels
# from the party-level election data, which reproduce the seat allocation in the
# chamber (all 513 seats in 2002, 2010 and 2014, and all but one in 2006)
coalition_lookup <- party_vote |>
  filter(round == 1 & coalition != "") |>
  add_unit() |>
  distinct(
    election_year, unit, office = position, party, coalition_fix = coalition
  ) |>
  filter(
    n() == 1,
    .by = c(election_year, unit, office, party)
  )

candidate <- candidate |>
  fix_coalition(coalition_lookup) |>
  select(-coalition_type, -coalition_name)

# federal deputies in the election data
election <- election_all |>
  filter(
    election_year %in% general_years & position == "deputado federal"
  ) |>
  fix_ibge() |>
  fix_coalition(coalition_lookup)

# election results --------------------------------------------------------
# outcomes and candidate status come from the election data. the candidate data
# keeps a pre-election status for some winners whose registration was later
# reinstated (e.g. vinicius gurgel and rosinha da adefal in 2010), and flags as
# elected candidates absent from the election data.
# a candidacy is an election, round, office, electoral unit and ballot number.
# withdrawn candidates share their ballot number with their substitute, so we
# also match the person, by cpf or else by name. candidates absent from the
# election data (e.g. registrations denied or withdrawn before the election,
# running mates and senate substitutes) keep the candidate data's labels
candidacy_key <- c(
  "election_year", "regular_election", "round", "position", "unit", "candidate_num"
)

election_status <- election_all |>
  add_unit() |>
  mutate(regular_election = is_regular_election(election_type)) |>
  distinct(
    across(all_of(candidacy_key)),
    cpf_candidate,
    candidate_name,
    outcome_election = outcome,
    status_election = na_if(candidate_status, "")
  )

candidate <- candidate |>
  add_unit() |>
  mutate(
    row_id = row_number(),
    regular_election = is_regular_election(election_type)
  )

candidate_election <- bind_rows(
  match_election(candidate, election_status, c(candidacy_key, "cpf_candidate")),
  match_election(candidate, election_status, c(candidacy_key, "candidate_name"))
) |>
  distinct(row_id, .keep_all = TRUE) |>
  mutate(in_election = TRUE)

candidate <- candidate |>
  left_join(candidate_election, by = "row_id") |>
  mutate(
    in_election = coalesce(in_election, FALSE),
    outcome = if_else(in_election, outcome_election, outcome),
    candidate_status = if_else(
      in_election, coalesce(status_election, candidate_status), candidate_status
    ),
    elected = if_else(
      in_election,
      as.integer(outcome_election %in% winner_outcomes),
      as.integer(elected)
    ),
    # marcus savius teixeira sousa (2002) withdrew and was replaced by a
    # substitute under the same ballot number, but the candidate data flags him
    # as elected
    is_replaced = election_year == 2002 &
      position == "deputado federal" &
      candidate_name == "marcus savius teixeira sousa",
    outcome = if_else(
      is_replaced, "renuncia/falecimento/cassacao antes da eleicao", outcome
    ),
    elected = if_else(is_replaced, 0L, elected),
    flag_participated = !candidate_status %in% c(
      "indeferido",
      "renuncia",
      "cancelado",
      "indeferido com recurso",
      "nao conhecimento do pedido",
      "inelegivel",
      "falecido",
      "falecimento"
    )
  ) |>
  select(-outcome_election, -status_election, -is_replaced)

# one record per cpf and election year ------------------------------------
# 4.6 thousand cpfs appear in more than one record within an election year:
# (1) different people (the electoral titles differ, and so do the birth years
# or names), e.g. elza alves pereira and maria marina silva (2004). the cpf
# goes to the person it is paired with in the most election years, counting
# both the candidate data and the election data (e.g. zairton cavalcante santos,
# who shares his cpf with a withdrawn 2006 governor candidate), and is set to
# missing for the others, or for all of them in case of a tie
# (2) the same person in both rounds, registered twice, running for two
# offices (e.g. candidates who withdrew from a ticket as running mates to run
# for city council) or in a supplementary election. we keep a single record,
# preferring regular elections, candidacies in the election data, candidates who
# participated, were elected, and the latest round
cpf_title_count <- bind_rows(
  candidate_input |> distinct(cpf_candidate, elec_title, election_year),
  election_all |> distinct(cpf_candidate, elec_title, election_year)
) |>
  filter(!is.na(cpf_candidate) & !is.na(elec_title)) |>
  count(cpf_candidate, elec_title, name = "n_years")

rm(candidate_input, election_all, election_status, candidate_election)

candidate <- candidate |>
  left_join(cpf_title_count, by = c("cpf_candidate", "elec_title")) |>
  mutate(
    n_years = coalesce(n_years, 0L),
    shared_cpf = !is.na(cpf_candidate) &
      n_distinct(elec_title) > 1 &
      (n_distinct(birthyear) > 1 | n_distinct(candidate_name) > 1),
    cpf_owner = n_years == max(n_years) &
      n_distinct(elec_title[n_years == max(n_years)]) == 1,
    .by = c(cpf_candidate, election_year)
  ) |>
  mutate(
    cpf_candidate = if_else(shared_cpf & !cpf_owner, NA, cpf_candidate)
  ) |>
  arrange(
    desc(regular_election),
    desc(in_election),
    desc(flag_participated),
    desc(elected),
    desc(round)
  ) |>
  filter(
    is.na(cpf_candidate) | row_number() == 1,
    .by = c(cpf_candidate, election_year)
  ) |>
  arrange(row_id) |>
  select(
    -row_id, -unit, -regular_election, -in_election, -n_years, -shared_cpf,
    -cpf_owner, -cod_tse, -round, -election_type
  )

# flag cpfs whose records may belong to different people: the names differ
# across election years, and so do the birth years (by more than 2 years) or
# the genders. we keep each record's birth year and gender as the tse recorded
# them for that election
candidate <- candidate |>
  mutate(
    birthyear_gap = suppressWarnings(
      max(as.integer(birthyear), na.rm = TRUE) -
        min(as.integer(birthyear), na.rm = TRUE)
    ),
    flag_ambiguous = !is.na(cpf_candidate) &
      n_distinct(candidate_name) > 1 &
      (birthyear_gap > 2 | n_distinct(gender[gender %in% c(2, 4)]) > 1),
    .by = cpf_candidate
  ) |>
  select(-birthyear_gap)

# fix incumbency: incumbents were elected to the same office in the previous
# cycle (8 years earlier for the senate). we keep the candidate data's labels
# when the previous cycle is unavailable (2002, 2004, and the senate in 2006),
# and for candidates without cpf
candidate_winner <- candidate |>
  filter(elected == 1 & !is.na(cpf_candidate)) |>
  distinct(
    cpf_candidate,
    position,
    election_year = election_year + if_else(position == "senador", 8L, 4L)
  ) |>
  mutate(elected_previous = TRUE)

candidate <- candidate |>
  left_join(
    candidate_winner,
    by = c("cpf_candidate", "position", "election_year")
  ) |>
  mutate(
    previous_cycle = election_year - if_else(position == "senador", 8L, 4L),
    incumbent = if_else(
      previous_cycle %in% election_year & !is.na(cpf_candidate),
      as.integer(coalesce(elected_previous, FALSE)),
      as.integer(incumbent)
    )
  ) |>
  select(-elected_previous, -previous_cycle)

# election data ------------------------------------------------------------
# cpfs set to missing in the candidate data are also missing here, and within
# an election year each cpf takes the candidate data's age
# fill missing cpfs in the election data from the candidate data, by ballot
# number among the candidates who ran (a withdrawn candidate shares the ballot
# number with their substitute), e.g. joao marcelo santos sousa, 2014, whose
# name is spelled differently in the two sources
candidate_cpf <- candidate |>
  filter(
    position == "deputado federal" & flag_participated & !is.na(cpf_candidate)
  ) |>
  distinct(election_year, state, candidate_num, cpf_fill = cpf_candidate) |>
  filter(n() == 1, .by = c(election_year, state, candidate_num))

election <- election |>
  left_join(candidate_cpf, by = c("election_year", "state", "candidate_num")) |>
  mutate(cpf_candidate = coalesce(cpf_candidate, cpf_fill)) |>
  select(-cpf_fill)

candidate_cpf_year <- candidate |>
  filter(!is.na(cpf_candidate)) |>
  distinct(cpf_candidate, election_year, age_candidate = age) |>
  mutate(in_candidate = TRUE)

election <- election |>
  left_join(candidate_cpf_year, by = c("cpf_candidate", "election_year")) |>
  transmute(
    cod_ibge_6,
    state,
    party,
    election_year,
    cpf_candidate = if_else(coalesce(in_candidate, FALSE), cpf_candidate, NA),
    candidate_name,
    coalition,
    age = coalesce(age_candidate, election_year - as.integer(birthyear)),
    candidate_status,
    outcome,
    vote
  )

# party membership --------------------------------------------------------
# for each candidate and election, the latest membership record in the
# candidate's party that started before the election. we treat start dates
# before 1980 or after 2019 as measurement error. 19.4% of candidates' parties
# do not match the membership data, and we drop those
candidate_last_membership <- candidate |>
  select(cpf_candidate, elec_title, election_year, party_candidate = party) |>
  inner_join(
    party_membership |>
      rename(elec_title = electoral_title) |>
      select(-matches("candidate")),
    by = "elec_title",
    relationship = "many-to-many"
  ) |>
  mutate(year_start = year(ymd(date_start))) |>
  filter(
    between(year_start, 1980, 2019) &
      year_start <= election_year &
      party_candidate == party
  ) |>
  arrange(desc(date_start)) |>
  slice_head(n = 1, by = c(elec_title, election_year)) |>
  select(-starts_with("party")) |>
  rename(cod_ibge_6_party_membership = cod_ibge_6)

# write-out candidates ------------------------------------------------------
# ibge code of the birth municipality, matched by name and state. we fail to
# match 6% of candidates
municipio <- municipio |>
  left_join(state, by = c("codigo_uf" = "state_id")) |>
  transmute(
    cod_ibge_6_birth = cod_ibge_6,
    mun_birth_name = str_to_lower(stri_trans_general(nome_municipio, "latin-ascii")),
    state_birth_name = str_to_lower(state)
  )

# all candidates in general elections
candidate |>
  filter(election_year %in% general_years) |>
  left_join(municipio, by = c("mun_birth_name", "state_birth_name")) |>
  fwrite(
    here("data", "output", "candidate", "cfscore_estimation", "candidate_federal_state.csv")
  )

# federal deputies in general elections, and all candidates in municipal
# elections
candidate |>
  filter(election_year %in% general_years & position == "deputado federal") |>
  write_by_year(here("data", "output", "candidate", "fed_state", "candidate_%s.csv"))

candidate |>
  filter(!election_year %in% general_years) |>
  write_by_year(here("data", "output", "candidate", "local", "candidate_%s.csv"))

candidate_last_membership |>
  fwrite(
    here("data", "output", "candidate", "candidate_party_membership.csv.gz")
  )

# write-out elections -------------------------------------------------------
election |>
  write_by_year(here("data", "output", "election", "election_%s.csv"))

# vote counts (turnout, blank and null votes) for federal deputies
list.files(
  here("data", "input", "tse"),
  "vote_count",
  full.names = T
) |>
  fread() |>
  filter(position == "deputado federal") |>
  fix_ibge() |>
  write_by_year(here("data", "output", "election", "vote_count_%s.csv"))

# legend votes for federal deputies, by coalition
party_vote |>
  filter(position == "deputado federal") |>
  group_by(election_year, cod_ibge_6, coalition, coalition_type) |>
  summarise(vote_legend = sum(vote_legend), .groups = "drop") |>
  write_by_year(here("data", "output", "election", "coalition_vote_%s.csv"))
