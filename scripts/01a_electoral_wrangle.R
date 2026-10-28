# set-up ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(stringi)
library(R.utils)

source(
  "scripts/funs.R"
)

# electoral data ----------------------------------------------------------
# candidate data
candidate_input <- fread(
  here("data", "input", "tse", "candidate.csv.gz"),
  colClasses = list(character = c("cpf_candidate", "elec_title")),
  nThread = parallel::detectCores() - 1,
  na.strings = ""
)

# lookup table for cpf by electoral title and name across all years (1998-2016)
# used to recover cpfs missing from the raw tse data (208 federal deputy
# candidates in 2002). we require an exact name match because electoral titles
# are not unique, and drop title-name pairs that map to more than one cpf
cpf_lookup <- candidate_input |>
  filter(
    nchar(cpf_candidate) == 11 &
    !(elec_title %in% c("", "#ni#", "-1"))
  ) |>
  distinct(elec_title, candidate_name, cpf_recovered = cpf_candidate) |>
  filter(n() == 1, .by = c(elec_title, candidate_name))

candidate <- candidate_input %>%
  filter(
    election_year >= 2002 & election_year <= 2014
  )

party_membership <- fread(
  here("data", "input", "tse", "filiado.csv.gz")
)

# party-level vote tallies (nominal and legend votes)
party_vote <- fread(
  here("data", "input", "tse", "party.csv.gz")
)

municipio <- read_csv(
  here("data", "input", "municipal", "municipios.csv"),
  col_select = c(nome_municipio, cod_ibge_6, codigo_uf)
)

state <- read_csv(
  here("data", "input", "municipal", "state_id.csv")
)

# select vars
candidate <- candidate %>%
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
    mun_birth_code,
    mun_birth_name,
    state_birth_name = state_birth_code,
    outcome,
    party,
    starts_with("times"),
    position_previous,
    occupation,
    occupation_code,
    # used to match candidacies in the vote tallies
    cod_tse,
    round,
    election_type
  ) %>%
  mutate(
    age = election_year - as.integer(birthyear)
  )

# fix coalition labels
# in 2006 and 2010, the raw tse data lumps all parties that ran alone in a state
# into one coalition label (e.g. 16 parties for deputado federal in sp, 2006)
# since a party joins a single coalition per office and state, we take labels
# from the party-level tallies, which reproduce the seat allocation in the chamber
# (all 513 seats in 2002, 2010 and 2014, and all but one in 2006).
# running mates and senate substitutes take the label of the ticket's office
coalition_lookup <- party_vote |>
  filter(round == 1 & coalition != "") |>
  distinct(election_year, state, office = position, party, coalition_fix = coalition) |>
  filter(n() == 1, .by = c(election_year, state, office, party))

candidate <- fix_coalition(candidate) |>
  select(-coalition_type, -coalition_name)

# recover missing cpfs from other records with the same electoral title
# in 2002, this recovers 22/208 federal deputy candidates without cpf,
# including 8/11 candidates who received votes
# we treat malformed cpfs as missing (e.g. 137 truncated cpfs in 2004)
candidate <- candidate |>
  mutate(
    cpf_candidate = if_else(
      nchar(cpf_candidate) == 11 & cpf_candidate != strrep("0", 11),
      cpf_candidate,
      NA
    )
  ) |>
  left_join(
    cpf_lookup,
    by = c("elec_title", "candidate_name")
  ) |>
  mutate(
    cpf_candidate = if_else(
      is.na(cpf_candidate), cpf_recovered, cpf_candidate
    )
  ) |>
  select(-cpf_recovered)

# fix names with stray quotes in the raw data
# e.g. """"carlos fernando d""""aguiar silva palacio"""" (2010)
candidate <- candidate |>
  mutate(
    candidate_name = candidate_name |>
      str_remove_all('^"+|"+$') |>
      str_replace_all('"+', "'")
  )

# coarsen occupation
candidate <- candidate %>%
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
candidate <- candidate %>%
  mutate(
    edu = case_when(
      between(edu, 1, 3) ~ "lower school",
      between(edu, 4, 5) ~ "middle school",
      between(edu, 6, 7) ~ "high school",
      edu >= 8 ~ "higher education",
      T ~ NA_character_
    )
  )

# fix race: race is self-identified and only available post-2014
# this has been cross-validated with the original race data
# due to inconsistencies in coding of race of candidates
candidate <- candidate %>%
  mutate(
    race = as.character(race),
    race = case_when(
      race == 3 ~ "white",
      race == 1 ~ "black_or_brown",
      race == 9 ~ "black_or_brown",
      race %in% c(5, 7) ~ "other",
      race == -4 ~ NA,
      T ~ race
    )
  )

# fix birth municipality and ensure that we use cod_ibge_6
# combine municipio data with state data
municipio <- municipio |> 
  left_join(
    state,
    by = c("codigo_uf" = "state_id")
  ) |> 
  # remove special characters and convert string to lower
  transmute(
    cod_ibge_6_birth = cod_ibge_6,
    mun_birth_name = stri_trans_general(
        nome_municipio, "latin-ascii"
      ) |> 
      str_to_lower(),
    state_birth_name = str_to_lower(state)
  )

# electoral data
# we keep all offices to bring outcomes and candidate status into the
# candidate data, and federal deputies for the remaining steps
election_all <- list.files(
  here("data", "input", "tse"),
  pattern = "^election.csv.gz$",
  full.names = T
) %>%
  fread(
    colClasses = list(character = c("cpf_candidate", "elec_title")),
    nThread = parallel::detectCores() - 1
  ) %>%
  filter(
    election_year >= 2002 & election_year <= 2014
  )

election <- election_all %>%
  filter(
    election_year %in% seq(2002, 2014, 4),
    position == "deputado federal"
  )

# fix boa esperanca do norte without cod_ibge_6
election <- election %>%
  mutate(
    cod_ibge_6 = if_else(
      mun_name == "boa esperanca do norte" & state == "mt",
      510183,
      cod_ibge_6
    )
  )

# the vote tallies carry the same lumped coalition labels
election <- fix_coalition(election)

# fill missing cpfs from the candidate data by ballot number
# withdrawn candidates share their ballot number with their substitute,
# so we also require the first name to match
# this recovers the 2010 and 2014 cases that names alone fail to match
# (e.g. joao marcelo santos sousa/souza) and the 2002 cpfs recovered above.
# it supersedes the hand-coded name fixes for the cpfs missing from the 2010
# (carlos fernando d'aguiar silva palacio) and 2014 (joao marcelo santos sousa,
# joaquim passarinho pinto de souza) election data
candidate_cpf <- candidate |>
  filter(
    position == "deputado federal" &
    nchar(cpf_candidate) == 11
  ) |>
  distinct(
    election_year,
    state,
    candidate_num,
    first_name = word(candidate_name, 1),
    cpf_fill = cpf_candidate
  ) |>
  filter(n() == 1, .by = c(election_year, state, candidate_num, first_name))

election <- election |>
  mutate(
    first_name = word(candidate_name, 1)
  ) |>
  left_join(
    candidate_cpf,
    by = c("election_year", "state", "candidate_num", "first_name")
  ) |>
  mutate(
    cpf_candidate = if_else(
      nchar(cpf_candidate) == 11, cpf_candidate, cpf_fill
    )
  )

# bring outcomes and candidate status from the vote tallies
# the candidate data keeps a pre-election status for some winners whose
# registration was later reinstated (e.g. vinicius gurgel and rosinha da adefal
# in 2010), and flags as elected candidates absent from the vote tallies
# we match candidacies by election, round, office, electoral unit and ballot
# number, and then by cpf, electoral title or name, since withdrawn candidates
# share their ballot number with their substitute. for candidates without cpf
# in the vote tallies, we also match by first name (e.g. joao marcelo santos
# sousa/souza, 2014).
# this matches all but 0.1% of candidacies in the vote tallies. candidates
# absent from the vote tallies (e.g. registrations denied or withdrawn before
# the election, running mates and senate substitutes) keep the candidate data's
# labels
winner_outcomes <- c("eleito", "media", "eleito por media", "eleito por qp")
candidacy_key <- c(
  "election_year", "regular_election", "round", "position", "unit", "candidate_num"
)

# supplementary elections (e.g. for governor of amazonas in 2017) are recorded
# under the election year of the regular election they replace
is_regular_election <- function(election_type) {
  is.na(election_type) |
    election_type == "" |
    str_detect(election_type, "^eleic(oes|ao)( gerais| municipal)? [0-9]{4}$")
}

# municipal offices are contested by municipality, the presidency nationally,
# and the remaining offices by state
add_unit <- function(data) {
  data |>
    mutate(
      unit = case_when(
        position %in% c("prefeito", "vereador") ~
          str_remove(as.character(cod_tse), "^0+"),
        position == "presidente" ~ "br",
        T ~ state
      )
    )
}

clean_candidacy <- function(data) {
  data |>
    add_unit() |>
    mutate(
      regular_election = is_regular_election(election_type),
      cpf_candidate = if_else(nchar(cpf_candidate) == 11, cpf_candidate, NA),
      elec_title = if_else(elec_title %in% c("", "#ni#", "-1"), NA, elec_title),
      name_key = str_remove_all(candidate_name, "[^a-z]"),
      first_name = word(candidate_name, 1)
    )
}

person_key <- c("cpf_candidate", "elec_title", "name_key", "first_name")

election_status <- election_all |>
  clean_candidacy() |>
  distinct(
    across(all_of(c(candidacy_key, person_key))),
    outcome_election = outcome,
    status_election = na_if(candidate_status, "")
  )

candidate_key <- candidate |>
  mutate(row_id = row_number()) |>
  clean_candidacy() |>
  select(row_id, all_of(c(candidacy_key, person_key)))

match_election <- function(id, lookup = election_status) {
  lookup <- lookup |>
    filter(!is.na(.data[[id]])) |>
    distinct(
      across(all_of(c(candidacy_key, id))), outcome_election, status_election
    ) |>
    filter(n() == 1, .by = all_of(c(candidacy_key, id)))

  candidate_key |>
    inner_join(lookup, by = c(candidacy_key, id)) |>
    select(row_id, outcome_election, status_election)
}

candidate_election <- bind_rows(
  match_election("cpf_candidate"),
  match_election("elec_title"),
  match_election("name_key"),
  match_election(
    "first_name", election_status |> filter(is.na(cpf_candidate))
  )
) |>
  mutate(in_election = TRUE) |>
  distinct(row_id, .keep_all = TRUE)

candidate <- candidate |>
  mutate(row_id = row_number()) |>
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
    )
  ) |>
  select(-outcome_election, -status_election)

# marcus savius teixeira sousa (2002) withdrew and was replaced by a substitute
# under the same ballot number, but the candidate data flags him as elected
candidate <- candidate |>
  mutate(
    is_replaced = election_year == 2002 &
      position == "deputado federal" &
      candidate_name == "marcus savius teixeira sousa",
    outcome = if_else(
      is_replaced, "renuncia/falecimento/cassacao antes da eleicao", outcome
    ),
    elected = if_else(is_replaced, 0L, elected)
  ) |>
  select(-is_replaced)

# cpf-title pairs in the vote tallies, used below to assign cpfs shared by
# different people
cpf_title_tally <- election_all |>
  filter(
    nchar(cpf_candidate) == 11 &
    !(elec_title %in% c("", "#ni#", "-1")) &
    !is.na(elec_title)
  ) |>
  distinct(cpf_candidate, elec_title, election_year)

rm(election_all, election_status, candidate_key, candidate_election)

# create a flag for whether the candidate participated or not in the elections
candidate <- candidate |>
  mutate(
    flag_participated = if_else(
      candidate_status %in% c(
        "indeferido",
        "renuncia",
        "cancelado",
        "indeferido com recurso",
        "nao conhecimento do pedido",
        "inelegivel",
        "falecido",
        "falecimento"
      ),
      FALSE,
      TRUE
    )
  )

# select vars
election <- election %>%
  transmute(
    cod_ibge_6,
    state,
    party,
    election_year,
    cpf_candidate,
    candidate_name,
    coalition,
    age = election_year - as.integer(birthyear),
    candidate_status,
    state,
    outcome,
    vote
  )

# ensure that cpfs are unique by election year
# 4.6 thousand cpfs appear in more than one record within an election year:
# (1) different people (the electoral title and the birthyear or first name
# differ), e.g. elza alves pereira and maria marina silva (2004). the cpf stays
# with the person who holds it in other election years or, failing that, with
# the person the vote tallies pair it with (e.g. zairton cavalcante santos, who
# shares his cpf with a withdrawn 2006 governor candidate). it is set to missing
# for the others (or for all of them, when no one does)
# (2) the same person in both rounds, registered twice, running for two
# offices (e.g. candidates who withdrew from a ticket as running mates to run
# for city council) or in a supplementary election. we keep a single record,
# preferring regular elections, candidacies in the vote tallies, candidates who
# participated, were elected, and the latest round
cpf_title_year <- candidate_input |>
  filter(
    nchar(cpf_candidate) == 11 &
    !(elec_title %in% c("", "#ni#", "-1")) &
    !is.na(elec_title)
  ) |>
  distinct(cpf_candidate, elec_title, other_year = election_year)

candidate <- candidate |>
  mutate(
    shared_cpf = !is.na(cpf_candidate) &
      n_distinct(elec_title) > 1 &
      (n_distinct(birthyear) > 1 | n_distinct(word(candidate_name, 1)) > 1),
    .by = c(cpf_candidate, election_year)
  )

cpf_shared <- candidate |>
  filter(shared_cpf) |>
  distinct(cpf_candidate, election_year, elec_title)

cpf_owner_other_year <- cpf_shared |>
  inner_join(
    cpf_title_year,
    by = c("cpf_candidate", "elec_title"),
    relationship = "many-to-many"
  ) |>
  filter(other_year != election_year) |>
  distinct(cpf_candidate, election_year, elec_title) |>
  filter(n() == 1, .by = c(cpf_candidate, election_year))

cpf_owner_tally <- cpf_shared |>
  anti_join(cpf_owner_other_year, by = c("cpf_candidate", "election_year")) |>
  semi_join(
    cpf_title_tally,
    by = c("cpf_candidate", "elec_title", "election_year")
  ) |>
  filter(n() == 1, .by = c(cpf_candidate, election_year))

cpf_owner <- bind_rows(cpf_owner_other_year, cpf_owner_tally) |>
  mutate(cpf_owner = TRUE)

candidate <- candidate |>
  left_join(
    cpf_owner,
    by = c("cpf_candidate", "election_year", "elec_title")
  ) |>
  mutate(
    cpf_candidate = if_else(
      shared_cpf & is.na(cpf_owner), NA, cpf_candidate
    )
  ) |>
  arrange(
    desc(is_regular_election(election_type)),
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
    -row_id, -in_election, -shared_cpf, -cpf_owner, -cod_tse, -round, -election_type
  )

# keep the vote tallies consistent with the candidate data: cpfs set to missing
# above are also set to missing in the tallies
cpf_kept <- candidate |>
  filter(!is.na(cpf_candidate)) |>
  distinct(cpf_candidate, election_year) |>
  mutate(cpf_kept = TRUE)

election <- election |>
  left_join(cpf_kept, by = c("cpf_candidate", "election_year")) |>
  mutate(
    cpf_candidate = if_else(coalesce(cpf_kept, FALSE), cpf_candidate, NA)
  ) |>
  select(-cpf_kept)

rm(cpf_title_tally, cpf_shared, cpf_owner_other_year, cpf_owner_tally, cpf_kept)

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

# identify candidates in the party membership data
# we recover 679.3 thousand records
# we verify that 84.7% of records match the name exactly
candidate_party_membership <- party_membership %>%
  rename(
    elec_title = electoral_title
  ) %>%
  # only retain party members who appear in our candidate data
  inner_join(
    candidate %>% distinct(elec_title),
    by = c("elec_title")
  ) |>
  # add start year of party membership 
  mutate(
    year_start = year(
      ymd(date_start)
    )
  ) |> 
  # remove measurement error
  filter(
    # the majority of start dates are after 1980 and prior to 2019 which suggests
    # measurement error prior to that year
    year_start >= 1980 &
    year_start <= 2019
  ) |> 
  select(-matches("candidate"))

# by candidate and electoral cycle, identify the latest party membership record
# there are 973.4 thousand distinct electoral titles in the candidate database
# we recover 518.8 thousand from the party membership data, or 53.3%
# note: (1) this dataset only includes candidates whose party membership is available
# (2) there are candidates whose party registration when a candidate
# does not match the party membership data (19.4%). we drop those.
candidate_last_membership <- candidate |> 
  select(
    cpf_candidate,
    elec_title,
    election_year,
    party_candidate = party
  ) |> 
  left_join(
    candidate_party_membership,
    by = "elec_title",
    relationship = "many-to-many"
  ) |> 
  arrange(
    desc(date_start)
  ) |> 
  filter(
    year_start <= election_year &
    party_candidate == party
  )  |> 
  group_by(elec_title, election_year) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  select(-starts_with("party")) |> 
  rename(
    cod_ibge_6_party_membership = mun_name
  )

# join cod_ibge_6_birth: municipality of birth
# we fail to join 81442/1353874 = 6% of our sample
candidate_clean <- candidate |> 
  left_join(
    municipio,
    by = c("mun_birth_name", "state_birth_name")
  )

# write-out all federal candidates
candidate_clean %>%
  filter(election_year %in% seq(2002, 2014, 4)) %>%
  fwrite(
    here(
      "data", "output", "candidate", "cfscore_estimation", "candidate_federal_state.csv"
    )
  )

# write-out candidates
candidate_clean <- candidate %>%
  filter(
    election_year >= 2002
  )

for (i in unique(candidate_clean$election_year)) {
  if (i %in% seq(2002, 2014, 4)) {
    # only deputados
    candidate_clean %>%
      filter(election_year == i, position == "deputado federal") %>%
      fwrite(
        here(
          paste0(
            "data/output/candidate/fed_state/candidate_", i, ".csv"
          )
        )
      )
  } else {
    candidate_clean %>%
      filter(election_year == i) %>%
      fwrite(
        here(
          paste0(
            "data/output/candidate/local/candidate_", i, ".csv"
          )
        )
      )
  }
}

# write-out party membership
# 665.0 thousand records found
candidate_last_membership |>
  fwrite(
    here("data", "output", "candidate", "candidate_party_membership.csv.gz")
  )

# ---------------------------------------------------------------------------- #
# write-out election
for (i in seq(2002, 2014, 4)) {
  election %>%
    filter(election_year == i) %>%
    fwrite(
      here(
        paste0(
          "data/output/election/election_", i, ".csv"
        )
      )
    )
}

# blank votes
vote <- list.files(
  here("data", "input", "tse"),
  "vote_count",
  full.names = T
) %>%
  fread()

# extract deputado federais
vote <- vote %>%
  filter(
    position == "deputado federal"
  )

# fix missing codigo ibge: this seems to be boa esperanca do norte, MT
vote <- vote |>
  mutate(
    cod_ibge_6 = if_else(
      cod_tse == 91065 & is.na(cod_ibge_6),
      510183,
      cod_ibge_6
    )
  )

# write-out vote
for (i in unique(vote$election_year)) {
  vote %>%
    filter(election_year == i) %>%
    fwrite(
      here(
        paste0(
          "data/output/election/vote_count_", i, ".csv"
        )
      )
    )
}

# vote for legend
# extract deputados federais
coalition <- party_vote %>%
  filter(
    position == "deputado federal"
  )

# fix boa esperanca do norte
coalition <- coalition %>%
  mutate(
    cod_ibge_6 = if_else(
      mun_name == "boa esperanca do norte" & state == "mt",
      510183,
      cod_ibge_6
    )
  )

# aggregate by coalition
coalition <- coalition %>%
  group_by(
    election_year,
    cod_ibge_6,
    coalition,
    coalition_type
  ) %>%
  summarise(
    vote_legend = sum(vote_legend)
  ) %>%
  ungroup()

# write out votes for coalition
for (i in unique(coalition$election_year)) {
  coalition %>%
    filter(election_year == i) %>%
    fwrite(
      here(
        paste0(
          "data/output/election/coalition_vote_", i, ".csv"
        )
      )
    )
}
