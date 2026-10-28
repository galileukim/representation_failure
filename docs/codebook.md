# Codebook — Representation Failure Replication Data

All files live under `data/` in the project root. Column descriptions are inferred from variable names, types, and example values.

---

## 1. `data/output/municipal/municipal_data.csv`

Municipality-level socioeconomic panel from the Brazilian Census (IBGE), long by municipality × census year (2000, 2010).

| Column | Type | Description |
|---|---|---|
| `cod_ibge_6` | integer | 6-digit IBGE municipality code |
| `state` | character | Two-letter state abbreviation |
| `year` | integer | Census year (2000 or 2010) |
| `age` | numeric | Average age of residents |
| `black_or_brown` | numeric | Share of residents identifying as *pardo* or *preto* (%) |
| `domestic_light` | numeric | Share of households with electricity (%) |
| `employment_rate` | numeric | Employment rate (%) |
| `female` | numeric | Share of female residents (%) |
| `garbage` | numeric | Share of households with garbage collection (%) |
| `high_school` | numeric | Share of adults with complete high school (%) |
| `higher_education` | numeric | Share of adults with higher education (%) |
| `lit_rate` | numeric | Adult literacy rate (%) |
| `log_budget_2002/2006/2010` | numeric | Log of municipal budget (BRL) in the given year |
| `log_fed_transfer_2002/2006/2010` | numeric | Log of federal transfers (BRL) in the given year |
| `lower_school` | numeric | Share of adults with incomplete primary school (%) |
| `median_wage` | numeric | Median monthly wage (BRL) |
| `middle_school` | numeric | Share of adults with complete middle school (%) |
| `pop` | integer | Total population |
| `rural_pop` | numeric | Share of rural population (%) |
| `sanitation` | numeric | Share of households with sanitation (%) |
| `water` | numeric | Share of households with piped water (%) |

---

## 2. `data/output/candidate/fed_state/candidate_{year}.csv`

Cleaned candidate-level data for federal (*Deputado Federal*, *Senador*) and state (*Deputado Estadual*) elections. Available for 2006, 2010, 2014.

| Column | Type | Description |
|---|---|---|
| `cpf_candidate` | integer64 | Candidate CPF (unique national tax ID) |
| `elec_title` | integer64 | Electoral registration number |
| `state` | character | State of candidacy (2-letter abbreviation) |
| `candidate_num` | integer | Ballot number |
| `candidate_name` | character | Full candidate name |
| `position` | character | Office sought (e.g. *deputado federal*) |
| `election_year` | integer | Election year |
| `coalition` | character | Name/list of parties in the candidate's coalition. In 2010, three parties appear both alone and in a coalition; see the note in section 7 |
| `birthyear` | integer | Year of birth |
| `candidate_status` | character | TSE candidacy status (e.g. *deferido*, *indeferido*) |
| `edu` | character | Highest level of education |
| `gender` | integer | Gender code (2 = male, 4 = female) |
| `race` | character | Self-reported race/colour (*branca*, *parda*, *preta*, …) |
| `incumbent` | integer | 1 if candidate currently holds the same office |
| `elected` | integer | 1 if candidate was elected |
| `mun_birth_code` | integer | IBGE code of birth municipality |
| `mun_birth_name` | character | Name of birth municipality |
| `state_birth_name` | character | State of birth |
| `outcome` | character | Election outcome label (e.g. *eleito*, *suplente*, *não eleito*) |
| `party` | character | Party abbreviation |
| `times_office` | integer | Number of prior terms in the same office |
| `times_run` | integer | Number of times previously run for any office |
| `position_previous` | character | Most recent previously held office (if any) |
| `occupation` | character | Declared occupation |
| `occupation_code` | integer | TSE occupation code |
| `age` | integer | Age at time of election |

---

## 3. `data/output/candidate/local/candidate_{year}.csv`

Same schema as the federal/state candidate file above, for local elections (*Prefeito*, *Vereador*). Available for 2004, 2008, 2012.

---

## 4. `data/output/ideology/candidate_ideology_pooled_fed_state.csv`

Pooled CFscore estimates for federal and state legislative candidates.

| Column | Type | Description |
|---|---|---|
| `cpf_candidate` | integer64 | Candidate CPF — links to candidate files |
| `cfscore` | numeric | Estimated ideal point (CFscore); higher = more right-wing |

---

## 5. `data/output/ideology/candidate_ideology_pooled_local.csv`

Same schema as `candidate_ideology_pooled_fed_state.csv`, for local (municipal) candidates.

---

## 6. `data/output/election/election_{year}.csv`

Candidate-level vote totals by municipality, for federal/state elections. Available for 2002–2014.

| Column | Type | Description |
|---|---|---|
| `cod_ibge_6` | integer | 6-digit IBGE municipality code |
| `state` | character | State abbreviation |
| `party` | character | Party abbreviation |
| `election_year` | integer | Election year |
| `cpf_candidate` | integer64 | Candidate CPF |
| `candidate_name` | character | Candidate name |
| `coalition` | character | Coalition composition |
| `age` | integer | Age of candidate at election |
| `candidate_status` | character | TSE candidacy status |
| `outcome` | character | Election outcome |
| `vote` | integer | Number of nominal votes received in this municipality |

---

## 7. `data/output/election/coalition_vote_{year}.csv`

Party-list (legend) votes aggregated by coalition and municipality. Available for 2002–2014.

| Column | Type | Description |
|---|---|---|
| `election_year` | integer | Election year |
| `cod_ibge_6` | integer | 6-digit IBGE municipality code |
| `coalition` | character | Coalition composition |
| `coalition_type` | character | Type of grouping (*partido* or *coligacao*) |
| `vote_legend` | integer | Votes cast for the party/coalition list |

**Parties listed both alone and in a coalition (2010).** In three states, a party has its own label while its partners' coalition label still names it: PSOL in AP (`pcb / pmn / prp / prtb / psdc / psol / ptb / ptc`), PTN in PA (`phs / pp / pr / psc / pt / pt do b / ptb / ptc / ptn`) and PHS in RR (`phs / pp / psb / psc / pt / ptb / ptc`). This is how the TSE vote tallies record them: the party's own rows are *partido isolado*, while the TSE candidate registry places all its candidates in the coalition. The tally version is consistent with the seat allocation. In RR it is the only version that reproduces the result: counting PHS's votes within the coalition would move a seat from the PRB/PRP coalition to PHS's partners. In AP and PA both versions give the same seats. The likely cause is that the party's participation in the coalition was annulled after registration, but the data cannot confirm this. The same labels appear in `coalition` in the candidate files (section 2).

---

## 8. `data/output/election/vote_count_{year}.csv`

Aggregate vote-count statistics by municipality × position × round. Available for 2002–2014.

| Column | Type | Description |
|---|---|---|
| `cod_tse` | integer | TSE municipality code |
| `cod_ibge_6` | integer | 6-digit IBGE municipality code |
| `election_year` | integer | Election year |
| `election_type` | character | Official election name (e.g. *Eleições Gerais 2014*) |
| `position` | character | Office (e.g. *Deputado Federal*) |
| `position_code` | integer | TSE position code |
| `registered_voters` | integer | Number of registered voters |
| `round` | integer | Electoral round (1 or 2) |
| `state` | character | State abbreviation |
| `vote_abstention` | integer | Number of abstentions |
| `vote_blank` | integer | Number of blank ballots |
| `vote_cancelled` | integer | Number of cancelled/null ballots |
| `vote_legend` | integer | List/legend votes |
| `vote_nominal` | integer | Nominal (candidate-specific) votes |
| `vote_turnout` | integer | Total valid turnout |

---

## 9. `data/output/candidate/candidate_party_membership.csv.gz`

TSE party membership registry linked to candidates at election time. Compressed CSV (`.csv.gz`).

| Column | Type | Description |
|---|---|---|
| `cpf_candidate` | integer64 | Candidate CPF |
| `elec_title` | integer | Electoral registration number |
| `election_year` | integer | Election year the candidate ran |
| `cod_ibge_6` | integer | IBGE municipality where membership was recorded |
| `state` | character | State abbreviation |
| `cod_ibge_6_party_membership` | character | Municipality name of party registration |
| `electoral_zone` | integer | Electoral zone number |
| `electoral_section` | integer | Electoral section number |
| `member_name` | character | Name on the membership record |
| `date_record_extraction` | date | Date the record was extracted from TSE |
| `date_start` | date | Date party membership began |
| `date_end` | date | Date party membership ended (NA if active) |
| `date_cancel` | date | Date membership was cancelled (NA if not cancelled) |
| `reason_cancel` | character | Reason for cancellation (NA if not cancelled) |
| `member_status` | character | Current membership status (e.g. *regular*) |
| `year_start` | integer | Year membership started |

---

## 10. `data/input/municipal/municipios.csv`

Reference crosswalk of Brazilian municipalities with geographic identifiers.

| Column | Type | Description |
|---|---|---|
| `cod_ibge_6` | integer | 6-digit IBGE municipality code |
| `nome_municipio` | character | Municipality name |
| `latitude` | numeric | Latitude of the municipality centroid |
| `longitude` | numeric | Longitude of the municipality centroid |
| `capital` | integer | 1 if state capital, 0 otherwise |
| `codigo_uf` | integer | IBGE state (UF) code |

---

## 11. `data/output/ideology/legislative_ideology.csv`

Survey-based party ideology scores for federal legislators, from the Legislative Survey (PELA/Salamanca), used to validate CFscores.

| Column | Type | Description |
|---|---|---|
| `candidate_name` | character | Legislator name |
| `election_year` | integer | Election year (2002 or 2006) |
| `party` | character | Party abbreviation |
| `state` | character | State of election |
| `ideology` | numeric | Left–right ideology score from the legislative survey |

