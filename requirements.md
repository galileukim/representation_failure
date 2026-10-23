# Input Requirements

These are the files the numbered scripts in `scripts/` read but never create. You have to supply them before you run the pipeline. All paths are relative to the repository root. Files that the pipeline itself creates are listed in [`outputs.md`](outputs.md).

The first section covers everything that belongs under `data/input/`, which is the folder to download from Dropbox. The second section covers files that the scripts currently read from somewhere else. Those paths need attention before the pipeline can run from a clean `data/input/` folder.

## 1. Files under `data/input/`

### `tse/`: raw TSE electoral data

| File | Read by | Notes |
|---|---|---|
| `candidate.csv.gz` | `01a_electoral_wrangle.R`, `07_create_state_level_ideology.R` | TSE candidate registry, all years. The scripts filter it to 2002–2014. |
| `filiado.csv.gz` | `01a_electoral_wrangle.R` | TSE party membership registry. |
| `election.csv.gz` | `01a_electoral_wrangle.R`, `13_candidates_diagnostics.R` | Candidate vote totals by municipality. Matched exactly by `^election.csv.gz$`. |
| `*vote_count*` | `01a_electoral_wrangle.R`, `13_candidates_diagnostics.R` | Aggregate vote counts by municipality. The name is not hard-coded: the script reads whichever file contains `vote_count`, so exactly one file may match. |
| `party.csv.gz` | `01a_electoral_wrangle.R` | Party and coalition (legend) votes. Matched by the regex `party.csv.gz`, so exactly one file in the folder may match. |

### `municipal/`: municipality identifiers, census and revenue

| File | Read by | Notes |
|---|---|---|
| `municipios.csv` | `01a_electoral_wrangle.R`, `03_complaints.R` | IBGE municipality crosswalk (`cod_ibge_6`, `nome_municipio`, `codigo_uf`, …). |
| `state_id.csv` | `01a_electoral_wrangle.R`, `02_censo_wrangle.R`, `03_complaints.R` | Maps the IBGE state code (`state_id`) to the state abbreviation. |
| `censo_mun_<year>.csv` | `02_censo_wrangle.R` | Municipal census covariates, one file per census year (2000 and 2010). Matched by `censo_mun_[20]\d+`. |
| `receita_mun.csv` | `02_censo_wrangle.R` | FINBRA municipal revenue. |

### `maps/`: boundary shapefiles

| File | Read by | Notes |
|---|---|---|
| `municipio.*` | `funs.R`, sourced by `14_candidate_figs.R` | ESRI shapefile of municipal boundaries (`.shp`, `.shx`, `.dbf`, `.prj`). Read with `sf::st_read()`. The attribute table must have the IBGE code in a column named `MUNICÍPI0`. |
| `estados_2010.*` | `funs.R`, sourced by `14_candidate_figs.R` | ESRI shapefile of 2010 state boundaries. |

### `contribution/`: campaign finance

| File | Read by | Notes |
|---|---|---|
| `campaign_fed_state.csv.gz` | `01b_cfscore_ideal_point_wrangle.R`, `04_contrib_matrix_pooled.R`, `11_validate_cfscore_with_spoils.R`, `14_candidate_figs.R` | Contributions to federal and state candidates. |
| `campaign_local.csv.gz` | `01b_cfscore_ideal_point_wrangle.R`, `04_contrib_matrix_pooled.R`, `14_candidate_figs.R` | Contributions to local candidates. `01b` matches it by the substring `campaign_local`, and `04` also requires the `.csv.gz` extension. |

`04_contrib_matrix_pooled.R` and `14_candidate_figs.R` read **every** file in this folder whose name starts with `campaign`. Any other file with that prefix would be pooled into the contribution matrix without warning.

### `ideology/`: ideology benchmarks

| File | Read by | Notes |
|---|---|---|
| `legislative_2003*.csv` and `legislative_2007*.csv` | `01b_cfscore_ideal_point_wrangle.R` | Zucco and Lauderdale (2011) legislator ideal points. Matched by `^legislative_200[3\|7].*csv$` and labelled 2002 and 2006 in alphabetical order, so exactly two files may match. |
| `legislative_survey_ideology_party.csv` | `05_candidates_cfscore_pooled.R`, `06_candidates_pooled_robustness.R` | Power and Rodrigues-Silveira (2018) party ideology from the Brazilian Legislative Survey. It anchors the CFscore priors. |
| `latinobarometer_ideology.csv` | `14_candidate_figs.R` | Latinobarómetro left–right self-placement by year. |

### `candidate/`: hand-curated candidate records

| File | Read by | Notes |
|---|---|---|
| `missing/missing2010.csv` | none | Names of 2010 candidates whose CPF is missing from the election file. No longer needed: `01a` fills these CPFs by ballot number. |
| `missing/missing2014.csv` | none | The same for 2014. |
| `deputados.csv` | `14_candidate_figs.R` | Chamber of Deputies roster (`nomeCivil`, `dataNascimento`, `municipioNascimento`, …), used to link deputies to the roll-call ideal points. |

### `corruption/`: candidate cassation records

| File | Read by | Notes |
|---|---|---|
| `motivo_cassacao*` | `08_generate_valence_corruption.R` | TSE reasons for cassation, Latin-1 encoded. `fread()` reads a single file, so exactly one file may match. |
| `candidate_2018.csv` | `08_generate_valence_corruption.R` | TSE 2018 candidate list with the original upper-case TSE column names. |

### `spoils_of_victory/`: Brollo et al. (2013) replication data

| File | Read by | Notes |
|---|---|---|
| `spoils_of_victory_replication_data.RData` | `11_validate_cfscore_with_spoils.R` | Must contain the object `depfed_data`. |

### `state/`: state-level census

| File | Read by | Notes |
|---|---|---|
| `census_state.csv` | `14_candidate_figs.R` | State covariates for 2000 and 2010. |

### `identifiers/`: crosswalks

| File | Read by | Notes |
|---|---|---|
| `state_names.csv` | `14_candidate_figs.R` | Maps state abbreviations to full state names, for the LaTeX candidate table. |

### `falabr/`: citizen complaints

| File | Read by | Notes |
|---|---|---|
| One file per year, 2014–2018 | `03_complaints.R` | Fala.BR ombudsman records from CGU (<https://www.gov.br/cgu/pt-br/acesso-a-informacao/dados-abertos/arquivos/ouvidoria>). They are semicolon-delimited and windows-1252 encoded. The script reads every file in the folder and labels them 2014–2018 in alphabetical order, so the folder must hold exactly five files whose names sort by year. |

## 2. Inputs read from outside `data/input/`

These files are needed, but the scripts read them from outside this repository. Until the paths are updated, each file has to be placed exactly where the code expects it.

| Path in the code | Read by | Issue |
|---|---|---|
| `../estimation/unobserved_valence.csv` | `12_validate_unobserved_valence.R` | Lives in a sibling `estimation` repository, not in this one. |
| `../estimation/stacking.csv` | `14_candidate_figs.R` | Lives in a sibling `estimation` repository, not in this one. |
