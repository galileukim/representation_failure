# Input Requirements

These are the files the numbered scripts in `scripts/` read but never create. You have to supply them before you run the pipeline. All paths are relative to the repository root. Files that the pipeline itself creates are listed in [`outputs.md`](outputs.md).

The first section covers everything that belongs under `data/input/`, which is the folder to download from Dropbox. The second section covers files that the scripts currently read from somewhere else. Those paths need attention before the pipeline can run from a clean `data/input/` folder.

## 1. Files under `data/input/`

### `tse/`: raw TSE electoral data

| File | Read by | Notes |
|---|---|---|
| `candidate.csv.gz` | `01_candidates_wrangle.R`, `08_create_state_level_ideology.R` | TSE candidate registry, all years. The scripts filter it to 2002–2014. |
| `filiado.csv.gz` | `01_candidates_wrangle.R` | TSE party membership registry. |
| `election.csv.gz` | `01_candidates_wrangle.R` | Candidate vote totals by municipality. Matched exactly by `^election.csv.gz$`. |
| `*vote_count*` | `01_candidates_wrangle.R` | Aggregate vote counts by municipality. The name is not hard-coded: the script reads whichever file contains `vote_count`, so exactly one file may match. |
| `party.csv.gz` | `01_candidates_wrangle.R` | Party and coalition (legend) votes. Matched by the regex `party.csv.gz`, so exactly one file in the folder may match. |

### `municipal/`: municipality identifiers

| File | Read by | Notes |
|---|---|---|
| `municipios.csv` | `01_candidates_wrangle.R`, `04_complaints.R` | IBGE municipality crosswalk (`cod_ibge_6`, `nome_municipio`, `codigo_uf`, …). |
| `state_id.csv` | `01_candidates_wrangle.R`, `04_complaints.R` | Maps the IBGE state code (`state_id`) to the state abbreviation. |

### `contribution/`: campaign finance

| File | Read by | Notes |
|---|---|---|
| `campaign_fed_state.csv.gz` | `01_candidates_wrangle.R`, `05_contrib_matrix_pooled.R`, `12_validate_cfscore_with_spoils.R`, `15_candidate_figs.R` | Contributions to federal and state candidates. |
| `campaign_local.csv.gz` | `01_candidates_wrangle.R`, `05_contrib_matrix_pooled.R`, `15_candidate_figs.R` | Contributions to local candidates. `01` matches it by the substring `campaign_local`, and `05` also requires the `.csv.gz` extension. |

`05_contrib_matrix_pooled.R` and `15_candidate_figs.R` read **every** file in this folder whose name starts with `campaign`. Any other file with that prefix would be pooled into the contribution matrix without warning.

### `ideology/`: ideology benchmarks

| File | Read by | Notes |
|---|---|---|
| `legislative_2003*.csv` and `legislative_2007*.csv` | `01_candidates_wrangle.R` | Zucco and Lauderdale (2011) legislator ideal points. Matched by `^legislative_200[3\|7].*csv$` and labelled 2002 and 2006 in alphabetical order, so exactly two files may match. |
| `legislative_survey_ideology_party.csv` | `06_candidates_cfscore_pooled.R`, `07_candidates_pooled_robustness.R` | Power and Rodrigues-Silveira (2018) party ideology from the Brazilian Legislative Survey. It anchors the CFscore priors. |
| `latinobarometer_ideology.csv` | `15_candidate_figs.R` | Latinobarómetro left–right self-placement by year. |

### `candidate/`: hand-curated candidate records

| File | Read by | Notes |
|---|---|---|
| `missing/missing2010.csv` | `02_fix_defective_candidates.R` | Names of 2010 candidates whose CPF is missing from the election file. |
| `missing/missing2014.csv` | `02_fix_defective_candidates.R` | The same for 2014. Only the first two names are used. |
| `deputados.csv` | `15_candidate_figs.R` | Chamber of Deputies roster (`nomeCivil`, `dataNascimento`, `municipioNascimento`, …), used to link deputies to the roll-call ideal points. |

### `corruption/`: candidate cassation records

| File | Read by | Notes |
|---|---|---|
| `motivo_cassacao*` | `09_generate_valence_corruption.R` | TSE reasons for cassation, Latin-1 encoded. `fread()` reads a single file, so exactly one file may match. |
| `candidate_2018.csv` | `09_generate_valence_corruption.R` | TSE 2018 candidate list with the original upper-case TSE column names. |

### `spoils_of_victory/`: Brollo et al. (2013) replication data

| File | Read by | Notes |
|---|---|---|
| `spoils_of_victory_replication_data.RData` | `12_validate_cfscore_with_spoils.R` | Must contain the object `depfed_data`. |

### `state/`: state-level census

| File | Read by | Notes |
|---|---|---|
| `census_state.csv` | `15_candidate_figs.R` | State covariates for 2000 and 2010. |

### `identifiers/`: crosswalks

| File | Read by | Notes |
|---|---|---|
| `state_names.csv` | `15_candidate_figs.R` | Maps state abbreviations to full state names, for the LaTeX candidate table. |

### `falabr/`: citizen complaints

| File | Read by | Notes |
|---|---|---|
| One file per year, 2014–2018 | `04_complaints.R` | Fala.BR ombudsman records from CGU (<https://www.gov.br/cgu/pt-br/acesso-a-informacao/dados-abertos/arquivos/ouvidoria>). They are semicolon-delimited and windows-1252 encoded. The script reads every file in the folder and labels them 2014–2018 in alphabetical order, so the folder must hold exactly five files whose names sort by year. |

## 2. Inputs read from outside `data/input/`

These files are needed, but the scripts read them from paths that are stale (from before the `data/input` and `data/output` refactor) or that sit outside this repository. Until the paths are updated, each file has to be placed exactly where the code expects it.

| Path in the code | Read by | Issue |
|---|---|---|
| `data/municipal/input/censo_mun_<year>*` (one file per census year, matched by `censo_mun_[20]\d+`) | `03_censo_wrangle.R` | Old path. It probably belongs under `data/input/municipal/`. |
| `data/municipal/input/state_id.csv` | `03_censo_wrangle.R` | The same file as `data/input/municipal/state_id.csv`, read from the old path. |
| `data/municipal/input/receita_mun.csv` | `03_censo_wrangle.R` | FINBRA municipal revenue. Old path. |
| `data/maps/municipio.*` and `data/maps/estados_2010.*` (ESRI shapefiles) | `funs.R`, sourced by `15_candidate_figs.R` | Old path. The script reads them with `rgdal::readOGR()`, but rgdal is no longer on CRAN and is not in `renv.lock`. |
| `data/candidate/fed_state/candidate_200[2-6].csv` | `15_candidate_figs.R` | Old path for files that `01_candidates_wrangle.R` now writes to `data/output/candidate/fed_state/`. Only `candidate_2006.csv` would match there, because `01` writes no 2002 file. |
| `../estimation/unobserved_valence.csv` | `13_validate_unobserved_valence.R` | Lives in a sibling `estimation` repository, not in this one. |
| `../estimation/stacking.csv` | `15_candidate_figs.R` | Lives in a sibling `estimation` repository, not in this one. |
| `~/princeton/R/data/tse/data/wrangle/election.csv.gz` and `…/wrangle/*vote_count*` | `14_candidates_diagnostics.R` | An absolute path on an older machine. These appear to be the same files as `data/input/tse/election.csv.gz` and `data/input/tse/*vote_count*`. |
