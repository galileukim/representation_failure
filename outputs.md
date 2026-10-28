# Pipeline Outputs

This file lists every file that the numbered scripts in `scripts/` write, grouped by the script that writes it and in run order. All paths are relative to the repository root. "Used by" names the later scripts that read the file; "—" means the file is a final output that nothing downstream reads. For column-level schemas, see [`docs/codebook.md`](docs/codebook.md). The inputs the pipeline needs are listed in [`requirements.md`](requirements.md).

## 1. Data files (`data/output/`)

### `01a_electoral_wrangle.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/candidate/cfscore_estimation/candidate_federal_state.csv` | `01b`, `04`, `05`, `07`, `09` | Candidates for every position in 2002, 2006, 2010 and 2014. This is the only candidate file that carries `cod_ibge_6_birth`. Missing 2002 CPFs are recovered where possible, and federal deputies' `elected`/`outcome` are reconciled with the vote tallies. Coalition labels for statewide offices come from the party-level tallies (see `party.csv.gz` in `requirements.md`). Parties absent from the tallies whose raw label is blank are labeled with their own name if they ran alone, or with the parties registered under the same coalition name; this rebuilds the labels of DF district deputies in 2006–2010, which can't be checked against the seat allocation. |
| `data/output/candidate/fed_state/candidate_{2002,2006,2010,2014}.csv` | `08`, `10`, `11`, `12`, `13` | Federal deputies only. Coalition labels reproduce the seat allocation of every chamber except one seat in ES 2006, where two lists finished 73 votes apart. In 2010, PSOL (AP), PTN (PA) and PHS (RR) run alone in the tallies, while their partners' coalition labels still name them; this follows the TSE tallies, not a labeling error (see the note in [`docs/codebook.md`](docs/codebook.md), section 7). 186 of the 4,900 candidates in 2002 have no CPF in the raw TSE data; 183 of them received no votes. |
| `data/output/candidate/local/candidate_{2004,2008,2012}.csv` | `01b`, `04`, `05`, `13` | All local candidates, one file per municipal election year. |
| `data/output/candidate/candidate_party_membership.csv.gz` | — | The latest party membership record for each candidate and election year. |
| `data/output/election/election_{2002,2006,2010,2014}.csv` | `10`, `13` | Federal deputies' votes by municipality. Five losing candidates have no CPF: three in 2002 and two in 2014 (absent from the candidate registry). |
| `data/output/election/vote_count_{year}.csv` | — | Federal deputy vote counts by municipality, one file per year in the input. |
| `data/output/election/coalition_vote_{year}.csv` | — | Federal deputy legend votes by coalition and municipality, one file per year in the input. In 2010, three parties have their own row and are also named in a partner coalition's label (see the note in [`docs/codebook.md`](docs/codebook.md), section 7). |

### `01b_cfscore_ideal_point_wrangle.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/ideology/contrib_matrix_fed_state.RData` | `09` | Holds the object `cm`, a list of `candidate`, `contribution` and `contrib_matrix`. |
| `data/output/ideology/contrib_matrix_local.RData` | — | The same structure for local candidates. Only the scripts in `scripts/deprecated/` read it. |
| `data/output/contribution/contribution_by_candidate_fed_state.csv` | — | Total contributions (`total_contrib`) and distinct donors (`total_contributors`) per candidate and election year, excluding projected contributions. Donors include companies. |
| `data/output/contribution/contribution_by_candidate_local.csv` | — | The same for local candidates. |
| `data/output/ideology/legislative_ideology.csv` | `13` | Zucco and Lauderdale (2011) legislator ideal points for 2002 and 2006. |

### `02_censo_wrangle.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/municipal/municipal_data.csv` | `13` | Municipal census covariates for 2000 and 2010, joined with FINBRA budget data. |

### `03_complaints.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/complaints/municipal_complaints.csv` | — | Fala.BR *denúncias* and *reclamações* about municipal bodies, counted by municipality and year from 2014 to July 2018. |

### `04_contrib_matrix_pooled.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/ideology/contrib_matrix_pooled.RData` | `05`, `06` | Holds the object `cm`, pooling federal, state and local candidates. |

### `05_candidates_cfscore_pooled.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/ideology/candidate_ideology_pooled_fed_state.csv` | `07`, `10`, `13` | Pooled CFscores for federal and state candidates, with `n_donors`, the number of distinct donors behind each score. |
| `data/output/ideology/candidate_ideology_pooled_local.csv` | `13` | Pooled CFscores for local candidates. |
| `data/output/ideology/cfscore_pooled.RData` | — | Holds the object `cfscore_pooled`, the full `awm()` result. |

### `06_candidates_pooled_robustness.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/ideology/candidate_ideology_pooled_5_percentile.csv` | `10` | CFscores re-estimated after dropping the top 5% of donors. |
| `data/output/ideology/candidate_ideology_pooled_10_percentile.csv` | `10` | CFscores re-estimated after dropping the top 10% of donors. |
| `data/output/ideology/cfscore_pooled_robustness.RData` | — | Holds the object `cfscore_robust`. |

### `07_create_state_level_ideology.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/ideology/candidate_ideology_only_state.csv` | — | Candidates for state offices (governor, state deputy, …) in 2006, 2010 and 2014, with their CFscores. Built from the cleaned `candidate_federal_state.csv`, so it has one record per CPF and year, and `age` is computed from the birth year. |

### `08_generate_valence_corruption.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/corruption/corrupt_candidate_2018.csv` | `12` | 2018 candidates with a corruption-related cassation. |

### `12_validate_unobserved_valence.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/corruption/candidate_corruption_with_valence.csv` | — | Elected 2014 deputies with a corruption flag and unobserved valence. |

Scripts `09`, `10`, `11` and `13` write no data files.

## 2. Figures and tables

### Figures (`figs/`)

Every figure is saved under `figs/`. Paths written without `here()` resolve against the working directory, so they assume the scripts run from the repository root, as `00_main.R` does. `13` creates its figure subfolders before saving, since `ggsave()` does not.

| File | Written by |
|---|---|
| `figs/ideology/histogram_pooled_cfscore.pdf` | `05_candidates_cfscore_pooled.R` |
| `figs/ideology/cfscore_pooled_campaign_vs_survey.pdf` | `05_candidates_cfscore_pooled.R` |
| `figs/ideology/cfscore_pooled_by_party.pdf` | `05_candidates_cfscore_pooled.R` |
| `figs/ideology/cfscore_robustness.pdf` | `10_validate_cfscore.R` |
| `figs/ideology/validation_contracts_contribution.pdf` | `11_validate_cfscore_with_spoils.R` |
| `figs/demo/map_br.png` | `13_candidate_figs.R` |
| `figs/demo/map_{median_wage,lit_rate,rural,sanitation}.png` | `13_candidate_figs.R` |
| `figs/campaign/campaign_total.png` | `13_candidate_figs.R` |
| `figs/campaign/campaign_distribution.png` | `13_candidate_figs.R` |
| `figs/party/party_vote_share.png` | `13_candidate_figs.R` |
| `figs/party/herfindahl_median_wage.pdf` | `13_candidate_figs.R` |
| `figs/party/herfindahl_lm.pdf` | `13_candidate_figs.R` |
| `figs/party_vote_share_state.pdf` | `13_candidate_figs.R` |
| `figs/party/party_share_fit.pdf` | `13_candidate_figs.R` |
| `figs/ideology/party_ideology_median_wage.pdf` | `13_candidate_figs.R` |
| `figs/ideology/party_ideology_rural.pdf` | `13_candidate_figs.R` |
| `figs/ideology/party_ideology_lower_school.pdf` | `13_candidate_figs.R` |
| `figs/ideology/party_ideology.pdf` | `13_candidate_figs.R` |
| `figs/ideology/candidates_elected_ideology.pdf` | `13_candidate_figs.R` |
| `figs/ideology/party_ideology_deputy_mayor.png` | `13_candidate_figs.R` |
| `figs/ideology/reg_ideology.png` | `13_candidate_figs.R` |
| `figs/ideology/reg_electorate_ideology.png` | `13_candidate_figs.R` |
| `figs/ideology/electorate_ideology.png` | `13_candidate_figs.R` |
| `figs/ideology/ideology_cfscore.png` | `13_candidate_figs.R` |
| `figs/candidate/candidate_valence_states.pdf` | `13_candidate_figs.R` |
| `figs/candidate/valence_state.pdf` | `13_candidate_figs.R` |
| `figs/candidate/valence_elected_nonelected.pdf` | `13_candidate_figs.R` |
| `figs/candidate/valence_selection.pdf` | `13_candidate_figs.R` |
| `figs/candidate/valence_education.pdf` | `13_candidate_figs.R` |
| `figs/candidate/valence_rural_edu.pdf` | `13_candidate_figs.R` |
| `figs/candidate/valence_lit_incumbent.pdf` | `13_candidate_figs.R` |
| `figs/candidate/valence_candidate_party.pdf` | `13_candidate_figs.R` |
| `figs/ideology/candidate_edu_ideology.png` | `13_candidate_figs.R` |
| `figs/ideology/candidate_occupation_ideology.png` | `13_candidate_figs.R` |

### Printed to the console only

| Output | Written by |
|---|---|
| Donor and candidate counts for the federal/state contribution matrix | `09_validate_contrib_matrix.R` |
| Correlations between the full-sample and robustness CFscores | `10_validate_cfscore.R` |
| Four stargazer LaTeX tables (logit and LPM, with and without unobserved valence) | `12_validate_unobserved_valence.R` |
| LaTeX table of candidates by state and year (kableExtra) | `13_candidate_figs.R` |

## 3. Files in the repository that the current scripts do not write

These files are in the repository, but no numbered script produces them. They were made by earlier versions of the code or by hand:

- `figs/ideology/cfscore_campaign_vs_survey.{pdf,png}`, `figs/ideology/cfscore_fed_state_by_party.{pdf,png}` and `figs/ideology/histogram_fed_state_cfscore.{pdf,png}`. The `.png` versions come from `scripts/deprecated/candidates_cfscore.R`.
- `figs/ideology/cfscore_pooled_campaign_vs_survey.png` and `figs/ideology/cfscore_pooled_by_party.png`. `05` now writes only the `.pdf` versions.
- `figs/ideology/histogram_bonica_cfscore.png` and `figs/robustness/cfscore_robustness.png`.
- Everything in `results/`. The `validate_*_valence.tex` files appear to be the console output of `12`, wrapped by hand in a LaTeX document.
