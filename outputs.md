# Pipeline Outputs

This file lists every file that the numbered scripts in `scripts/` write, grouped by the script that writes it and in run order. All paths are relative to the repository root. "Used by" names the later scripts that read the file; "—" means the file is a final output that nothing downstream reads. For column-level schemas, see [`docs/codebook.md`](docs/codebook.md). The inputs the pipeline needs are listed in [`requirements.md`](requirements.md).

## 1. Data files (`data/output/`)

### `01a_electoral_wrangle.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/candidate/cfscore_estimation/candidate_federal_state.csv` | `01b`, `04`, `05`, `09` | Candidates for every position in 2002, 2006, 2010 and 2014. This is the only candidate file that carries `cod_ibge_6_birth`. Missing 2002 CPFs are recovered where possible, and federal deputies' `elected`/`outcome` are reconciled with the vote tallies. |
| `data/output/candidate/fed_state/candidate_{2002,2006,2010,2014}.csv` | `08`, `10`, `11`, `12`, `14` | Federal deputies only. 186 of the 4,900 candidates in 2002 have no CPF in the raw TSE data; 183 of them received no votes. |
| `data/output/candidate/local/candidate_{2004,2008,2012}.csv` | `01b`, `04`, `05`, `14` | All local candidates, one file per municipal election year. |
| `data/output/candidate/candidate_party_membership.csv.gz` | — | The latest party membership record for each candidate and election year. |
| `data/output/election/election_{2002,2006,2010,2014}.csv` | `10`, `14` | Federal deputies' votes by municipality. Votes cast in Boa Esperança do Norte (MT, 2002), which has no IBGE code, are dropped. Five losing candidates have no CPF: three in 2002 and two in 2014 (absent from the candidate registry). |
| `data/output/election/vote_count_{year}.csv` | — | Federal deputy vote counts by municipality, one file per year in the input. |
| `data/output/election/coalition_vote_{year}.csv` | — | Federal deputy legend votes by coalition and municipality, one file per year in the input. |

### `01b_cfscore_ideal_point_wrangle.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/ideology/contrib_matrix_fed_state.RData` | `09` | Holds the object `cm`, a list of `candidate`, `contribution` and `contrib_matrix`. |
| `data/output/ideology/contrib_matrix_local.RData` | — | The same structure for local candidates. Only the scripts in `scripts/deprecated/` read it. |
| `data/output/contribution/contribution_by_candidate_fed_state.csv` | — | Total contributions per candidate and election year, excluding projected contributions. |
| `data/output/contribution/contribution_by_candidate_local.csv` | — | The same for local candidates. |
| `data/output/ideology/legislative_ideology.csv` | `14` | Zucco and Lauderdale (2011) legislator ideal points for 2002 and 2006. |

### `02_censo_wrangle.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/municipal/municipal_data.csv` | `14` | Municipal census covariates for 2000 and 2010, joined with FINBRA budget data. |

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
| `data/output/ideology/candidate_ideology_pooled_fed_state.csv` | `07`, `10`, `14` | Pooled CFscores for federal and state candidates. |
| `data/output/ideology/candidate_ideology_pooled_local.csv` | `14` | Pooled CFscores for local candidates. |
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
| `data/output/ideology/candidate_ideology_only_state.csv` | — | Candidates for state offices (governor, state deputy, …) in 2006, 2010 and 2014, with their CFscores. |

### `08_generate_valence_corruption.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/corruption/corrupt_candidate_2018.csv` | `12` | 2018 candidates with a corruption-related cassation. |

### `12_validate_unobserved_valence.R`

| File | Used by | Notes |
|---|---|---|
| `data/output/corruption/candidate_corruption_with_valence.csv` | — | Elected 2014 deputies with a corruption flag and unobserved valence. |

Scripts `09`, `10`, `11`, `13` and `14` write no data files.

## 2. Figures and tables

### Inside the repository (`figs/`)

| File | Written by |
|---|---|
| `figs/histogram_pooled_cfscore.pdf` | `05_candidates_cfscore_pooled.R` |
| `figs/cfscore_pooled_campaign_vs_survey.pdf` | `05_candidates_cfscore_pooled.R` |
| `figs/cfscore_pooled_by_party.pdf` | `05_candidates_cfscore_pooled.R` |

### Outside `figs/` (`Presentation/` paths)

Scripts `10`, `11` and `14` save their figures under a `Presentation/` folder, which is not part of this repository. How the code builds each path determines where the file actually goes:

- `here("../Presentation/…")` resolves to a `Presentation/` folder next to the repository.
- `here("Presentation/…")` resolves to `Presentation/` inside the repository, which does not exist.
- `"Presentation/…"` with no `here()` is relative to the working directory.

`ggsave()` does not create missing folders, so these calls fail unless the folder already exists.

| File | Written by | Path form in the code |
|---|---|---|
| `Presentation/figs/ideology/cfscore_robustness.pdf` | `10_validate_cfscore.R` | `here("../Presentation/…")` |
| `Presentation/figs/ideology/validation_contracts_contribution.pdf` | `11_validate_cfscore_with_spoils.R` | `here("../Presentation/…")` |
| `Presentation/figs/demo/map_br.png` | `14_candidate_figs.R` | `"Presentation/…"` |
| `Presentation/figs/demo/map_{median_wage,lit_rate,rural,sanitation}.png` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/campaign/campaign_total.png` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/campaign/campaign_distribution.png` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/party/party_vote_share.png` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/party/herfindahl_median_wage.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/party/herfindahl_lm.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/party/party_share_fit.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/party_vote_share_state.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/ideology/party_ideology_median_wage.pdf` | `14_candidate_figs.R` | `here("../Presentation/…")` |
| `Presentation/figs/ideology/party_ideology_rural.pdf` | `14_candidate_figs.R` | `here("../Presentation/…")` |
| `Presentation/figs/ideology/party_ideology_lower_school.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/ideology/party_ideology.pdf` | `14_candidate_figs.R` | `"Presentation/…"` |
| `Presentation/figs/ideology/candidates_elected_ideology.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/ideology/party_ideology_deputy_mayor.png` | `14_candidate_figs.R` | `here("../Presentation/…")` |
| `Presentation/figs/ideology/reg_ideology.png` | `14_candidate_figs.R` | `here("../Presentation/…")` |
| `Presentation/figs/ideology/reg_electorate_ideology.png` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/ideology/electorate_ideology.png` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/ideology/ideology_cfscore.png` | `14_candidate_figs.R` | `here("../Presentation/…")` |
| `Presentation/figs/ideology/candidate_edu_ideology.png` | `14_candidate_figs.R` | `"Presentation/…"` |
| `Presentation/figs/ideology/candidate_occupation_ideology.png` | `14_candidate_figs.R` | `"Presentation/…"` |
| `Presentation/figs/candidate/candidate_valence_states.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/candidate/valence_state.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/candidate/valence_elected_nonelected.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/candidate/valence_selection.pdf` | `14_candidate_figs.R` | `here("Presentation/…")` |
| `Presentation/figs/candidate/valence_candidate_party.pdf` | `14_candidate_figs.R` | `"Presentation/…"` |

All of these figures except the four `map_*.png` files are saved with the `plot + ggsave(…)` pattern. With the ggplot2 version pinned in `renv.lock` (4.0.3), `ggsave()` writes the file and then the `+` step fails with "Can't add … to a ggplot object". The script stops at that point, so each script writes at most one of these figures in a single run.

### Printed to the console only

| Output | Written by |
|---|---|
| Donor and candidate counts for the federal/state contribution matrix | `09_validate_contrib_matrix.R` |
| Correlations between the full-sample and robustness CFscores | `10_validate_cfscore.R` |
| Four stargazer LaTeX tables (logit and LPM, with and without unobserved valence) | `12_validate_unobserved_valence.R` |
| LaTeX table of candidates by state and year (kableExtra) | `14_candidate_figs.R` |

## 3. Committed files that the current scripts do not write

These files are in the repository, but no numbered script produces them. They were made by earlier versions of the code or by hand:

- `figs/cfscore_campaign_vs_survey.{pdf,png}`, `figs/cfscore_fed_state_by_party.{pdf,png}` and `figs/histogram_fed_state_cfscore.{pdf,png}`. The `.png` versions come from `scripts/deprecated/candidates_cfscore.R`.
- `figs/cfscore_pooled_by_party.png` and `figs/cfscore_pooled_campaign_vs_survey.png`. `05` now writes only the `.pdf` versions.
- `figs/histogram_bonica_cfscore.png` and `figs/robustness/cfscore_robustness.png`.
- Everything in `results/`. The `validate_*_valence.tex` files appear to be the console output of `12`, wrapped by hand in a LaTeX document.
