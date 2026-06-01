# Representation Failure — Replication Repository

## Overview

This repository contains the replication code for the paper on representation failure and candidate ideology in Brazilian elections. The analysis estimates candidate ideal points (CFscores) using campaign contribution data and validates them against survey-based measures.

## Directory Structure

```
representation_failure/
├── data/                        # Not committed — see Data section below
│   ├── candidate/               # Candidate-level data (federal, state, local elections)
│   │   ├── fed_state/           # Federal and state legislative candidates
│   │   ├── local/               # Municipal-level candidates
│   │   ├── missing/             # Manually corrected missing observations
│   │   └── cfscore_estimation/  # Intermediate estimation outputs
│   ├── municipal/               # Municipal covariates (IBGE, TSE revenue data)
│   └── tse/                     # Raw TSE electoral data (candidates, parties, members)
├── scripts/                     # R scripts (numbered workflow below)
├── figs/                        # Output figures (PDF/PNG)
├── results/                     # LaTeX tables and validation outputs
├── docs/                        # Manuscript sections and appendices
├── renv.lock                    # Package lockfile for reproducibility
└── replication.Rproj            # RStudio/Positron project file
```

## Data

The raw and processed data files are **not committed to this repository**. They are available in the shared **Dropbox** folder. Download the `data/` directory from Dropbox and place it at the root of this repository before running any scripts.

## Reproducing the Analysis

### 1. Prerequisites

- R ≥ 4.4
- [`renv`](https://rstudio.github.io/renv/) for package management

### 2. Restore the R environment

Open the project (`replication.Rproj`) and run:

```r
renv::restore()
```

This installs all packages at the exact versions recorded in `renv.lock`.

### 3. Run the scripts

Execute the scripts in the following order:

| Step | Script | Description |
|------|--------|-------------|
| 1 | `scripts/candidates_wrangle.R` | Clean and merge raw TSE candidate data |
| 2 | `scripts/censo_wrangle.R` | Process IBGE census covariates |
| 3 | `scripts/contrib_matrix_pooled.R` | Build the candidate–donor contribution matrix |
| 4 | `scripts/candidates_cfscore_pooled.R` | Estimate pooled CFscores |
| 5 | `scripts/candidates_pooled_robustness.R` | Robustness checks for CFscore estimation |
| 6 | `scripts/create_state_level_ideology.R` | Aggregate ideology to the state level |
| 7 | `scripts/fix_defective_candidates.R` | Correct defective candidate records |
| 8 | `scripts/generate_valence_corruption.R` | Construct valence/corruption measures |
| 9 | `scripts/validate_cfscore.R` | Validate CFscores against survey benchmarks |
| 10 | `scripts/validate_cfscore_with_spoils.R` | Validate using spoils-based measures |
| 11 | `scripts/validate_contrib_matrix.R` | Validate the contribution matrix |
| 12 | `scripts/validate_unobserved_valence.R` | Validate unobserved valence measure |
| 13 | `scripts/candidates_diagnostics.R` | Diagnostics and summary statistics |
| 14 | `scripts/candidate_figs.R` | Generate all manuscript figures |

Helper functions used across scripts are defined in `scripts/functions.R` and `scripts/funs.R`.
