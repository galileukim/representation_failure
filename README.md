# Representation Failure — Replication Repository

## Overview

This repository contains the replication code for the paper on representation failure and candidate ideology in Brazilian elections. The analysis estimates candidate ideal points (CFscores) using campaign contribution data and validates them against survey-based measures.

## Directory Structure

```
representation_failure/
├── data/
│   ├── input/                       # Raw input data — not committed (see Data section)
│   │   ├── tse/                     # Raw TSE electoral files (candidates, parties, members)
│   │   ├── municipal/               # Municipal shapefiles, IBGE identifiers, state IDs
│   │   ├── contribution/            # Campaign finance files (campaign_fed_state.csv.gz, etc.)
│   │   ├── ideology/                # Legislative survey ideology data, Latinobarometer
│   │   ├── corruption/              # TSE cassation files and 2018 candidate list
│   │   ├── candidate/missing/       # Manually corrected missing candidate records
│   │   ├── spoils_of_victory/       # Replication data from Brollo et al. (2013)
│   │   ├── state/                   # State-level census data
│   │   └── identifiers/             # State name crosswalks
│   └── output/                      # All script-generated outputs (not committed)
│       ├── candidate/
│       │   ├── cfscore_estimation/  # Cleaned candidate files used in cfscore estimation
│       │   ├── fed_state/           # Federal and state legislative candidates by year
│       │   └── local/               # Municipal-level candidates by year
│       ├── election/                # Cleaned election, vote count, and coalition files
│       ├── ideology/                # Contribution matrices, cfscore estimates, legislative ideology
│       ├── municipal/               # Processed IBGE municipal dataset
│       └── corruption/              # Constructed corruption/valence indicators
├── scripts/                     # R scripts (numbered workflow below)
├── figs/                        # Output figures (PDF/PNG)
├── results/                     # LaTeX tables and validation outputs
├── docs/                        # Manuscript sections and appendices
├── renv.lock                    # Package lockfile for reproducibility
└── replication.Rproj            # RStudio/Positron project file
```

## Data

All data files are **not committed to this repository**. They are available in the shared **Dropbox** folder. Download both the `data/input/` and `data/output/` directories from Dropbox and place them under `data/` at the root of this repository before running any scripts.

> If you wish to reproduce outputs from scratch, you only need `data/input/`. The `data/output/` directory will be populated as you run the scripts in order.

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
| 1 | `scripts/candidates_wrangle.R` | Clean raw TSE candidate/election data → `data/output/candidate/`, `data/output/election/` |
| 2 | `scripts/censo_wrangle.R` | Process IBGE census covariates → `data/output/municipal/` |
| 3 | `scripts/contrib_matrix_pooled.R` | Build candidate–donor contribution matrix → `data/output/ideology/` |
| 4 | `scripts/candidates_cfscore_pooled.R` | Estimate pooled CFscores → `data/output/ideology/` |
| 5 | `scripts/candidates_pooled_robustness.R` | Robustness checks for CFscore estimation → `data/output/ideology/` |
| 6 | `scripts/create_state_level_ideology.R` | Aggregate ideology to state level → `data/output/ideology/` |
| 7 | `scripts/fix_defective_candidates.R` | Correct defective candidate records in `data/output/election/` |
| 8 | `scripts/generate_valence_corruption.R` | Construct valence/corruption measures → `data/output/corruption/` |
| 9 | `scripts/validate_cfscore.R` | Validate CFscores against survey benchmarks |
| 10 | `scripts/validate_cfscore_with_spoils.R` | Validate using spoils-based measures |
| 11 | `scripts/validate_contrib_matrix.R` | Validate the contribution matrix |
| 12 | `scripts/validate_unobserved_valence.R` | Validate unobserved valence measure → `data/output/corruption/` |
| 13 | `scripts/candidates_diagnostics.R` | Diagnostics and summary statistics |
| 14 | `scripts/candidate_figs.R` | Generate all manuscript figures → `figs/` |

Helper functions used across scripts are defined in `scripts/functions.R` and `scripts/funs.R`.
