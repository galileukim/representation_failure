# Changelog

## `refactor` branch (October 2026)

Changes since `master`. Most of the data fixes below change files in `data/output/`, so rerun the pipeline from `01a_electoral_wrangle.R` onward to pick them up.

### Data fixes

**Coalition labels (2006 and 2010).** The raw TSE candidate and vote data lumped every party that ran alone in a state into one fake coalition (for example, 16 parties for federal deputy in SP in 2006). Coalition labels now come from the TSE party-level tallies (`party.csv.gz`), where each party belongs to exactly one list per office and state. The fix applies to all statewide offices, in both the candidate files and `election_{year}.csv`. With the new labels, a D'Hondt allocation reproduces all 513 federal deputy seats in 2002, 2010 and 2014, and 512 of 513 in 2006.

**Missing CPFs.**
- CPFs missing from the raw candidate data are recovered from the same person's records in other years, matched by electoral title. This recovers 22 of the 208 federal deputy candidates without a CPF in 2002.
- CPFs missing from the vote tallies are filled from the candidate data by ballot number and first name. This replaces `02_fix_defective_candidates.R`, which patched three named candidates in 2010 and 2014 by hand and had to be rerun after every run of `01a`. That script has been removed.
- CPFs and electoral titles are now read as text, so leading zeros are kept.

**Election outcomes.** Federal deputies' `outcome` and `elected` now agree with the vote tallies. The candidate data kept a pre-election status for some winners who were reinstated after the election (for example, Vinicius Gurgel and Rosinha da Adefal in 2010). It also flagged as elected a candidate who withdrew before the vote and was replaced under the same ballot number (Marcus Savius Teixeira Sousa, 2002).

**Candidate names.** Stray quotation marks are removed. For example, `""""carlos fernando d""""aguiar silva palacio""""` becomes `carlos fernando d'aguiar silva palacio`.

**Municipality codes.**
- Votes cast in Boa Esperança do Norte (MT) are now assigned its IBGE code (510183) instead of being dropped. This applies to candidate votes and to legend votes.
- One municipality missing its IBGE code in the vote counts (TSE code 91065) now gets code 150550.

**New column.** The candidate files have a new `flag_participated` column. It is `FALSE` for candidates whose registration was rejected, withdrawn or cancelled, or who died before the election.

### Known issues

- **ES 2006:** one federal deputy seat is not reproduced from our vote totals. The two lists involved finished 73 votes apart, and every candidate on both lists was approved ("deferido"). Our totals probably differ slightly from the TSE's final count.
- **DF district deputies, 2002–2010:** parties that ran in a coalition have no coalition label, because neither TSE source records it.
