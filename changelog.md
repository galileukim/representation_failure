# Changelog

## `refactor` branch (October 2026)

Changes since `master`. Most of the data fixes below change files in `data/output/`, so rerun the pipeline from `01a_electoral_wrangle.R` onward to pick them up.

### Data fixes

**Coalition labels (2006 and 2010).** The raw TSE candidate and vote data lumped every party that ran alone in a state into one fake coalition (for example, 16 parties for federal deputy in SP in 2006). Coalition labels now come from the TSE party-level tallies (`party.csv.gz`), where each party belongs to exactly one list per office and state. The fix applies to all statewide offices, in both the candidate files and `election_{year}.csv`. With the new labels, a D'Hondt allocation reproduces all 513 federal deputy seats in 2002, 2010 and 2014, and 512 of 513 in 2006.

**Missing CPFs.**
- CPFs missing from the raw candidate data are recovered from the same person's records in other years, matched by electoral title. This recovers 22 of the 208 federal deputy candidates without a CPF in 2002.
- CPFs missing from the vote tallies are filled from the candidate data by ballot number and first name. This replaces `02_fix_defective_candidates.R`, which patched three named candidates in 2010 and 2014 by hand and had to be rerun after every run of `01a`. That script has been removed.
- CPFs and electoral titles are now read as text, so leading zeros are kept.

**Outcomes, duplicates and incumbency.**
- **Outcome and candidate status.** For every office in the vote tallies (2002–2014), `outcome`, `candidate_status` and `elected` now come from the tallies (`election.csv.gz`) for candidates found in both datasets. Everyone else keeps the candidate data's values. The candidate data kept a pre-election status for some winners who were reinstated after the election (for example, Vinicius Gurgel and Rosinha da Adefal in 2010).
- **One record per CPF and election year.** About 4,600 CPFs appeared in more than one record in the same year: once per round, registered twice, running for two offices, or also running in a supplementary election. We keep one record, preferring the regular election, the candidacy in the vote tallies, candidates who participated or were elected, and the last round. The candidate files go from 1,358,159 to 1,353,882 records, and CPFs in `candidate_party_membership.csv.gz` are unique as well.
- **CPFs shared by different people.** About 400 CPFs belonged to two or more different people in the same year (different electoral titles, and different birthyears or first names). The CPF stays with the person who holds it in other election years and is set to missing for the others. When no one holds it in other years, about half the cases, it is set to missing for everyone.
- **Incumbency.** `incumbent` is now recomputed from the corrected `elected` column: a candidate is an incumbent if they were elected to the same office in the previous cycle (eight years earlier for the Senate). The raw data had no Senate incumbents and undercounted others; for example, city council incumbents in 2008 rise from 34,875 to 38,854. The 2002 elections, the 2004 municipal elections and the 2006 Senate race keep the raw labels, since the data has no earlier cycle for them.
- **Malformed CPFs and one hand-coded outcome.** 137 truncated CPFs (2004) and all-zero CPFs are now treated as missing, so they can be recovered from electoral titles like other missing CPFs. Marcus Savius Teixeira Sousa (federal deputy, 2002) withdrew and was replaced under the same ballot number, but is absent from the vote tallies. He is recoded by hand as withdrawn and not elected.

**Candidate names.** Stray quotation marks are removed. For example, `""""carlos fernando d""""aguiar silva palacio""""` becomes `carlos fernando d'aguiar silva palacio`.

**Municipality codes.**
- Votes cast in Boa Esperança do Norte (MT) are now assigned its IBGE code (510183) instead of being dropped. This applies to candidate votes and to legend votes.
- One municipality missing its IBGE code in the vote counts (TSE code 91065) now gets code 150550.

**New column.** The candidate files have a new `flag_participated` column. It is `FALSE` for candidates whose registration was rejected, withdrawn or cancelled, or who died before the election.

### Known issues

- **ES 2006:** one federal deputy seat is not reproduced from our vote totals. The two lists involved finished 73 votes apart, and every candidate on both lists was approved ("deferido"). Our totals probably differ slightly from the TSE's final count.
- **DF district deputies, 2002–2010:** parties that ran in a coalition have no coalition label, because neither TSE source records it.
- **Winners missing from the vote tallies:** about 56 candidates flagged as elected in the candidate data have no record in `election.csv.gz`, so they keep the candidate data's `outcome`, `candidate_status` and `elected`. 27 are 2004 city council members in Rio de Janeiro state, where the tallies seem to be missing whole municipalities. The rest are mayors and city council members in 2004, 2008 and 2012.
