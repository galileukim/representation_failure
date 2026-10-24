# CFscore alignment fix

*October 2026, `refactor` branch*

## Problem

`awm()` (`scripts/functions_bonica.R`) assumes that row *i* of the candidate table is the candidate in column *i* of the contribution matrix. It reads starting values from the table and writes final scores back to it by position. Nothing checked this, and in `05_candidates_cfscore_pooled.R` the two were in different orders. As a result, each candidate:

1. started from an unrelated candidate's party position, and
2. received an unrelated candidate's final score.

The two errors mirror each other. Twelve iterations do not move scores far from their starting values, so each row's score mostly restated the party prior it was given. Party averages therefore looked plausible (PT left, DEM right), but individual scores bore almost no relation to the candidate's donors.

**Affected:** `cfscore_pooled.RData`, the percentile robustness estimates in `06_candidates_pooled_robustness.R`, everything built from them, and very likely `cfscore_pooled_legacy.RData`.

## Fix

Both calling scripts and `awm()` itself now reorder the candidate table to match the matrix columns, and `awm()` stops if they disagree:

```r
cands <- cands[match(colnames(contrib_matrix), cands$cpf_candidate), ]
stopifnot(
  "every contrib. matrix column must match one candidate" =
    identical(cands$cpf_candidate, colnames(contrib_matrix))
)
```

`awm()` now gives the same result whatever order the candidates arrive in. A matrix column with no matching candidate produces `NA`, so the check fails and estimation stops.

## Evidence

Correctly attached scores reproduce themselves: recomputing each candidate's score from their own donors' averages should return the saved score. Because a candidate's own score feeds into their donors' averages, even randomly shuffled scores reach about 0.45 on this test.

| Check | Before | After |
|---|---|---|
| Table rows matching their matrix column | 1 / 283,059 | 283,059 / 283,059 |
| Correlation with donor-implied scores | 0.49 (shuffle: 0.45) | 1.00 |
| Variance explained by party (R²) | 0.06 | 0.26 |

After the fix, party averages correlate 0.87 with the Power and Rodrigues-Silveira (2018) survey positions across the 26 parties with at least 2,000 candidates.

## Caveats

- **Party agreement is partly built in.** Survey positions are the starting values, so the 0.87 correlation overstates independent validation. The donor test above is the check that does not depend on the prior.
- **Legacy file can't be verified exactly.** `cfscore_pooled_legacy.RData` (June 2026) used an older contribution matrix that no longer exists (43 more donors, 46 more candidates). It was built the same way and fails the donor test the same way (0.49). Its individual scores correlate only 0.07 with the misaligned new estimates, yet its party averages correlate 0.98 with them, which is what two runs that both restate the prior would produce.
- **Candidate sets differ slightly.** Legacy and new files share 283,050 CPFs: 55 appear only in legacy and 9 only in the new file, mostly 2004 and 2008 local candidates affected by this branch's CPF cleaning.
