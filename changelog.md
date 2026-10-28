# Changelog

## `refactor` branch (October 2026)

This branch cleans up the TSE candidate and election data that every later script builds on. Most of the fixes change the files in `data/output/`, so rerun the pipeline from `01a_electoral_wrangle.R` onward before using the new numbers, using the `00_main.R` script.

Many of the fixes come down to how two TSE sources disagree, so here is a quick orientation:

- The **candidate data** (`candidate.csv.gz`) has one row per candidacy, with each candidate's background and registration details.
- The **election data** (`election.csv.gz`) has the vote tallies, with one row per candidate and municipality.

When the two disagree about what happened on election day, we now trust the election data.

### Overview

- **Coalitions:** coalition labels now come from the TSE party-level tallies. This removes the inconsistent coalitions in 2006 and 2010 and reproduces the official allocation of federal deputy seats.
- **Election results:** outcomes, candidate status and `elected` now come from the election data for every candidate who appears in both sources.
- **One record per candidate:** each CPF now appears at most once per election year. CPFs that were wrongly shared by different people are given to their owner or set to missing.
- **Incumbency:** `incumbent` is rebuilt from the corrected results of the previous election.
- **Identifiers and names:** missing and malformed CPFs are recovered where possible, stray quotation marks are removed from names, and missing municipality codes are filled in.
- **CFscores:** scores are now attached to the right candidates. Previously almost every candidate got another candidate's score.

### What changed, and why

#### Coalition labels now match the official seat count

In 2006 and 2010, the raw TSE data put every party that ran alone in a state into a single made-up coalition. In São Paulo in 2006, for example, 16 unrelated parties appear as one federal deputy list. Seats are allocated by list, so this distorts any analysis of electoral competition.

Coalition labels now come from the TSE party-level tallies (`party.csv.gz`), where each party belongs to exactly one list per office and state. As a check, allocating seats with the official D'Hondt formula and the new labels reproduces all 513 federal deputy seats in 2002, 2010 and 2014, and 512 of 513 in 2006 (see Known issues). The fix covers all statewide offices, in both the candidate files and `election_{year}.csv`.

Parties missing from the tallies keep the candidate data's label. Where that label is blank, a party registered as running alone (*partido isolado*) is labeled with its own name. A party in a named coalition gets the list of parties whose candidates registered under that name in the same state, or municipality for local offices. For example, the six parties of *Unidos por Goiás* (GO, 2014), whose registration was denied, are now labeled `pen / phs / pmn / psl / ptc / pv`. This fills the blank labels of Federal District district deputies in 2002–2010 and of most local candidates in 2004 and 2008. About 21,700 local candidates in 2008 and a few dozen others stay blank, because the TSE records neither a coalition type nor a coalition name for them. A coalition rebuilt this way lists only the parties that fielded candidates under that name.

#### Election results come from the election data

The candidate data sometimes records a candidate's status from before the election and never updates it. For example, Vinicius Gurgel and Rosinha da Adefal (2010) won seats after their registrations were reinstated, but the candidate data still shows their earlier status.

For every candidate who also appears in the election data, `outcome`, `candidate_status` and `elected` now come from the election data. Candidates who never reached the ballot (registration rejected or withdrawn) don't appear there, and neither do running mates or Senate substitutes. All of these keep the candidate data's values.

Matching the two sources is harder than it sounds, because a candidate who withdraws passes their ballot number to a substitute. The script therefore matches on both the ballot number and the person (CPF, electoral title or name). It also keeps supplementary elections separate from regular ones (see the next section).

#### One record per candidate and election year

The same CPF used to appear more than once in the same election year, about 4,600 times in total. Most of these repeats have ordinary explanations:

- governor and mayor races list each candidate once per round;
- some people were registered twice for the same race;
- some ran for two offices, often after withdrawing as someone's running mate;
- some also ran in a supplementary election, a new election held when a result is annulled, which the TSE files under the original year.

Left as they were, these repeats would count the same person several times in any candidate-level analysis. We now keep one record per person and year: the one that best reflects the election that actually took place. In order of preference, that means:

1. a regular election over a supplementary one;
2. a candidacy that appears in the vote tallies;
3. a candidate who actually ran;
4. a candidate who won;
5. the last round.

The candidate files go from 1,358,159 to 1,353,874 records. The party membership file (`candidate_party_membership.csv.gz`) is built from the deduplicated data, so its CPFs are unique by year too.

#### When two people share a CPF

In about 400 cases, the same CPF belongs to clearly different people in the same year: their electoral titles differ, and so do their birth years or first names. For example, in 2004 Elza Alves Pereira and Maria Marina Silva, from different municipalities, share a CPF. These are data-entry errors in the TSE files, and nothing in the data says for sure whose CPF it is.

If one of the people uses that CPF in other election years, we give it to them and set it to missing for the others. Failing that, if the same year's vote tallies pair the CPF with one of them (by electoral title), it goes to that person. For example, Zairton Cavalcante Santos (CE, 2006) shares his CPF with a governor candidate who withdrew, but only he appears in the tallies. If neither rule applies, we set the CPF to missing for everyone rather than guess. CPFs set to missing in the candidate data are set to missing in `election_{year}.csv` too, so every CPF in the vote tallies has a candidate record. These candidates stay in the data; they just can't be linked to other years or datasets by CPF.

#### Incumbency is rebuilt from election results

The raw `incumbent` flag is unreliable. It marks no Senate incumbents at all, and it misses many others: it counts 34,875 city council incumbents in 2008, against 38,902 by our count.

An incumbent is now someone who was elected to the same office in the previous election, according to the corrected `elected` column. For the Senate, the previous election means eight years earlier, since senators serve eight-year terms. Holding a different office doesn't count, so a federal deputy running for governor is not an incumbent.

The raw flag stays where the data has no earlier election to look back to: 2002, the 2004 municipal elections and the 2006 Senate race. It also stays for candidates without a CPF.

#### Filling in missing CPFs

- **From the candidate's other years.** Some candidates have no CPF in the candidate data. We look for the same person in other years, matching on electoral title and exact name, since an electoral title alone is occasionally shared. This recovers 22 of the 208 federal deputy candidates without a CPF in 2002.
- **Broken CPFs.** 137 truncated CPFs (2004) and 23 all-zero CPFs are now treated as missing, so the same step can recover them.
- **CPFs missing from the vote tallies** are filled in from the candidate data, using the ballot number and first name. This replaces `02_fix_defective_candidates.R`, which patched three candidates by hand and had to be rerun after every run of `01a`. That script has been removed.
- **Leading zeros.** CPFs and electoral titles are now read as text, so leading zeros are no longer lost.

#### CFscores are attached to the right candidates

The estimation function `awm()` matched scores to candidates by position, assuming that row *i* of the candidate table was the candidate in column *i* of the contribution matrix. The two were in different orders, so almost every candidate got the starting value and the final score of an unrelated candidate. Party averages still looked plausible, because each row's score largely reflected the party position it started with.

The candidate table is now put in the order of the matrix columns, both in `05` and `06` and inside `awm()`, which also stops with an error if any column has no matching candidate. This affects all CFscore outputs, including `cfscore_pooled_legacy.RData`. See [`docs/cfscore_alignment_fix.md`](docs/cfscore_alignment_fix.md) for the details and checks.

#### Smaller fixes

- **Names:** stray quotation marks are removed. For example, `""""carlos fernando d""""aguiar silva palacio""""` becomes `carlos fernando d'aguiar silva palacio`.
- **Municipality codes:** votes cast in Boa Esperança do Norte (MT) now get IBGE code 510183 instead of being dropped, for both candidate and legend votes. In the vote counts, the municipality with TSE code 91065 now gets IBGE code 150550.
- **One hand-coded outcome:** Marcus Savius Teixeira Sousa (federal deputy, 2002) withdrew and his ballot number went to a substitute. He is missing from the vote tallies, but the candidate data lists him as elected, so we recode him by hand as withdrawn and not elected.
- **New `flag_participated` column:** `FALSE` for candidates whose registration was rejected, withdrawn or cancelled, or who died before the election. It is based on `candidate_status`, so it uses the corrected status.

### Known issues

- **Espírito Santo, 2006:** one federal deputy seat doesn't come out of our vote totals. The two lists involved finished 73 votes apart, and every candidate on both lists was approved ("deferido"), so our totals probably differ slightly from the TSE's final count.
- **Federal District district deputies, 2006–2010:** coalition labels are rebuilt from the coalition names in the candidate data, because neither TSE source lists the member parties. They can't be checked against the seat allocation.
- **Winners missing from the vote tallies:** 56 candidates whom the candidate data lists as elected have no record in the election data, so they keep the candidate data's values. 27 are 2004 city council members in Rio de Janeiro state, where the vote tallies seem to be missing entire municipalities. The rest are mayors and city council members in 2004, 2008 and 2012.
