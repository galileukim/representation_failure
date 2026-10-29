# Changelog

## `refactor` (October 2026)

This refactirubg cleans up the TSE candidate and election data that every later script builds on. Most of the fixes change the files in `data/output/`, so rerun the pipeline from `01a_electoral_wrangle.R` onward before using the new numbers, using the `00_main.R` script.

Many of the fixes come down to how two TSE sources disagree, so here is a quick orientation:

- The **candidate data** (`candidate.csv.gz`) has one row per candidacy, with each candidate's background and registration details.
- The **election data** (`election.csv.gz`) has the vote votes, with one row per candidate and municipality.

When the two disagree about what happened on election day, we now trust the election data.

### Overview

- **Coalitions:** coalition labels now come from the TSE party-level votes. This removes the inconsistent coalitions in 2006 and 2010 and reproduces the official allocation of federal deputy seats.
- **Election results:** outcomes, candidate status and `elected` now come from the election data for every candidate who appears in both sources.
- **One record per candidate:** each CPF now appears at most once per election year. CPFs that were wrongly shared by different people are given to their owner or set to missing.
- **Incumbency:** `incumbent` is rebuilt from the corrected results of the previous election.
- **Identifiers and names:** missing and malformed CPFs are recovered where possible, stray quotation marks are removed from names, and missing municipality codes are filled in.
- **CFscores:** scores are now attached to the right candidates. Previously almost every candidate got another candidate's score.

### What changed

#### Coalition labels now match the official seat count

In 2006 and 2010, the raw TSE data put every party that ran alone in a state into a single coalition. In São Paulo in 2006, for example, 16 unrelated parties appear as one federal deputy list. Seats are allocated by list, so this distorts any analysis of electoral competition.

Coalition labels now come from the TSE party-level votes (`party.csv.gz`), where each party belongs to exactly one list per office and electoral unit: the state for federal and state offices, and the municipality for mayor and city council. As a check, allocating seats with the official D'Hondt formula and the new labels reproduces all 513 federal deputy seats in 2002, 2010 and 2014, and 512 of 513 in 2006 (see Known issues). Municipal labels now agree with the party-level votes for every candidate the votes cover; before, between 0.1% and 16% disagreed, depending on the year and office (e.g. 7% of 2004 mayor labels and 16% of 2008 city council labels). The fix applies to the candidate files and to `election_{year}.csv`.

Parties missing from the votes keep the candidate data's label, except that a party registered as running alone (*partido isolado*) gets its own name. Where the label is blank, a party in a named coalition gets the list of parties whose candidates registered under that name in the same state, or municipality for local offices. For example, the six parties of *Unidos por Goiás* (GO, 2014), whose registration was denied, are now labeled `pen / phs / pmn / psl / ptc / pv`. This fills the blank labels of Federal District district deputies in 2002–2010. A coalition rebuilt this way lists only the parties that fielded candidates under that name. About 2,100 local candidates in 2008 and a few dozen others stay blank, because the TSE records neither a coalition type nor a coalition name for them.

#### Election results come from the election data

The candidate data sometimes records a candidate's status from before the election and never updates it. For example, Vinicius Gurgel and Rosinha da Adefal (2010) won seats after their registrations were reinstated, but the candidate data still shows their earlier status.

For every candidate who also appears in the election data, `outcome`, `candidate_status` and `elected` now come from the election data. Candidates who never reached the ballot (registration rejected or withdrawn) don't appear there, and neither do running mates or Senate substitutes. All of these keep the candidate data's values.

Matching the two sources is harder than it sounds, because a candidate who withdraws passes their ballot number to a substitute. The script therefore matches on both the ballot number and the person: by CPF, or else by exact name. It also keeps supplementary elections separate from regular ones (see the next section).

#### One record per candidate and election year

The same CPF used to appear more than once in the same election year, about 4,600 times in total. Most of these repeats have ordinary explanations:

- governor and mayor races list each candidate once per round;
- some people were registered twice for the same race;
- some ran for two offices, often after withdrawing as someone's running mate;
- some also ran in a supplementary election, a new election held when a result is annulled, which the TSE files under the original year.

Left as they were, these repeats would count the same person several times in any candidate-level analysis. We now keep one record per person and year: the one that best reflects the election that actually took place. In order of preference, that means:

1. a regular election over a supplementary one;
2. a candidacy that appears in the vote votes;
3. a candidate who actually ran;
4. a candidate who won;
5. the last round.

The candidate files go from 1,358,159 to 1,353,877 records. The party membership file (`candidate_party_membership.csv.gz`) is built from the deduplicated data, so its CPFs are unique by year too.

#### When two people share a CPF

In about 400 cases, the same CPF belongs to clearly different people in the same year: their electoral titles differ, and so do their birth years or names. For example, in 2004 Elza Alves Pereira and Maria Marina Silva, from different municipalities, share a CPF. These are data-entry errors in the TSE files, and nothing in the data says for sure whose CPF it is.

The CPF goes to the person it is paired with (by electoral title) in the most election years, counting both the candidate data and the vote votes, and is set to missing for the others. For example, Zairton Cavalcante Santos (CE, 2006) shares his CPF with a governor candidate who withdrew, but only he also appears with it in the votes. In case of a tie, we set the CPF to missing for everyone rather than guess. CPFs set to missing in the candidate data are set to missing in `election_{year}.csv` too, so every CPF in the vote votes has a candidate record. These candidates stay in the data; they just can't be linked to other years or datasets by CPF.

#### Birth year and gender across elections

The TSE records birth year and gender anew for each candidacy, and the same CPF's records often disagree from one election to the next. 3,276 CPFs have the same name but birth years more than 2 years apart, and 612 have different genders. Among federal deputy candidates in 2006–2014, the counts are 132 and 23.

We keep each record's birth year and gender as the TSE recorded them for that election, rather than overwriting them with values from other years. Within an election, each CPF appears once in the candidate data, and `age` in `election_{year}.csv` now comes from that record. Previously it came from the vote votes' own birth year, which disagreed with the candidate data for three CPFs in 2010 and 2014.

A new column, `flag_ambiguous`, marks the CPFs whose records may belong to different people: the names differ across years, and so do the birth years (by more than 2 years) or the genders. For example, João Espírito Santo (2004) and Elena Fregona Espírito Santo (2008) share a CPF. The flag covers 619 CPFs (1,487 records), mostly in the local files; 22 of them include a federal deputy candidacy in 2006–2014. These records are kept and can still be linked across years by CPF, so analyses that follow candidates over time may want to drop or check them.

#### Incumbency is rebuilt from election results

The raw `incumbent` flag is unreliable. It marks no Senate incumbents at all, and it misses many others: it counts 34,875 city council incumbents in 2008, against 38,902 by our count.

An incumbent is now someone who was elected to the same office in the previous election, according to the corrected `elected` column. For the Senate, the previous election means eight years earlier, since senators serve eight-year terms. Holding a different office doesn't count, so a federal deputy running for governor is not an incumbent.

The raw flag stays where the data has no earlier election to look back to: 2002, the 2004 municipal elections and the 2006 Senate race. It also stays for candidates without a CPF.

#### Filling in missing CPFs

- **From the candidate's other years.** Some candidates have no CPF in the candidate data. We look for the same person in other years, matching on electoral title and exact name, since an electoral title alone is occasionally shared. This recovers 22 of the 208 federal deputy candidates without a CPF in 2002.
- **Broken CPFs.** 137 truncated CPFs (2004) and 23 all-zero CPFs are now treated as missing, so the same step can recover them.
- **CPFs missing from the vote votes** are filled in from the candidate data by ballot number, among the candidates who actually ran (a withdrawn candidate shares the ballot number with their substitute). Names aren't used, since they are sometimes spelled differently in the two sources (e.g. João Marcelo Santos Sousa/Souza, 2014). This replaces `02_fix_defective_candidates.R`, which patched three candidates by hand and had to be rerun after every run of `01a`. That script has been removed.
- **Leading zeros.** CPFs and electoral titles are now read as text, so leading zeros are no longer lost.

#### CFscores are attached to the right candidates

The estimation function `awm()` matched scores to candidates by position, assuming that row *i* of the candidate table was the candidate in column *i* of the contribution matrix. The two were in different orders, so candidates got the starting value and the final score of an unrelated candidate. Party averages still looked plausible, because each row's score largely reflected the party position it started with.

The candidate table is now put in the order of the matrix columns, both in `05` and `06` and inside `awm()`, which also stops with an error if any column has no matching candidate. This affects all CFscore outputs, including `cfscore_pooled_legacy.RData`. See [`docs/cfscore_alignment_fix.md`](docs/cfscore_alignment_fix.md) for the details and checks.

#### Smaller fixes

- **Names:** stray quotation marks are removed, in both the candidate data and the vote votes. For example, `""""carlos fernando d""""aguiar silva palacio""""` becomes `carlos fernando d'aguiar silva palacio`. Every match on names uses the exact name.
- **Electoral titles:** placeholder titles (`#ni#`, `-1`) are now blank.
- **Municipality codes:** votes cast in Boa Esperança do Norte (MT, TSE code 91065) now get IBGE code 510183 instead of being dropped, in the candidate votes, legend votes and vote counts alike.
- **One hand-coded outcome:** Marcus Savius Teixeira Sousa (federal deputy, 2002) withdrew and his ballot number went to a substitute. He is missing from the vote votes, but the candidate data lists him as elected, so we recode him by hand as withdrawn and not elected.
- **State-level ideology file:** `07_create_state_level_ideology.R` now builds `candidate_ideology_only_state.csv` from the cleaned candidate data rather than the raw TSE file. It therefore picks up the corrected CPFs, outcomes and incumbency and keeps one record per CPF and year, so the file drops from 45,731 to 45,423 rows. It also computes `age` from the birth year instead of trusting the raw age field, which was wrong in some cases: Maria Lizete Rendeiro da Conceição Baia (PA, 2010) was listed as 7 years old with a 1903 birth year. The `edu_desc` column is gone, because nothing downstream used it.
- **Birth municipality code dropped:** `mun_birth_code` is removed from the candidate files. The TSE value is −1 or −3 ("not informed") in every year. `candidate_federal_state.csv` keeps `cod_ibge_6_birth`, the IBGE code matched from the birth municipality and state names.
- **Party membership columns renamed:** in `candidate_party_membership.csv.gz`, `cod_ibge_6_party_membership` now holds the IBGE code of the municipality where membership was recorded, and `mun_name` holds its name. Previously `cod_ibge_6_party_membership` held the name.
- **Donor counts for CFscores:** `candidate_ideology_pooled_{fed_state,local}.csv` now include `n_donors` and `total_contrib`, the number of distinct donors and the total contributions behind each CFscore. Both cover the donors in the estimation matrix: individuals who gave to the candidate and to at least one other candidate, in any year. About half of the candidates (135,876 of 283,059) have a single donor, so their CFscore rests on one donor's giving.
- **Pooled contribution matrix has individual donors only:** `04_contrib_matrix_pooled.R` now drops company donors (anything other than an 11-digit CPF) when it builds the matrix, the same rule as the contribution summaries. `05` and `06` used to apply this filter themselves, so the CFscores don't change.
- **Contribution summaries count individual donors only:** in `contribution_by_candidate_{fed_state,local}.csv`, `total_contrib` and `total_contributors` are now `contrib_individual` and `n_individual_donors`, and company donors are no longer counted. Projected contributions (from parties, other candidates and the candidate's own funds) stay excluded. These are per-year figures, so they differ from `total_contrib` and `n_donors` in the CFscore files, which are pooled across years and count only donors who also gave to another candidate.
- **New `flag_participated` column:** `FALSE` for candidates whose registration was rejected, withdrawn or cancelled, or who died before the election. It is based on `candidate_status`, so it uses the corrected status.

### Known issues

- **Espírito Santo, 2006:** one federal deputy seat doesn't come out of our vote totals. The two lists involved finished 73 votes apart, and every candidate on both lists was approved ("deferido"), so our totals probably differ slightly from the TSE's final count.
- **Federal District district deputies, 2006–2010:** coalition labels are rebuilt from the coalition names in the candidate data, because neither TSE source lists the member parties. They can't be checked against the seat allocation.
- **Implausible birth years and gender codes:** these come directly from the raw candidate data, and we leave them as they are. Some of these people have a plausible value in another year's record, but we don't recover it.
  - 158 candidacies have a birth year that puts their age outside 18–100: 150 in the local files (range 5–112) and 8 in `candidate_federal_state.csv`. Examples are Deodata Pereira Borges and José de Souza Pinto (BA, 2006), born in 1905 and 1904, and Nelson Luzia Oliveira (GO, 2006), born in 1999 in that year's record but in 1959 in his 2002 record.
  - Gender code 0 appears for 208 local candidates in 2004 and 177 federal and state candidates in 2002.
- **No CPFs in the legislative ideology data:** `legislative_ideology.csv` (Zucco and Lauderdale 2011 roll-call ideal points) can only be linked to candidates by name, because the source data doesn't include CPFs.
- **Winners missing from the vote votes:** 56 candidates whom the candidate data lists as elected have no record in the election data, so they keep the candidate data's values. 27 are 2004 city council members in Rio de Janeiro state, where the vote votes seem to be missing entire municipalities. The rest are mayors and city council members in 2004, 2008 and 2012.
