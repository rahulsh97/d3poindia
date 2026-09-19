# Assessment: `data/emp_data.rda` in Git history

**Status:** removed from the current tree on `fix/honest-metrics-nfhs` (commit
`f52aadc`); still present in earlier history (introduced in `654c834`, present in
`origin/main`). **History was NOT rewritten** — this documents the finding and the
remedy for a separate, approved decision.

## What the file is
- 43,345 rows x 9 columns: `state_name, year, gender, age (banded), sector
  (rural/urban), education (banded), employment_type, state_norm, region`.
- A person-level extract derived from PLFS unit-level data (microdata.gov.in),
  filtered to employed persons. Wages and economic-status codes were dropped in
  pre-processing; what remains is broad categorical attributes only.

## Disclosure / re-identification risk: LOW
- No direct identifiers (no name, no household/person id), **no GPS/location beyond
  state**, no exact age (banded), no continuous quasi-identifiers.
- Cells defined by state x age-band x gender x rural/urban x education-band x
  employment-type contain many individuals; single-individual re-identification is
  not feasible. PLFS is itself a public-use microdata product.

## Licensing / redistribution: the real concern (UNRESOLVED)
- The records derive from MoSPI PLFS unit-level data, whose terms of use govern
  redistribution. Whether a derived **person-level** subset may be redistributed
  (e.g., committed to a public repo) is the open MoSPI-terms question flagged
  across this ecosystem. Independently of disclosure risk, the project's stated
  principle is that **person-level survey records should not be published on
  GitHub**. So the file should not remain in history.

## Remedy (do NOT run without explicit approval — history rewrite is destructive)
1. Confirm no branch still needs the file (only history retains it now).
2. On a fresh clone, purge it from all history:
   `git filter-repo --path data/emp_data.rda --invert-paths`
   (or BFG: `bfg --delete-files emp_data.rda`).
3. Force-push all refs (`git push --force --all` and `--tags`).
4. Have any collaborators re-clone; old clones/forks retain the blob until removed.
5. Note: GitHub may keep the blob reachable via cached views/forks until it
   garbage-collects; contact GitHub Support to purge if required.
6. Verify: `git log --all -- data/emp_data.rda` returns nothing.

## Recommendation
Not an emergency (low disclosure risk), but schedule the history purge to honour
the no-microdata principle and pre-empt the licensing question. Treat it as a
separate, coordinated, approved change — not part of this review pass.
