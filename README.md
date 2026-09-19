# BharatViz

BharatViz is a small Shiny application that maps **published, aggregate
state-level indicators for India** as descriptive regional context. It is the
India panel of the **Growth & Work Atlas** — a research question about whether
industries that produce more value per worker also deliver better pay and jobs,
and where those gains fail to reach people.

> **This India panel is descriptive context, not an industrial outcome.** The
> health/education indicators shown here are **not** joined to any industry, wage
> or productivity measure, and co-location is descriptive, never causal.

## What it shows (v0.2)
- A **choropleth** of one real NFHS-5 (2019-21) state indicator at a time:
  - *Women age 15-49 who are anaemic (%)*
  - *Women with 10+ years of schooling (%)*
- A **ranked view** of the same values (legible without colour, keyboard/text
  accessible).
- An explicit **legend** (unit, survey round, source, "grey = no data — never
  zero", coverage count, and whether higher is a better/worse outcome).
- A **methods panel** (indicator, universe, denominator, missing rule, boundary
  provenance, non-causal note) and a **CSV download** of the plotted aggregate.

## What changed from the previous version (correction)
The previous app mapped `count(filtered PLFS records in a state) / count(all PLFS
records in that state)` and labelled it a "proportion", titled the page
"Expenditure and Manufacturing Maps of India", and credited ASI. That number was
a **share of unweighted sample records**, not a population rate, wage, or
industrial outcome, and the shipped PLFS extract has **no survey weights**, so no
valid population rate could be computed from it. This version:
- **removes** the misleading sample-share metric and the person-level PLFS
  microdata from the package (microdata should not ship on GitHub);
- **replaces** it with a real, cited NFHS-5 state indicator layer;
- **replaces** the boundary layer with the complete-outline SimpleMaps geometry
  (CC BY 4.0) instead of the previous incomplete outline;
- corrects the title, footer and source credits.

## Data & sources
- **Indicators:** NFHS-5 (2019-21) state fact sheets, IIPS/MoHFW; open re-use via
  the Open Government Data Platform India (NDSAP). See
  [`data-raw/NFHS_SOURCE.md`](data-raw/NFHS_SOURCE.md). Aggregate only — **no
  microdata**.
- **Boundaries:** SimpleMaps India state layer, CC BY 4.0. See
  [`data-raw/MAP_SOURCE.md`](data-raw/MAP_SOURCE.md). A boundary depiction here is
  not an official Government of India map.
- Package data is rebuilt reproducibly by
  [`data-raw/build_data.R`](data-raw/build_data.R).

## Run locally
```r
# from the package root, with the framework packages installed
pkgload::load_all(".")
d3poindia::run_app()
```
The app installs from GitHub for deployment; see `deploy.R` (deploys to
shinyapps.io, not GitHub Pages).

## Develop / validate
```r
Rscript data-raw/build_india_map.R            # (network) rebuild the boundary RDS from SimpleMaps
Rscript data-raw/build_data.R                 # rebuild data/*.rda from vendored sources
Rscript data-raw/verify_nfhs.R                # validate the NFHS extract (national anchor + all-36 checks)
Rscript -e 'devtools::test()'                 # data-integrity + missing-value output tests
R CMD build . && R CMD check *.tar.gz --no-manual
```

See also: [`data-raw/NFHS_SOURCE.md`](data-raw/NFHS_SOURCE.md) (verification &
reuse rights), [`data-raw/MAP_SOURCE.md`](data-raw/MAP_SOURCE.md) (boundary
provenance + checksum), [`docs/BOUNDARY_CHECK.md`](docs/BOUNDARY_CHECK.md)
(Kashmir/northern outline check), and
[`docs/DATA_DISCLOSURE_ASSESSMENT.md`](docs/DATA_DISCLOSURE_ASSESSMENT.md)
(prior PLFS extract in history — finding + remedy).

## Limitations
- Descriptive regional context only; not causal, not an industrial outcome.
- NFHS-5 round; preserve the survey round and boundary vintage when citing.
- The **global industry module** (productivity → pay → jobs, and the accounting
  bridge) is **not yet shipped** — see
  [`docs/GLOBAL_MODULE_PLAN.md`](docs/GLOBAL_MODULE_PLAN.md).

## Licence
Apache-2.0 (code). Data licences are per source above.
