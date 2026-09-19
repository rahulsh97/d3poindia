# NFHS-5 state indicators: source and provenance

`data/nfhs_state.rda` holds a small set of **aggregate** state-level indicators from
India's National Family Health Survey, Round 5 (NFHS-5, 2019-21). It contains **no
microdata and no person-level records** — only published state percentages.

## Source
- **Survey:** National Family Health Survey 2019-21 (NFHS-5), conducted by the
  International Institute for Population Sciences (IIPS) for the Ministry of Health
  and Family Welfare (MoHFW), Government of India.
- **Published fact sheets:** state/UT fact sheets, IIPS/MoHFW
  (<https://www.nfhsiips.in/nfhsuser/nfhs5.php>, <http://rchiips.org/nfhs/>).
- **Open re-use copy:** "All India and State/UT-wise Factsheets of NFHS-5" on the
  Open Government Data (OGD) Platform India, released under the National Data Sharing
  and Accessibility Policy (NDSAP) / Government Open Data Licence – India
  (<https://www.data.gov.in/resource/all-india-and-stateut-wise-factsheets-national-family-health-survey-nfhs-5-2019-2021>).
- **Machine-readable compilation used for this build:** `pratapvardhan/NFHS-5`
  (`NFHS-5-States.csv`), a CSV transcription of the official fact sheets citing
  `rchiips.org/nfhs` (vendored as `data-raw/NFHS-5-States.csv`, retrieved 2026-09-19).

## Indicators included (state total, NFHS-5 column `nfhs5_total`)
| id | source fact-sheet line | universe | unit |
|---|---|---|---|
| `women_anaemia` | "All women age 15-49 years who are anaemic (%)" | all women 15-49 | % |
| `women_schooling10` | "Women with 10 or more years of schooling (%)" | women 15-49 | % |

## Handling
- The fact-sheet line "India" (national total) is **dropped** — it is not a state.
- Two name variants are crosswalked to the boundary layer: "Andaman & Nicobar
  Islands" → "Andaman and Nicobar"; "NCT Delhi" → "Delhi".
- All 36 states/UTs have non-missing values for both indicators. The app still
  renders any state absent from the data as **grey (no data)** — never as zero.

## Limitations
- These are **descriptive regional context** measures. They are **not** industrial
  outcomes and must not be attributed to any industry, wage, or productivity effect.
- Preserve the survey round (NFHS-5, 2019-21) and state-boundary vintage when citing.
- Verify against the official fact sheets before any publication; if NFHS-6 final
  state indicators become available, re-run this build against that source.
