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
- The fact-sheet line "India" (national total) is **dropped** — it is not a state
  (but it is used as the national verification anchor, below).
- Name variants are crosswalked to the boundary layer (normalised match plus two
  explicit overrides): "Andaman & Nicobar Islands" → "Andaman and Nicobar";
  "NCT Delhi" → "Delhi".
- All 36 states/UTs have non-missing values for both indicators. The app renders
  any state absent from the data as **grey (no data)** — never as zero.

## Verification performed (see `data-raw/verify_nfhs.R`)
The vendored file is a third-party transcription, so it is treated as an extract
to be validated, not as the primary source. `verify_nfhs.R` runs and passes:
1. **National anchor vs official.** The extract's India totals match the official
   NFHS-5 fact sheet exactly: women 15-49 anaemic **57.0%** (NFHS-4 53.1%),
   women with 10+ years schooling **41.0%** (NFHS-4 35.7%). The 53→57% anaemia
   rise is corroborated by the MoHFW compendium and independent reporting.
2. **Urban/rural/total consistency for all 36 states.** Each published state total
   lies within `[min(urban,rural), max(urban,rural)]` (a population-weighted blend
   cannot fall outside its parts) — a row-by-row transcription-error check.
3. **Completeness + range.** 36/36 states present, no NA, all values in `[0,100]`.
4. **Official spot-check.** The extreme value — Ladakh all-women anaemia
   **92.8%** (the highest in India) — matches the official Ladakh UT fact sheet.

The **definitive reference remains the official fact sheets** (compendium below);
this extract is validated for use as descriptive context, not offered as a
substitute for the official publication.

## Reuse rights
- The indicator values are **official Government of India statistics** published in
  the NFHS-5 fact sheets. The Open Government Data Platform India copy is released
  under NDSAP / the Government Open Data Licence – India (attribution required).
  Reproducing a small set of state-level aggregate percentages with attribution to
  NFHS-5 (IIPS/MoHFW) is within these terms. No microdata is used or redistributed.
- Cite as: IIPS & MoHFW, National Family Health Survey (NFHS-5), 2019-21, India.
- Official compendium (all states/UTs):
  Phase-II <https://dhsprogram.com/pubs/pdf/OF43/NFHS-5_India_and_State_Factsheet_Compendium_Phase-II.pdf>;
  India report FR375 <https://dhsprogram.com/pubs/pdf/FR375/FR375.pdf>.

## Limitations
- These are **descriptive regional context** measures. They are **not** industrial
  outcomes and must not be attributed to any industry, wage, or productivity effect.
- Preserve the survey round (NFHS-5, 2019-21) and state-boundary vintage when citing.
- If NFHS-6 final state indicators become available, re-run this build against that
  source and re-verify with `verify_nfhs.R`.
