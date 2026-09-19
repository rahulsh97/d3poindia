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

## Verification performed
The vendored file is a third-party transcription (DOI-backed: Harvard Dataverse
10.7910/DVN/42WNZF, citing rchiips.org/nfhs). Its plotted values have now been
compared, for **all 36 states/UTs and both indicators**, against the **official
NFHS-5 fact sheets** in the MoHFW/IIPS compendia (Phase I = `FS_P1.pdf`,
Phase II = `FS_P2.pdf`; the state total is the NFHS-5 "Total" column).

**Result: 0 mismatches** across 36 states x 2 indicators (tolerance 0.05pp). Every
value used by the app equals the official fact-sheet total. National anchor also
matches (anaemia 57.0 pct, schooling 41.0 pct). Page references (1-based) below.

Supporting internal checks (`data-raw/verify_nfhs.R`, all pass): urban/rural/total
consistency and completeness/range for all 36 states.

### Fact-sheet reconciliation (all 36; page refs)
| State/UT | Anaemia % | src | Schooling 10+ % | src | Match |
|---|--:|---|--:|---|:--:|
| Andaman and Nicobar | 57.5 | FS_P1 p.13 | 52.5 | FS_P1 p.11 | OK |
| Andhra Pradesh | 58.8 | FS_P1 p.19 | 39.6 | FS_P1 p.17 | OK |
| Arunachal Pradesh | 40.3 | FS_P2 p.15 | 39.4 | FS_P2 p.13 | OK |
| Assam | 65.9 | FS_P1 p.25 | 29.6 | FS_P1 p.23 | OK |
| Bihar | 63.5 | FS_P1 p.31 | 28.8 | FS_P1 p.29 | OK |
| Chandigarh | 60.3 | FS_P2 p.81 | 59.6 | FS_P2 p.79 | OK |
| Chhattisgarh | 60.8 | FS_P2 p.21 | 36.9 | FS_P2 p.19 | OK |
| Dadra and Nagar Haveli and Daman and Diu | 62.5 | FS_P1 p.37 | 35.8 | FS_P1 p.35 | OK |
| Delhi | 49.9 | FS_P2 p.87 | 59.7 | FS_P2 p.85 | OK |
| Goa | 39.0 | FS_P1 p.43 | 71.5 | FS_P1 p.41 | OK |
| Gujarat | 65.0 | FS_P1 p.49 | 33.8 | FS_P1 p.47 | OK |
| Haryana | 60.4 | FS_P2 p.27 | 49.5 | FS_P2 p.25 | OK |
| Himachal Pradesh | 53.0 | FS_P1 p.55 | 65.9 | FS_P1 p.53 | OK |
| Jammu and Kashmir | 65.9 | FS_P1 p.61 | 51.3 | FS_P1 p.59 | OK |
| Jharkhand | 65.3 | FS_P2 p.33 | 33.2 | FS_P2 p.31 | OK |
| Karnataka | 47.8 | FS_P1 p.67 | 50.2 | FS_P1 p.65 | OK |
| Kerala | 36.3 | FS_P1 p.73 | 77.0 | FS_P1 p.71 | OK |
| Ladakh | 92.8 | FS_P1 p.85 | 50.0 | FS_P1 p.83 | OK |
| Lakshadweep | 25.8 | FS_P1 p.79 | 67.8 | FS_P1 p.77 | OK |
| Madhya Pradesh | 54.7 | FS_P2 p.39 | 29.3 | FS_P2 p.37 | OK |
| Maharashtra | 54.2 | FS_P1 p.91 | 50.4 | FS_P1 p.89 | OK |
| Manipur | 29.4 | FS_P1 p.103 | 48.1 | FS_P1 p.101 | OK |
| Meghalaya | 53.8 | FS_P1 p.97 | 35.1 | FS_P1 p.95 | OK |
| Mizoram | 34.8 | FS_P1 p.109 | 50.0 | FS_P1 p.107 | OK |
| Nagaland | 28.9 | FS_P1 p.115 | 44.4 | FS_P1 p.113 | OK |
| Odisha | 64.3 | FS_P2 p.45 | 33.0 | FS_P2 p.43 | OK |
| Puducherry | 55.1 | FS_P2 p.93 | 65.4 | FS_P2 p.91 | OK |
| Punjab | 58.7 | FS_P2 p.51 | 56.0 | FS_P2 p.49 | OK |
| Rajasthan | 54.4 | FS_P2 p.57 | 33.4 | FS_P2 p.55 | OK |
| Sikkim | 42.1 | FS_P1 p.121 | 49.0 | FS_P1 p.119 | OK |
| Tamil Nadu | 53.4 | FS_P2 p.63 | 56.6 | FS_P2 p.61 | OK |
| Telangana | 57.6 | FS_P1 p.127 | 45.5 | FS_P1 p.125 | OK |
| Tripura | 67.2 | FS_P1 p.133 | 23.2 | FS_P1 p.131 | OK |
| Uttar Pradesh | 50.4 | FS_P2 p.69 | 39.3 | FS_P2 p.67 | OK |
| Uttarakhand | 42.6 | FS_P2 p.75 | 50.4 | FS_P2 p.73 | OK |
| West Bengal | 71.4 | FS_P1 p.139 | 32.9 | FS_P1 p.137 | OK |

*Method:* extracted with `pypdf` from the official compendia; per state the NFHS-5
"Total" column for line "16. Women with 10 or more years of schooling (%)" and line
"95. All women age 15-49 years who are anaemic (%)". The compendium PDFs are the
definitive reference.

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
