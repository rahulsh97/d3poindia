# ASI 2023-24 source, definitions and verification (BharatViz productivity & pay layer)

## Source (official, aggregate; no microdata)
- **Publication:** Annual Survey of Industries (ASI) 2023-24, Ministry of
  Statistics and Programme Implementation (MoSPI). Reference period: accounting
  years ending between 1 Apr 2023 and 31 Mar 2024. Released 27 Aug 2025.
- **Table used:** *Table 4 - "Estimate of some important characteristics by
  State"*, sheet `web4`, **All Industries** (see "Coverage" below). Value figures
  in **Rs Lakh**; workers/persons in **Number**.

## Coverage: what "All Industries" means
"All Industries" is the ASI aggregate **across all covered NIC industry groups**
for the **registered factory sector**, i.e. the whole ASI frame:
- factories registered under the **Factories Act, 1948** (Sec 2m(i)/2m(ii));
- **bidi & cigar** establishments (Bidi and Cigar Workers Act, 1966);
- **electricity undertakings** (generation, transmission, distribution) **not**
  registered with the Central Electricity Authority (CEA).

The statutory **"manufacturing process"** (Factories Act Sec 2(k)) that defines
an ASI factory is broader than everyday "manufacturing": besides making/altering
goods it explicitly includes **generating/transmitting power, pumping oil/water/
sewage, cold storage, ship repair, and printing/bookbinding** (ASI
CONCEPTS_AND_DEFINITIONS, para 6). So the ASI "All Industries" total is
**predominantly manufacturing but not manufacturing-only**, and it is **not the
whole economy**: the **unorganised sector is excluded** (that is the separate
ASUSE universe). This layer is therefore labelled "registered factory sector
(all industries)", not "manufacturing", throughout the app and data.
- **Obtained from:** MoSPI NADA catalogue, ASI 2023-24
  (`https://microdata.gov.in/NADA/index.php/catalog/256`) -> **Downloads** ->
  *"Table 4 Estimate of important characteristics by State_2023_2024.xls"*
  (download id 3938). Cross-checked against *Table 3 - Principal Characteristics
  by Major States* (id 3937) and the ASI 2023-24 PIB results note (27 Aug 2025).
- **Vendored:** `data-raw/ASI_2023-24_Table4_State.xls` (and
  `ASI_2023-24_Table3_MajorStates.xls` for cross-checking).
- **Built by:** `data-raw/build_asi.R` -> `data/asi_state.rda`.

## Reuse rights
Every ASI 2023-24 table on the MoSPI portals is **"Released under the Revised
Guidelines for Statistical Data Dissemination (GSDD), in accordance with NDSAP,
2012"** - the official framework for disseminating aggregate official statistics.
Only published **aggregate** state totals are used here; **no unit-level
microdata** is used or redistributed.

## Indicators (both current prices, worker basis)
| id | definition (from Table 4, All Industries) | unit |
|----|-------------------------------------------|------|
| `wages_per_worker` | "Wages to Workers" / "Number of Workers" | Rs lakh/worker |
| `nva_per_worker`   | "Net Value Added" / "Number of Workers"  | Rs lakh/worker |

### Why these are matched
ASI distinguishes **Workers** from **Total Persons Engaged** (workers +
supervisory/managerial + others), and correspondingly **Wages** (ASI
CONCEPTS_AND_DEFINITIONS para 19: remuneration paid regularly to *workers*) from
**Emoluments** (para 22: defined as wages but paid to *all employees* plus
benefits in kind). MoSPI's own published structural ratios therefore pair
**Wages per Worker** and **Emoluments per Persons Engaged** - never "emoluments
per worker". So:
- **Pay** uses `Wages to Workers / Number of Workers`: numerator and denominator
  refer to the **same workers** (matched).
- **Productivity** uses **Net Value Added**, the directly-published,
  correctly-labelled value-added measure, per worker (ASI's own headline
  labour-productivity ratio is "Net Value Added per Worker"). Gross Value Added
  is not a separate row in Table 4; NVA is used to avoid any derivation.

Wages, workers and value added all come from the **same** Table 4 "All
Industries" columns - same state, same sector (registered factory sector),
same year - so their coverage is consistent.

## Verification (reproduced by build_asi.R from the published totals)
**All-India anchor** (must equal the published ASI 2023-24 structural ratios):
- Wages per Worker = 33,598,653 lakh x 1e5 / 15,519,957 = **Rs 216,487** (published 216,487).
- NVA per Worker  = 210,314,335 lakh x 1e5 / 15,519,957 = **Rs 1,355,122** (published 1,355,122).

**Three states, recalculated from Table 4 published totals:**
| State | Workers | Wages (Rs lakh) | NVA (Rs lakh) | Wages/worker | NVA/worker |
|-------|--------:|----------------:|--------------:|-------------:|-----------:|
| Maharashtra | 1,849,633 | 4,925,708 | 34,145,981 | Rs 2.66 lakh | Rs 18.46 lakh |
| Gujarat     | 1,978,830 | 4,361,165 | 29,099,304 | Rs 2.20 lakh | Rs 14.71 lakh |
| Tamil Nadu  | 2,475,675 | 4,904,057 | 21,395,650 | Rs 1.98 lakh | Rs 8.64 lakh |
| Sikkim      |    20,825 |    61,717 |  1,528,940 | Rs 2.96 lakh | Rs 73.42 lakh |

**Sikkim is a genuine outlier, not an error:** a very small factory workforce
(20,825 workers) with very high net value added (Rs 15,289 crore), giving
NVA/worker = 1,528,940 x 1e5 / 20,825 = **Rs 73.42 lakh**. This is a small
denominator combined with high value added per worker; the specific industry
composition behind it is not asserted here, as Table 4 is state-total only and
no state-by-industry breakdown was consulted. Verified against Table 4 sheet
`web4`, column "Sikkim".

## State crosswalk
Table 4 lists 36 States/UTs (excluding the All-India column) - the same 36 as the
`india_map` boundary layer. Names are matched by normalisation (`&`->`and`,
upper-case, strip non-alphanumerics) plus three explicit overrides:
`Andaman & N. Island -> Andaman and Nicobar`, `Chattisgarh -> Chhattisgarh`,
`Dadra & N Haveli & Daman & Diu -> Dadra and Nagar Haveli and Daman and Diu`.
The `Lakshadweep*` footnote marker is stripped. All 36 resolve 1:1.

## Interpretation limits (enforced in the UI copy)
- **Current prices** (nominal), single year: these are **level** differences,
  not real/inflation-adjusted and not a trend or growth rate.
- **ASI = registered factory sector, all industries** (predominantly
  manufacturing, also covered electricity/gas/water/repair units); **not the
  whole economy** and the unorganised sector is out of scope. Industry
  composition differs across states, so cross-state gaps partly reflect what each
  state makes.
- **Descriptive, not causal:** higher productivity is not shown to cause higher
  pay. The ASI layer is not joined to the NFHS layer.
