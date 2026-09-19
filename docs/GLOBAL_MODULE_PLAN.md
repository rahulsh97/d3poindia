# Growth & Work Atlas — global module plan (NOT yet shipped)

The India panel (BharatViz, NFHS-5 context) is implemented. The **global
industry module** — the productivity → pay → jobs story and the accounting
bridge — is **deliberately not shipped yet**. Per the brief, importers are
implemented only *after* source download and variable-level checks. This file
records the feasibility findings and the concrete next steps.

## Non-negotiable boundary rules (apply to every global series)
- **UNIDO INDSTAT monetary values are at current prices.** Never present nominal
  value added per employee as *real* productivity growth.
- **Real productivity growth vs real compensation growth** may only be paired
  when commensurate deflators/denominators exist for both. Otherwise show levels
  or nominal ratios, clearly labelled.
- **Disclose estimated/missing observations** and country/industry coverage on
  every chart.
- **No manufactured data**: no synthesised observations, crosswalk weights,
  PPP comparability, wage deflators, correlations, or causal claims.
- The accounting bridge (within-industry vs employment-share shift) is a
  **descriptive identity**, not a causal estimate or forecast; state the exact
  identity, exclusions, and residual.

## OECD STAN (first global source)
- **Access probe (2026-09-19):** the OECD SDMX REST base
  (`https://sdmx.oecd.org/public/rest/...`) is reachable (HTTP 200). A guessed
  STAN dataflow id returned 404, so **the exact dataflow / DSD id and variable
  codes must be confirmed** from the OECD Data Explorer before coding.
- **Next steps (in order):**
  1. Identify the current STAN Rev.4 dataflow id + DSD via the Data Explorer.
  2. Pull, for 2–3 countries, real value added (VALK/…), employment (EMPN),
     hours (HRSN) and employee compensation (LABR) — confirm exact codes.
  3. Record variable definitions, base year/deflator, `OBS_STATUS`
     (estimated/provisional), and country/year coverage.
  4. Only if real VA **and** a real deflator/denominator are both present, build
     the paired productivity-vs-compensation slope chart + employment strip.
- **Ship gate:** the global chart appears only when the above pull is verified;
  until then the module stays absent (no placeholder numbers).

## RBI India KLEMS (India industry, real)
- Separate RBI product (not UNIDO, not state-level). Real industry value added /
  labour-productivity trends and decompositions.
- **Plan:** download the KLEMS data + manual from
  <https://www.rbi.org.in/Scripts/KLEMS.aspx>; verify variable definitions,
  industry classification and base year at the variable level; write a
  source-specific methods page; then implement the importer. **Do not** merge
  KLEMS into district/state records — it is national industry-level.

## UNIDO INDSTAT (global industry, current prices)
- Free, CC BY 4.0 since Feb 2022 (see the workspace feasibility memo). Official
  REST API is **Cloudflare-protected**; use the documented API, ordinary browser
  access, or official bulk download only — **never** anti-bot circumvention. If
  documented access is unreliable, stop and use a manual-download + validated
  importer workflow, or contact UNIDO.
- **Plan:** import as a **separate nominal levels/ratios panel** (wage bill /
  value added; wages per employee), explicitly labelled current-price; never
  described as real growth. Verify per-database CC BY 4.0 (the "unless otherwise
  stated" exception) before publishing derived data.

## Sequencing
NFHS India slice (done) → STAN feasibility pull (verify ids) → STAN global chart
(if valid) → RBI KLEMS methods page + importer → UNIDO nominal panel. Each behind
its own source-specific methods page and tests.
