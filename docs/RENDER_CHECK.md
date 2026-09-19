# Render check: missing state appears grey

**Claim under test:** the app states "grey = no data (never zero)". Because the
choropleth fill is resolved in the browser by the d3po binding (not in R), the
unit test in `tests/testthat/test-nfhs-data.R` can only confirm that (a) a missing
state keeps `value = NA` (never 0) and its label is "no data", and (b) the
`po_geomap` object builds. Whether it actually **renders** grey is verified here,
at the render level.

## Procedure (reproducible)
`Rscript data-raw/check_missing_render.R` builds the map with one state forced
missing and saves the d3po widget to HTML; open it in a browser and inspect the
SVG fills.

## Result (2026-09-19, Kerala dropped from `women_anaemia`)
- The rendered SVG contained **exactly one `#cccccc` (neutral grey) polygon**
  among 37 paths — the missing state — while every state with a value used a
  viridis `rgb(...)` colour.
- Visual confirmation: Kerala rendered grey at the southern tip; all other states
  followed the anaemia gradient (Ladakh yellow ≈ 92.8, dark-purple lows).

**Conclusion:** a genuinely missing value renders as grey (`#cccccc`), distinct
from the gradient and from zero. The "grey = no data" statement is accurate.
