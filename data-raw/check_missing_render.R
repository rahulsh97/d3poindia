# check_missing_render.R — confirm that a genuinely missing state RENDERS grey
# (not just that the widget object builds). Fill colours are resolved in the
# browser by the d3po binding, so this is a render-level check, not a unit test.
#
# Procedure:
#   1. Build the map with one state forced missing (value = NA).
#   2. Save the d3po widget to HTML.
#   3. Open it in a browser and inspect the SVG: the missing state must have a
#      neutral grey fill (d3po uses #cccccc for no-data), distinct from the
#      viridis gradient used for states with values.
#
# Recorded result (2026-09-19, Kerala dropped from women_anaemia): the SVG
# contained exactly ONE #cccccc polygon among 37 paths — the missing state —
# while all others were viridis rgb() colours. Visually, Kerala rendered grey.
# See docs/RENDER_CHECK.md.
#
# Run:  Rscript data-raw/check_missing_render.R  (then open the HTML in a browser)
suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(d3po)
}))

drop_state <- "Kerala"
d <- nfhs_map_data("women_anaemia", drop = drop_state)
stopifnot(is.na(d$value[d$region == drop_state]))           # forced missing
stopifnot(!any(is.na(d$value[d$region != drop_state])))     # others intact

w <- d3po(d) |>
  po_geomap(daes(group = region, color = viridisLite::viridis(5),
                 size = value, gradient = TRUE, tooltip = label))

out <- file.path(tempdir(), "d3po_missing_render.html")
htmlwidgets::saveWidget(w, out, selfcontained = FALSE)
cat("Wrote", out, "\n")
cat("Open it and confirm exactly one grey (#cccccc) polygon =", drop_state, "\n")
