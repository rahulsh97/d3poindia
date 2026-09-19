# build_data.R — reproducibly builds the aggregate package data used by BharatViz.
#
# Inputs (vendored in data-raw/, all AGGREGATE public data — no microdata):
#   - NFHS-5-States.csv        : NFHS-5 (2019-21) state fact-sheet key indicators
#                                (see data-raw/NFHS_SOURCE.md for provenance/licence)
#   - india_states_map.rds     : SimpleMaps India state boundaries, CC BY 4.0
#                                (see data-raw/MAP_SOURCE.md)
#
# Outputs (data/):
#   - nfhs_state.rda : tidy state x indicator aggregates (2 indicators)
#   - india_map.rda  : sf boundaries with a `region` key matching nfhs_state$region
#
# Run from the package root:  Rscript data-raw/build_data.R
suppressWarnings(suppressMessages({
  library(sf)
}))

here <- function(...) file.path("data-raw", ...)

## ---- 1. NFHS aggregates -----------------------------------------------------
raw <- read.csv(here("NFHS-5-States.csv"), check.names = FALSE, stringsAsFactors = FALSE)

# Indicator registry: exact source string -> short id + display label + unit.
indicators <- list(
  women_anaemia = list(
    src   = "95. All women age 15-49 years who are anaemic22 (%)",
    label = "Women age 15-49 who are anaemic",
    unit  = "%",
    universe = "All women age 15-49 years",
    higher_is = "worse"
  ),
  women_schooling10 = list(
    src   = "16. Women with 10 or more years of schooling (%)",
    label = "Women with 10+ years of schooling",
    unit  = "%",
    universe = "Women age 15-49 years",
    higher_is = "better"
  )
)

# NFHS state name -> geometry region name resolver.
# "India" is the national total and is intentionally dropped (not a state).
# Strategy: normalise (&->and, upper, strip non-alnum) and match to the geometry;
# fall back to explicit overrides for the two names that do not normalise-match.
geo_regions <- {
  g0 <- readRDS(here("india_states_map.rds"))
  g0$state_name
}
norm <- function(x) gsub("[^A-Z0-9]", "", toupper(gsub("&", "AND", x)))
geo_norm <- norm(geo_regions)
overrides <- c(
  "Andaman & Nicobar Islands" = "Andaman and Nicobar",
  "NCT Delhi"                 = "Delhi"
)
fix_name <- function(x) {
  vapply(x, function(s) {
    if (s %in% names(overrides)) return(unname(overrides[s]))
    hit <- which(geo_norm == norm(s))
    if (length(hit) == 1) return(geo_regions[hit])
    NA_character_
  }, character(1))
}

rows <- lapply(names(indicators), function(id) {
  meta <- indicators[[id]]
  sub <- raw[raw$indicator == meta$src & raw$state != "India",
             c("state", "nfhs5_total")]
  if (nrow(sub) == 0) stop("Indicator not found in source: ", meta$src)
  data.frame(
    region        = unname(fix_name(sub$state)),
    indicator     = id,
    indicator_lab = meta$label,
    value         = suppressWarnings(as.numeric(sub$nfhs5_total)),
    unit          = meta$unit,
    universe      = meta$universe,
    higher_is     = meta$higher_is,
    round         = "NFHS-5 (2019-21)",
    source        = "NFHS-5 state fact sheets, IIPS/MoHFW (via data.gov.in, NDSAP)",
    stringsAsFactors = FALSE
  )
})
nfhs_state <- do.call(rbind, rows)
stopifnot(!any(is.na(nfhs_state$value)))          # source values are complete
stopifnot(!any(is.na(nfhs_state$region)))         # every NFHS state resolved to a region
rownames(nfhs_state) <- NULL

## ---- 2. Boundaries ----------------------------------------------------------
g <- readRDS(here("india_states_map.rds"))
india_map <- g
india_map$region <- india_map$state_name          # key used by the app + joins
india_map <- india_map[, c("region", "state_code", "geometry")]
india_map <- sf::st_as_sf(india_map)

## ---- 3. Coverage report (printed at build time) -----------------------------
geo <- unique(india_map$region)
dat <- unique(nfhs_state$region)
cat("NFHS regions:", length(dat), "| geometry regions:", length(geo), "\n")
cat("NFHS regions with NO geometry (should be 0):",
    paste(setdiff(dat, geo), collapse = ", "), "\n")
cat("Geometry regions with NO NFHS data (shown gray on the map):",
    paste(setdiff(geo, dat), collapse = ", "), "\n")

## ---- 4. Save ----------------------------------------------------------------
save(nfhs_state, file = "data/nfhs_state.rda", compress = "xz")
save(india_map,  file = "data/india_map.rda",  compress = "xz")
cat("Wrote data/nfhs_state.rda (", nrow(nfhs_state), "rows ) and data/india_map.rda (",
    nrow(india_map), "features )\n")
