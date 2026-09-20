#' Look up the aggregate rows for one indicator (internal)
#'
#' Routes an indicator id to whichever aggregate layer defines it (NFHS-5 context
#' or ASI manufacturing productivity & pay). Both layers share the same schema,
#' so the server renders either identically.
#'
#' @param indicator An indicator id from `nfhs_state` or `asi_state`.
#' @return The matching rows (one per state), or a zero-row frame if unknown.
#' @noRd
indicator_rows <- function(indicator) {
  a <- d3poindia::nfhs_state
  b <- d3poindia::asi_state
  if (indicator %in% a$indicator) return(a[a$indicator == indicator, , drop = FALSE])
  if (indicator %in% b$indicator) return(b[b$indicator == indicator, , drop = FALSE])
  a[0, , drop = FALSE]
}

#' Build the choropleth sf for one indicator (internal)
#'
#' Joins the selected indicator onto the boundary layer and adds the tooltip
#' label. States with no value keep `NA` (rendered grey by the app), never zero.
#' Factored out of the server so the missing-value path is testable, and shared
#' by the NFHS and ASI layers.
#'
#' @param indicator An indicator id from `nfhs_state` or `asi_state`.
#' @param drop Optional character vector of regions to force-missing. Used only
#'   in tests to exercise the grey / "no data" path.
#' @return An `sf` object with `region`, `value` (numeric or `NA`) and `label`.
#' @noRd
map_data_for <- function(indicator, drop = NULL) {
  sel  <- indicator_rows(indicator)
  unit <- if (nrow(sel) > 0) sel$unit[1]  else ""
  rnd  <- if (nrow(sel) > 0) sel$round[1] else ""
  # Percentages read best at 1 dp; currency (Rs lakh/worker) at 2 dp.
  digits <- if (identical(unit, "%")) 1L else 2L
  sep    <- if (identical(unit, "%")) "" else " "     # "77%" vs "18.46 Rs lakh/worker"
  if (!is.null(drop)) sel <- sel[!(sel$region %in% drop), , drop = FALSE]

  # Attach the value by key with base match(), preserving the sf object and its
  # geometry (a dplyr join on sf depends on sf's dplyr S3 methods being
  # registered, which is not guaranteed when the package is merely loaded).
  # Unmatched (missing/dropped) states get NA - never 0 - and render grey.
  d <- d3poindia::india_map
  d$value <- round(sel$value[match(d$region, sel$region)], digits)
  d$label <- ifelse(
    is.na(d$value),
    paste0(d$region, ": no data (", rnd, ")"),
    paste0(d$region, ": ", d$value, sep, unit, " - ", rnd)
  )
  sf::st_as_sf(d)
}

#' Build the choropleth sf for one NFHS indicator (internal, back-compat)
#'
#' Thin wrapper over [map_data_for()] kept for existing callers/tests.
#'
#' @param indicator One of `nfhs_state$indicator`.
#' @param drop See [map_data_for()].
#' @noRd
nfhs_map_data <- function(indicator, drop = NULL) {
  map_data_for(indicator, drop = drop)
}
