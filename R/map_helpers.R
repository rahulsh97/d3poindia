#' Build the choropleth sf for one NFHS indicator (internal)
#'
#' Joins the selected NFHS-5 indicator onto the boundary layer and adds the
#' tooltip label. States with no value keep `NA` (rendered grey by the app),
#' never zero. Factored out of the server so the missing-value path is testable.
#'
#' @param indicator One of `nfhs_state$indicator`.
#' @param drop Optional character vector of regions to force-missing. Used only
#'   in tests to exercise the grey / "no data" path.
#' @return An `sf` object with `region`, `value` (numeric or `NA`) and `label`.
#' @importFrom dplyr left_join
#' @noRd
nfhs_map_data <- function(indicator, drop = NULL) {
  sel <- d3poindia::nfhs_state[d3poindia::nfhs_state$indicator == indicator, , drop = FALSE]
  unit <- if (nrow(sel) > 0) sel$unit[1] else "%"
  if (!is.null(drop)) sel <- sel[!(sel$region %in% drop), , drop = FALSE]

  d <- dplyr::left_join(d3poindia::india_map, sel[, c("region", "value")], by = "region")
  d <- sf::st_as_sf(d)
  d$value <- round(d$value, 1)
  d$label <- ifelse(
    is.na(d$value),
    paste0(d$region, ": no data (NFHS-5)"),
    paste0(d$region, ": ", d$value, unit, " - NFHS-5 (2019-21)")
  )
  d
}
