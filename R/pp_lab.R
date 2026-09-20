# Productivity-Pay Sensitivity Lab (internal helpers)
#
# A small, honest, DESCRIPTIVE cross-section built only from the already-verified
# ASI 2023-24 state aggregates (`asi_state`): 36 states/UTs, single year, current
# prices. It describes the association between labour productivity (net value
# added per worker) and pay (wages per worker) across states, and how sensitive
# that association is to individual states (leave-one-state-out).
#
# Deliberately NOT included: fixed effects, clustering, causal claims, or any
# invented/synthesised data. This is one bivariate ordinary least-squares line
# through 36 points, reported as an association only.

#' Assemble the 36-state productivity/pay frame from asi_state (internal)
#' @return data.frame with region, productivity (NVA/worker), pay (wages/worker),
#'   both in Rs lakh/worker, current prices. Rows with a missing value dropped.
#' @noRd
pp_lab_data <- function() {
  a <- d3poindia::asi_state
  p <- a[a$indicator == "nva_per_worker",   c("region", "value")]
  w <- a[a$indicator == "wages_per_worker", c("region", "value")]
  d <- merge(p, w, by = "region")
  names(d) <- c("region", "productivity", "pay")
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d[order(d$region), , drop = FALSE]
}

#' Descriptive OLS fit of pay on productivity (internal)
#' @param d frame with `productivity` and `pay`.
#' @return list(n, slope, intercept, r, r2) or NULL if < 3 points.
#' @noRd
pp_fit <- function(d) {
  if (is.null(d) || nrow(d) < 3L) return(NULL)
  m <- stats::lm(pay ~ productivity, data = d)
  cf <- stats::coef(m)
  list(
    n         = nrow(d),
    slope     = unname(cf[["productivity"]]),
    intercept = unname(cf[["(Intercept)"]]),
    r         = stats::cor(d$productivity, d$pay),
    r2        = summary(m)$r.squared
  )
}

#' Leave-one-state-out influence on the slope (jackknife) (internal)
#'
#' For each state, drop it, refit, and record the slope without it and the change
#' in slope (full - without). States with the largest absolute change are the most
#' influential on the descriptive relationship.
#' @param d frame from [pp_lab_data()].
#' @return data.frame(region, slope_without, dslope), ordered by |dslope| desc.
#' @noRd
pp_influence <- function(d) {
  full <- pp_fit(d)
  if (is.null(full)) return(NULL)
  slope_wo <- vapply(seq_len(nrow(d)), function(i) {
    f <- pp_fit(d[-i, , drop = FALSE])
    if (is.null(f)) NA_real_ else f$slope
  }, numeric(1))
  out <- data.frame(
    region        = d$region,
    slope_without = slope_wo,
    dslope        = full$slope - slope_wo,
    stringsAsFactors = FALSE
  )
  out[order(-abs(out$dslope)), , drop = FALSE]
}
