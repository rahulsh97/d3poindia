# Tests for the Productivity-Pay Sensitivity Lab helpers (descriptive; 36-state).

test_that("pp_lab_data returns the 36-state productivity/pay frame", {
  d <- pp_lab_data()
  expect_equal(nrow(d), 36L)
  expect_setequal(names(d), c("region", "productivity", "pay"))
  expect_false(anyNA(d))
  expect_true(all(d$productivity > 0) && all(d$pay > 0))
})

test_that("pp_fit reproduces a plain OLS of pay on productivity", {
  d <- pp_lab_data()
  f <- pp_fit(d)
  m <- stats::lm(pay ~ productivity, data = d)
  expect_equal(f$n, 36L)
  expect_equal(f$slope, unname(stats::coef(m)[["productivity"]]), tolerance = 1e-9)
  expect_equal(f$intercept, unname(stats::coef(m)[["(Intercept)"]]), tolerance = 1e-9)
  expect_equal(f$r, stats::cor(d$productivity, d$pay), tolerance = 1e-9)
  expect_true(f$r2 >= 0 && f$r2 <= 1)
  expect_null(pp_fit(d[1:2, ]))            # < 3 points -> NULL
})

test_that("leave-one-out influence is well-formed and identifies the top mover", {
  d <- pp_lab_data()
  full <- pp_fit(d)$slope
  inf <- pp_influence(d)
  expect_equal(nrow(inf), 36L)
  expect_false(anyNA(inf$dslope))
  # dslope defined as slope(all) - slope(without state)
  i_ker <- which(inf$region == "Kerala")
  refit <- pp_fit(d[d$region != "Kerala", ])$slope
  expect_equal(inf$dslope[i_ker], full - refit, tolerance = 1e-9)
  # ordered by |dslope| descending
  expect_equal(inf$region, inf$region[order(-abs(inf$dslope))])
  # Sikkim is the extreme productivity outlier (73.4 lakh/worker) -> most
  # influential single state on the fitted slope.
  expect_equal(inf$region[1], "Sikkim")
})
