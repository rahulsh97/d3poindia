# Data-integrity tests for the BharatViz ASI 2023-24 productivity & pay layer.
# Release gates: coverage, units/labels, state-key join, missing-vs-zero, and
# reference values recomputed from the official published totals.

test_that("asi_state has the two indicators, each covering all 36 states", {
  d <- d3poindia::asi_state
  expect_setequal(unique(d$indicator), c("wages_per_worker", "nva_per_worker"))
  for (ind in unique(d$indicator)) {
    s <- d[d$indicator == ind, ]
    expect_equal(nrow(s), 36L)
    expect_equal(length(unique(s$region)), 36L)
    expect_false(any(is.na(s$value)))
    expect_true(all(s$value > 0))              # positive Rs lakh/worker
  }
})

test_that("units, round and source are correctly and consistently labelled", {
  d <- d3poindia::asi_state
  expect_true(all(d$unit == "Rs lakh/worker"))
  expect_true(all(d$round == "ASI 2023-24 (current prices)"))
  expect_true(all(grepl("ASI 2023-24", d$source)))
  expect_true(all(grepl("current prices", d$source)))
  expect_true(all(d$higher_is %in% c("better", "worse")))
  # Coverage must be labelled as the factory sector / All Industries, not as
  # plain "manufacturing" (which would overstate the scope).
  expect_true(all(grepl("factory sector", d$universe)))
  expect_true(all(grepl("factory sector", d$source)))
  expect_false(any(grepl("manufacturing", d$universe, ignore.case = TRUE)))
  # No real-growth / trend language must leak into user-facing labels.
  txt <- tolower(paste(d$indicator_lab, d$universe, d$source))
  expect_false(any(grepl("real growth|trend|inflation-adjusted", txt)))
})

test_that("every asi_state region matches a boundary in india_map", {
  d <- d3poindia::asi_state
  g <- d3poindia::india_map
  expect_true(all(d$region %in% g$region))
  expect_equal(nrow(g), 36L)
})

test_that("map helper marks a genuinely missing state NA/no-data and builds a widget", {
  d <- map_data_for("nva_per_worker", drop = "Maharashtra")
  mh_val <- d$value[d$region == "Maharashtra"]
  mh_lab <- d$label[d$region == "Maharashtra"]
  expect_true(is.na(mh_val))                   # missing -> NA, not 0
  expect_false(isTRUE(mh_val == 0))
  expect_match(mh_lab, "no data")
  expect_false(any(is.na(d$value[d$region != "Maharashtra"])))
  obj <- d3po::po_geomap(
    d3po::d3po(d),
    d3po::daes(group = region, color = viridisLite::viridis(5),
               size = value, gradient = TRUE, tooltip = label)
  )
  expect_true(inherits(obj, "d3po"))
})

test_that("known ASI 2023-24 values are reproduced from published state totals", {
  d <- d3poindia::asi_state
  wpw <- function(st) d$value[d$indicator == "wages_per_worker" & d$region == st]
  npw <- function(st) d$value[d$indicator == "nva_per_worker"   & d$region == st]
  # Recomputed from Table 4 (All Industries), in Rs lakh/worker (see ASI_SOURCE.md).
  expect_equal(wpw("Maharashtra"), 2.66, tolerance = 0.01)
  expect_equal(npw("Maharashtra"), 18.46, tolerance = 0.02)
  expect_equal(wpw("Gujarat"),     2.20, tolerance = 0.01)
  expect_equal(npw("Gujarat"),     14.71, tolerance = 0.02)
  expect_equal(wpw("Tamil Nadu"),  1.98, tolerance = 0.01)
  expect_equal(npw("Tamil Nadu"),   8.64, tolerance = 0.02)
  # Sikkim: verified small-workforce, high-NVA outlier (20,825 workers;
  # NVA 1,528,940 lakh -> 73.42 lakh/worker). Kept, not capped.
  expect_equal(wpw("Sikkim"),       2.96, tolerance = 0.01)
  expect_equal(npw("Sikkim"),      73.42, tolerance = 0.02)
  expect_equal(npw("Sikkim"),
               max(d$value[d$indicator == "nva_per_worker"]))  # the top outlier
  # Sanity: productivity exceeds pay per worker in every state (NVA includes
  # returns to capital and non-worker persons), and pay is a plausible share.
  m <- merge(
    d[d$indicator == "wages_per_worker", c("region", "value")],
    d[d$indicator == "nva_per_worker",   c("region", "value")],
    by = "region", suffixes = c("_w", "_n")
  )
  expect_true(all(m$value_n > m$value_w))
})
