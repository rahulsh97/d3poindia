# Data-integrity tests for the BharatViz NFHS-5 aggregate layer.
# These are release gates: denominators/units, source vintage, state-key
# coverage, missing-vs-zero, and value ranges.

test_that("nfhs_state has the expected indicators, each covering all 36 states", {
  d <- d3poindia::nfhs_state
  expect_setequal(unique(d$indicator), c("women_anaemia", "women_schooling10"))
  for (ind in unique(d$indicator)) {
    s <- d[d$indicator == ind, ]
    expect_equal(nrow(s), 36L)                 # one row per state/UT
    expect_equal(length(unique(s$region)), 36L)
    expect_false(any(is.na(s$value)))          # source is complete
  }
})

test_that("values are valid percentages with documented units and round", {
  d <- d3poindia::nfhs_state
  expect_true(all(d$unit == "%"))
  expect_true(all(d$value >= 0 & d$value <= 100))
  expect_true(all(d$round == "NFHS-5 (2019-21)"))
  expect_true(all(nzchar(d$source)))
  expect_true(all(d$higher_is %in% c("better", "worse")))
})

test_that("every nfhs_state region matches a boundary in india_map (state-key coverage)", {
  d <- d3poindia::nfhs_state
  g <- d3poindia::india_map
  expect_true(all(d$region %in% g$region))
  # india_map is the authoritative 36-state boundary layer
  expect_equal(nrow(g), 36L)
})

test_that("india_map is a valid sf layer in EPSG:4326 with a region key", {
  g <- d3poindia::india_map
  expect_s3_class(g, "sf")
  expect_true(all(c("region", "geometry") %in% names(g)))
  expect_equal(sf::st_crs(g)$epsg, 4326L)
})

test_that("missing states join to NA, never to zero (missing != zero)", {
  g <- d3poindia::india_map
  # Drop one state from the indicator to simulate a gap.
  sel <- d3poindia::nfhs_state[d3poindia::nfhs_state$indicator == "women_anaemia", ]
  sel <- sel[sel$region != "Kerala", c("region", "value")]
  d <- dplyr::left_join(g, sel, by = "region")
  kerala <- d$value[d$region == "Kerala"]
  expect_true(is.na(kerala))                   # missing -> NA
  expect_false(isTRUE(kerala == 0))            # never silently zero
})

test_that("known NFHS-5 reference values are preserved (guards transcription errors)", {
  d <- d3poindia::nfhs_state
  ker <- d$value[d$indicator == "women_schooling10" & d$region == "Kerala"]
  bih <- d$value[d$indicator == "women_schooling10" & d$region == "Bihar"]
  ldk <- d$value[d$indicator == "women_anaemia"    & d$region == "Ladakh"]
  expect_equal(ker, 77.0, tolerance = 0.1)     # Kerala schooling highest
  expect_true(bih < 35)                        # Bihar schooling low
  expect_equal(ldk, 92.8, tolerance = 0.1)     # Ladakh anaemia highest
})
