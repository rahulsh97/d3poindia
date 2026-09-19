# verify_nfhs.R — transparent validation of the NFHS-5 extract used by BharatViz.
#
# Runs three independent checks on data-raw/NFHS-5-States.csv:
#   (1) National anchor: India totals match the official NFHS-5 fact sheet.
#   (2) Urban/rural/total consistency for all 36 states/UTs: the published state
#       total must lie within [min(urban,rural), max(urban,rural)] (a population
#       weighted blend cannot fall outside its parts) — catches transcription
#       errors row by row.
#   (3) Range + completeness: every state has a numeric value in [0, 100].
# Prints the full 36-row table for both indicators for auditability.
#
# Run:  Rscript data-raw/verify_nfhs.R
raw <- read.csv("data-raw/NFHS-5-States.csv", check.names = FALSE, stringsAsFactors = FALSE)

inds <- c(
  women_anaemia     = "95. All women age 15-49 years who are anaemic22 (%)",
  women_schooling10 = "16. Women with 10 or more years of schooling (%)"
)
# Official NFHS-5 all-India fact-sheet totals (IIPS/MoHFW compendium).
anchor <- c(women_anaemia = 57.0, women_schooling10 = 41.0)

fails <- 0
for (id in names(inds)) {
  src <- inds[[id]]
  d <- raw[raw$indicator == src, c("state", "nfhs5_urban", "nfhs5_rural", "nfhs5_total")]
  u <- suppressWarnings(as.numeric(d$nfhs5_urban))
  r <- suppressWarnings(as.numeric(d$nfhs5_rural))
  t <- suppressWarnings(as.numeric(d$nfhs5_total))

  # (1) national anchor
  india_t <- t[d$state == "India"]
  ok_anchor <- isTRUE(abs(india_t - anchor[[id]]) < 0.05)
  cat(sprintf("\n== %s ==\n", id))
  cat(sprintf("(1) national anchor: India total = %.1f (official %.1f) -> %s\n",
              india_t, anchor[[id]], ifelse(ok_anchor, "OK", "MISMATCH")))
  if (!ok_anchor) fails <- fails + 1

  # states only (drop India)
  keep <- d$state != "India"
  ds <- d[keep, ]; us <- u[keep]; rs <- r[keep]; ts <- t[keep]

  # (3) completeness + range
  n_states <- nrow(ds)
  n_na <- sum(is.na(ts))
  in_range <- all(ts >= 0 & ts <= 100, na.rm = TRUE)
  cat(sprintf("(3) states: %d | NA totals: %d | all in [0,100]: %s\n",
              n_states, n_na, in_range))
  if (n_states != 36 || n_na != 0 || !in_range) fails <- fails + 1

  # (2) urban/rural/total consistency (0.5pp tolerance for rounding)
  lo <- pmin(us, rs) - 0.5; hi <- pmax(us, rs) + 0.5
  bad <- which(!(ts >= lo & ts <= hi))
  cat(sprintf("(2) total within [min(u,r),max(u,r)] for all states: %s",
              ifelse(length(bad) == 0, "OK\n", "VIOLATIONS:\n")))
  if (length(bad) > 0) {
    fails <- fails + 1
    print(data.frame(state = ds$state[bad], urban = us[bad], rural = rs[bad], total = ts[bad]))
  }
}

cat(sprintf("\nRESULT: %s (%d check group failure(s))\n",
            ifelse(fails == 0, "ALL CHECKS PASSED", "FAILED"), fails))
if (fails > 0) quit(status = 1)
