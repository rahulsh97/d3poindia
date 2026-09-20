# build_asi.R - reproducibly builds the ASI 2023-24 factory-sector productivity &
# pay layer used by BharatViz. ("All Industries" = the ASI registered factory
# sector across all covered NIC groups: Factories Act units + bidi/cigar
# establishments + non-CEA electricity undertakings; predominantly manufacturing
# but also covered electricity/gas/water/cold-storage/repair units. The
# unorganised sector is out of scope.)
#
# Input (vendored in data-raw/, AGGREGATE published data - no microdata):
#   - ASI_2023-24_Table4_State.xls : ASI 2023-24 "Table 4: Estimate of some
#     important characteristics by State", All-Industries (registered factory
#     sector). Official source: MoSPI NADA catalogue, ASI 2023-24
#     (https://microdata.gov.in/NADA/index.php/catalog/256, Downloads ->
#     "Table 4 Estimate of important characteristics by State"). Released under
#     the Revised Guidelines for Statistical Data Dissemination (GSDD, in
#     accordance with NDSAP, 2012). See data-raw/ASI_SOURCE.md.
#
# Output (data/):
#   - asi_state.rda : tidy state x indicator aggregates (2 indicators), same
#     schema as nfhs_state, region key matching india_map$region.
#
# Two indicators, both CURRENT PRICES, ASI 2023-24, worker basis:
#   wages_per_worker = "Wages to Workers" / "Number of Workers"   (matched: the
#                      numerator is the wage bill paid to those same workers)
#   nva_per_worker   = "Net Value Added"  / "Number of Workers"   (NVA is the
#                      directly-published, correctly-labelled value-added measure;
#                      per-worker is ASI's own headline labour-productivity ratio)
#
# Values are expressed in Rs LAKH per worker (source values are in Rs lakh;
# dividing by the worker count keeps the compact "lakh per worker" unit).
#
# Run from the package root:  Rscript data-raw/build_asi.R
suppressWarnings(suppressMessages({
  library(readxl)
  library(sf)
}))

here <- function(...) file.path("data-raw", ...)

## ---- 1. Read the official state table --------------------------------------
xls <- here("ASI_2023-24_Table4_State.xls")
raw <- suppressMessages(read_excel(xls, col_names = FALSE, .name_repair = "minimal"))
raw <- as.data.frame(raw)

# Row 4 holds the column headers: col1 = "Characteristics", col2 = "All India",
# cols 3.. = individual States/UTs.
hdr <- as.character(unlist(raw[4, ]))
state_cols <- 3:ncol(raw)                       # drop the All-India column (col 2)
asi_states <- hdr[state_cols]

# Locate the three characteristic rows by their (fixed) row labels.
lab <- as.character(raw[[1]])
row_of <- function(pattern) {
  hit <- which(grepl(pattern, lab, ignore.case = TRUE))
  if (length(hit) != 1) stop("Expected exactly one row matching: ", pattern,
                             " (found ", length(hit), ")")
  hit
}
r_workers <- row_of("^\\s*6\\. *Number of Workers")
r_wages   <- row_of("^\\s*8\\. *Wages to Workers")
r_nva     <- row_of("^\\s*17\\. *Net Value Added")

num <- function(row) suppressWarnings(as.numeric(unlist(raw[row, state_cols])))
workers <- num(r_workers)      # Number (persons)
wages   <- num(r_wages)        # Rs lakh
nva     <- num(r_nva)          # Rs lakh

## ---- 2. Crosswalk ASI state names -> india_map region key ------------------
geo_regions <- readRDS(here("india_states_map.rds"))$state_name
norm <- function(x) gsub("[^A-Z0-9]", "", toupper(gsub("&", "AND", x)))
geo_norm <- norm(geo_regions)
# Explicit overrides for the ASI spellings that do not normalise-match.
overrides <- c(
  "Andaman & N. Island"            = "Andaman and Nicobar",
  "Chattisgarh"                    = "Chhattisgarh",
  "Dadra & N Haveli & Daman & Diu" = "Dadra and Nagar Haveli and Daman and Diu"
)
fix_name <- function(x) {
  vapply(x, function(s) {
    s <- trimws(gsub("\\*+$", "", s))           # strip footnote marker (Lakshadweep*)
    if (s %in% names(overrides)) return(unname(overrides[s]))
    hit <- which(geo_norm == norm(s))
    if (length(hit) == 1) return(geo_regions[hit])
    NA_character_
  }, character(1))
}
region <- unname(fix_name(asi_states))
if (any(is.na(region))) {
  stop("Unmatched ASI state names: ",
       paste(asi_states[is.na(region)], collapse = ", "))
}
stopifnot(length(unique(region)) == length(geo_regions))   # all 36, 1:1

## ---- 3. Derive the two per-worker indicators (Rs lakh per worker) -----------
# value_lakh / workers = (value in Rs lakh) per worker.
mk <- function(value_lakh, id, lab_txt, universe) {
  data.frame(
    region        = region,
    indicator     = id,
    indicator_lab = lab_txt,
    value         = value_lakh / workers,
    unit          = "Rs lakh/worker",
    universe      = universe,
    higher_is     = "better",
    round         = "ASI 2023-24 (current prices)",
    source        = "ASI 2023-24, MoSPI - Table 4 (All Industries, registered factory sector); NADA catalogue 256; current prices",
    stringsAsFactors = FALSE
  )
}
asi_state <- rbind(
  mk(wages, "wages_per_worker",
     "Wages per worker",
     "Workers in the ASI registered factory sector (all industries)"),
  mk(nva, "nva_per_worker",
     "Net value added per worker",
     "ASI registered factory sector (all industries)")
)
rownames(asi_state) <- NULL
stopifnot(!any(is.na(asi_state$value)), all(asi_state$value > 0))

## ---- 4. Verification: reproduce official ratios from published totals -------
# 4a. All-India anchor (must match ASI 2023-24 published structural ratios, in
#     rupees: Wages per Worker = 216487; Net Value Added per Worker = 1355122).
ai <- function(row) suppressWarnings(as.numeric(raw[row, 2]))   # All-India col
ai_workers <- ai(r_workers); ai_wages <- ai(r_wages); ai_nva <- ai(r_nva)
anchor_wages_rs <- ai_wages * 1e5 / ai_workers
anchor_nva_rs   <- ai_nva   * 1e5 / ai_workers
cat(sprintf("All-India anchor  wages/worker = Rs %.0f (published 216487)\n", anchor_wages_rs))
cat(sprintf("All-India anchor  NVA/worker   = Rs %.0f (published 1355122)\n", anchor_nva_rs))
stopifnot(abs(anchor_wages_rs - 216487) < 5, abs(anchor_nva_rs - 1355122) < 5)

# 4b. Three named states, recalculated from the published totals (Rs/worker and
#     Rs lakh/worker).
recheck <- function(st) {
  j <- which(region == st)
  w  <- workers[j]; wl <- wages[j]; nl <- nva[j]
  cat(sprintf(
    "%-12s workers=%d  wages=%s lakh  NVA=%s lakh  ->  wages/worker=Rs %.0f (%.2f lakh)  NVA/worker=Rs %.0f (%.2f lakh)\n",
    st, w, format(wl, big.mark=","), format(nl, big.mark=","),
    wl*1e5/w, wl/w, nl*1e5/w, nl/w))
}
cat("\nThree-state recalculation from published totals (Table 4, All Industries):\n")
invisible(lapply(c("Maharashtra", "Gujarat", "Tamil Nadu"), recheck))

## ---- 5. Coverage report -----------------------------------------------------
cat(sprintf("\nasi_state: %d rows, %d states x %d indicators\n",
            nrow(asi_state), length(unique(asi_state$region)),
            length(unique(asi_state$indicator))))

## ---- 6. Save ----------------------------------------------------------------
save(asi_state, file = "data/asi_state.rda", compress = "xz")
cat("Wrote data/asi_state.rda\n")
