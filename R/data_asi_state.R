#' ASI 2023-24 manufacturing productivity & pay, state-level (aggregate)
#'
#' Aggregate, published state/UT indicators from India's Annual Survey of
#' Industries (ASI 2023-24), **registered factory sector, all industries**.
#' "All Industries" is the ASI aggregate across all covered NIC groups: units
#' registered under the Factories Act 1948, plus bidi/cigar establishments and
#' non-CEA electricity undertakings. It is predominantly manufacturing but its
#' statutory "manufacturing process" also covers electricity generation/
#' transmission, gas & water/cold storage and repair; the unorganised sector is
#' excluded. Contains **no microdata**: derived only from official published
#' state totals. Long format, one row per state x indicator. Two indicators, both
#' at **current prices** on a **worker** basis:
#' \itemize{
#'   \item \code{wages_per_worker}: "Wages to Workers" / "Number of Workers"
#'     (numerator and denominator both refer to the same workers).
#'   \item \code{nva_per_worker}: "Net Value Added" / "Number of Workers"
#'     (NVA is the directly-published value-added measure; per-worker is ASI's own
#'     headline labour-productivity ratio).
#' }
#' Values are in Rs lakh per worker. These are descriptive level differences at
#' current prices; ASI covers only the registered factory sector (not the whole
#' economy or the unorganised sector) and industry composition differs across
#' states, so cross-state gaps are not real-growth, productivity-causes-pay, or
#' whole-economy statements.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{region}{State/UT name, matching \code{\link{india_map}}'s \code{region}.}
#'   \item{indicator}{Short indicator id (\code{wages_per_worker}, \code{nva_per_worker}).}
#'   \item{indicator_lab}{Human-readable indicator label.}
#'   \item{value}{Value in Rs lakh per worker (current prices).}
#'   \item{unit}{Measurement unit (\code{"Rs lakh/worker"}).}
#'   \item{universe}{Population the indicator refers to.}
#'   \item{higher_is}{Whether a higher value is a \code{"better"} or \code{"worse"} outcome.}
#'   \item{round}{Survey round, \code{"ASI 2023-24 (current prices)"}.}
#'   \item{source}{Provenance string.}
#' }
#' @source ASI 2023-24, MoSPI - Table 4 "Estimate of some important
#'   characteristics by State" (All Industries), MoSPI NADA catalogue 256;
#'   released under the Revised Guidelines for Statistical Data Dissemination
#'   (GSDD, in accordance with NDSAP, 2012). See \code{data-raw/ASI_SOURCE.md}.
#'   Built by \code{data-raw/build_asi.R}.
#' @examples
#' \dontrun{
#' data(asi_state)
#' subset(asi_state, indicator == "nva_per_worker")
#' }
"asi_state"
