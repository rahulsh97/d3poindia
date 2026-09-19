#' NFHS-5 state-level indicators (aggregate)
#'
#' Aggregate, published state-level indicators from India's National Family Health
#' Survey, Round 5 (NFHS-5, 2019-21). Contains **no microdata** and no person-level
#' records: only official state percentages. Long format, one row per
#' state x indicator.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{region}{State/UT name, matching \code{\link{india_map}}'s \code{region}.}
#'   \item{indicator}{Short indicator id (\code{women_anaemia}, \code{women_schooling10}).}
#'   \item{indicator_lab}{Human-readable indicator label.}
#'   \item{value}{Published state percentage (NFHS-5 total column).}
#'   \item{unit}{Measurement unit (\code{"\%"}).}
#'   \item{universe}{Population the indicator refers to.}
#'   \item{higher_is}{Whether a higher value is a \code{"better"} or \code{"worse"} outcome.}
#'   \item{round}{Survey round, \code{"NFHS-5 (2019-21)"}.}
#'   \item{source}{Provenance string.}
#' }
#' @source NFHS-5 state fact sheets, IIPS/MoHFW; Open Government Data Platform India
#'   (NDSAP). See \code{data-raw/NFHS_SOURCE.md}. Built by \code{data-raw/build_data.R}.
#' @examples
#' \dontrun{
#' data(nfhs_state)
#' subset(nfhs_state, indicator == "women_schooling10")
#' }
"nfhs_state"
