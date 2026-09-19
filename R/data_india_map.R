#' India state boundaries (SimpleMaps, CC BY 4.0)
#'
#' Simple-features boundaries for India's states and union territories, used for
#' choropleth mapping. Sourced from SimpleMaps under CC BY 4.0 and documented in
#' \code{data-raw/MAP_SOURCE.md}. A boundary depiction here is not an official
#' Government of India map.
#'
#' @format An \code{sf} data frame with 36 features and columns:
#' \describe{
#'   \item{region}{State/UT name, the join key to \code{\link{nfhs_state}}.}
#'   \item{state_code}{Numeric state code (character).}
#'   \item{geometry}{\code{sf} geometry, EPSG:4326.}
#' }
#' @source SimpleMaps (\url{https://simplemaps.com/gis/country/in}), CC BY 4.0.
#'   See \code{data-raw/MAP_SOURCE.md}. Built by \code{data-raw/build_data.R}.
#' @examples
#' \dontrun{
#' data(india_map)
#' plot(india_map["region"])
#' }
"india_map"
