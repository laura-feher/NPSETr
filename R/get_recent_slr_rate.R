#' Calculate a park-specific recent rate of sea-level rise using sea-level data
#' from the nearest NOAA tide gauge.
#'
#' This function downloads sea-level data for the nearest NOAA tide gauge from
#' the tides and currents website
#' (\url{https://tidesandcurrents.noaa.gov/sltrends/}) and then calculates a
#' rate of sea-level rise for the specified time period. Defaults to the 19 year
#' time period for the most recent tidal epoch: 2001-2019.
#'
#' @inheritParams get_sea_level_data
#'
#' @param start_year integer; The start year for the recent SLR rate. Defaults
#'   to 2001.
#' @param end_year integer; The end year for the recent SLR rate. Defaults to
#'   2019.
#'
#' @details The recent rate of sea-level rise is calculated via linear
#'   regression using monthly mean sea-levels with the regular seasonal
#'   fluctuations removed. Sea-level data is downloaded from the NOAA tides and
#'   currents website \url{https://tidesandcurrents.noaa.gov/}. Note that
#'   monthly mean sea level values are relative to the most recent mean
#'   sea-level datum established by CO-OPS
#'   (\url{https://tidesandcurrents.noaa.gov/datum_options.html}).The value in
#'   the 'estimate' column for the 'yr' term represents the recent rate of
#'   sea-level rise, and the value in 'std.error' column represents the standard
#'   error of the recent sea-level rise rate.
#'
#' @returns A data frame with a park-specific recent rate of sea-level rise
#'   calculated using the sea-level data from the nearest NOAA tide gauge for
#'   the specified time period.
#'
#' @references Moon, J.A., Feher, L.C., Lane, T.C., Vervaeke, W.C., Osland,
#'   M.J., Head, D.M., Chivoiu, B.C., Stewart, D.R., Johnson, D.J., Grace, J.B.,
#'   and Metzger, K.L., 2022. Surface elevation change dynamics in coastal
#'   marshes along the Northwestern Gulf of Mexico: Anticipating effects of
#'   rising sea-level and intensifying hurricanes. Wetlands
#'   42(5):49.(\url{https://doi.org/10.1007/s13157-022-01565-3})
#'
#' @references Chivoiu, B., Osland, M.J., Collini, R., Martin, S., Tirpak, J.,
#'   and Wilson, B., 2020. Local sea level rise information sheets for Texas,
#'   Louisiana, Mississippi, Alabama and Florida: Northern Gulf of Mexico
#'   Sentinel Site Cooperative, U.S. Geological Survey and U.S. Fish and
#'   Wildlife Service: Mississippi-Alabama Sea Grant Consortium,
#'   (\url{https://placeslr.org/our-products/federally-managed-lands-two-pagers/})
#'
#' @seealso \code{\link{get_sea_level_data}}
#'
#' @import broom.mixed
#' @import tidyr
#' @importFrom readr read_csv
#' @importFrom nlme gls
#' @importFrom nlme corAR1
#' @importFrom nlme intervals
#' @import dplyr
#'
#' @export
#'
#' @examples
#' get_recent_slr_rate(park = "ASIS")
#'
get_recent_slr_rate <- function(park, nauset = FALSE, start_year = 2001, end_year = 2019) {

    slr_data <- get_sea_level_data(park = park, nauset = nauset, start_year = start_year, end_year = end_year)

    suppressWarnings(data <- slr_data %>%
        group_by(park_code, station_num) %>%
        tidyr::nest(data = everything(.)) %>%
        mutate(gls_mod = purrr::map(data, ~nlme::gls(Monthly_MSL_mm ~ yr, data = .x, correlation = nlme::corAR1())),
               tidied = purrr::map(gls_mod, broom.mixed::tidy),
               glanced = purrr::map(gls_mod, broom.mixed::glance),
               int = purrr::map(gls_mod, ~nlme::intervals(.x, levels = 0.95))) %>%
        tidyr::unnest(tidied, glanced) %>%
        mutate(object_type = "recent slr rate",
               confint = estimate - int[[1]][[1]][[2]]))

    return(data)
}

