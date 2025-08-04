#' Calculate a park-specific recent rate of sea-level rise using sea-level data
#' from the nearest NOAA tide gauge.
#'
#' This function downloads sea-level data for the nearest NOAA tide gauge from
#' the tides and currents website (https://tidesandcurrents.noaa.gov/sltrends/)
#' and then calculates a rate of sea-level rise for the specified time period.
#' Defaults to the 19 year time period for the most recent tidal epoch:
#' 2001-2019.
#'
#' @inheritParams get_sea_level_data
#'
#' @details The recent rate of sea-level rise is calculated via linear
#'   regression using monthly mean sea-levels with the regular seasonal
#'   fluctuations removed. Sea-level data is downloaded from the NOAA tides and
#'   currents website https://tidesandcurrents.noaa.gov/. Note that monthly mean
#'   sea level values are relative to the most recent mean sea-level datum
#'   established by CO-OPS
#'   (https://tidesandcurrents.noaa.gov/datum_options.html).The value in the
#'   'estimate' column for the 'yr' term represents the recent rate of sea-level
#'   rise, and the value in 'std.error' column represents the standard error of
#'   the recent sea-level rise rate.
#'
#' @returns A data frame with a park-specific recent rate of sea-level rise
#'   calculated using the sea-level data from the nearest NOAA tide gauge for
#'   the specified time period.
#'
#' @seealso [get_sea_level_data()]
#'
#' @import dplyr
#' @import broom.mixed
#' @importFrom readr read_csv
#' @importFrom tidyr unnest
#' @importFrom nlme gls
#' @importFrom nlme corAR1
#' @importFrom nlme intervals
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
        nest(data = everything(.)) %>%
        mutate(gls_mod = map(data, ~nlme::gls(Monthly_MSL_mm ~ yr, data = .x, correlation = nlme::corAR1())),
               tidied = map(gls_mod, broom.mixed::tidy),
               glanced = map(gls_mod, broom.mixed::glance),
               int = map(gls_mod, ~nlme::intervals(.x, levels = 0.95))) %>%
        tidyr::unnest(tidied, glanced) %>%
        mutate(object_type = "recent slr rate",
               confint = estimate - int[[1]][[1]][[2]]))

    return(data)
}

