#' Get the current park-specific "long-term" rate of sea-level rise published by
#' NOAA tides and currents
#'
#' This function downloads the current "long-term" rate of sea-level rise
#' published on the NOAA tides and currents website for the nearest tide gauge
#' (https://tidesandcurrents.noaa.gov/sltrends/).
#'
#' @inheritParams get_sea_level_data
#'
#' @details Rates of sea-level rise reported by NOAA are (generally) updated on
#'   an regular basis. The rates of sea-level rise (here referred to as the
#'   long-term rate) are based on the entire period of record for each NOAA
#'   gauge, however, the gauges vary signficantly in age and therefore this
#'   "long-term" rate may be based on a relatively short time period for some
#'   gauges. For example, the Ocean City MD gauge was installed in 1975 and thus
#'   the period of record for the gauge is 1975 - present (50 years in 2025), in
#'   contrast to the Boston gauge which was installed in 1921 and thus has a
#'   period of record of 1921 - present (104 years in 2025).
#'
#' @returns A data frame with the current park-specific "long-term" rate of
#'   sea-level rise published by NOAA.
#'
#' @import dplyr
#' @import stringr
#' @importFrom readr read_csv
#'
#' @export
#'
#' @examples
#' get_long_slr_rate(park = "ASIS")
#'
get_long_slr_rate <- function(park, nauset = FALSE) {

    noaa_ids <- noaa_tide_gauges(park = park, nauset = nauset)

    data <- as.data.frame(readr::read_csv("https://tidesandcurrents.noaa.gov/sltrends/data/USStationsLinearSeaLevelTrends.csv", show_col_types = FALSE, name_repair = make.names)) %>%
        rename_with(., ~stringr::str_replace_all(.x, "\\.{2,}", ".")) %>%
        rename_with(., ~stringr::str_replace_all(.x, "X", "Perc")) %>%
        filter(Station.ID == noaa_ids$station_num) %>%
        mutate(object_type = "long term slr rate")

    return(data)
}
