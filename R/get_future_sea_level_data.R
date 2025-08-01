#' Get park-specific future predicted sea-level data from Sweet et al. 2022
#'
#' This function gets future predicted sea-level data for the nearest NOAA tide
#' gauge based on calculations provided in Sweet et al. 2022 for the 5th
#' National Climate Assessment.
#'
#' @inheritParams get_sea_level_data
#'
#' @param scenario_percentile character (optional). For each of the 5 GMSL
#'   scenarios (identified by the rise amounts in meters by 2100--0.3 m , 0.5 m.
#'   1.0 m, 1.5 m and 2.0 m), there is a low, medium (med) and high value,
#'   corresponding to the 17th, 50th, and 83rd percentiles. One of `NULL`
#'   (default), `"LOW"`, `"MED"`, or `"HIGH"`. The default (`NULL`) returns all
#'   percentiles.
#'
#' @references Sweet, W.V., B.D. Hamlington, R.E. Kopp, C.P. Weaver, P.L.
#'   Barnard, D. Bekaert, W. Brooks, M. Craghan, G. Dusek, T. Frederikse, G.
#'   Garner, A.S. Genz, J.P. Krasting, E. Larour, D. Marcy, J.J. Marra, J.
#'   Obeysekera, M. Osler, M. Pendleton, D. Roman, L. Schmied, W. Veatch, K.D.
#'   White, and C. Zuzak, 2022: Global and Regional Sea Level Rise Scenarios for
#'   the United States: Updated Mean Projections and Extreme Water Level
#'   Probabilities Along U.S. Coastlines. NOAA Technical Report NOS 01. National
#'   Oceanic and Atmospheric Administration, National Ocean Service, Silver
#'   Spring, MD, 111 pp.
#'   https://oceanservice.noaa.gov/hazards/sealevelrise/noaa-nos-techrpt01-global-regional-SLR-scenarios-US.pdf
#'
#' @returns A data frame with the future predicted sea-level rise amounts
#'   (millimeters) by the end of each 10-year period between 2005 to 2150 based
#'   on Sweet et al. 2022.
#'
#' @import dplyr
#' @importFrom readr read_csv
#'
#' @export
#'
#' @examples
#' get_future_sea_level_data(park = "ASIS")
#'
#' get_future_sea_level_data(park = "CACO", nauset = TRUE, scenario_percentile = "MED")
#'
get_future_sea_level_data <- function (park, nauset = FALSE, scenario_percentile = NULL) {

    noaa_ids <- noaa_tide_gauges(park = park, nauset = nauset)

    suppressMessages(data <- future_slr_projections_sweet_2022 %>%
        filter(`PSMSL Site` == noaa_ids$PSMSL_Site) %>%
        mutate(park_code = park,
               slr_by_2100_m = as.numeric(str_sub(Scenario, 1, 3)),
               across(starts_with("RSL"), ~.x * 10),
               scenario_name = case_when(str_detect(Scenario, "0.3") ~ "low",
                                         str_detect(Scenario, "0.5") ~ "int_low",
                                         str_detect(Scenario, "1.0") ~ "int",
                                         str_detect(Scenario, "1.5") ~ "int_high",
                                         str_detect(Scenario, "2.0") ~ "high"),
               scenario_perc = str_extract(Scenario, "[A-Za-z]+"),
               object_type = "future sea level data") %>%
        {if (!is.null(scenario_percentile))
            filter(., str_detect(Scenario, scenario_percentile))
            else
                .} %>%
        rename_with(., ~paste0("rsl_", str_extract(.x, "\\d{4}"), "_mm"), .cols = starts_with("RSL")) %>%
        select(park_code, "PSMSL_Site" = `PSMSL Site`, "PSMSL_ID" = `PSMSL ID`, "NOAA_Name" = `NOAA Name`, "NOAA_ID" = `NOAA ID`, Scenario, scenario_name, slr_by_2100_m, scenario_perc, starts_with("rsl"), object_type))

    return(data)
}
