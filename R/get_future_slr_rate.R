#' Get park-specific future predicted sea-level rise rates from Sweet et al.
#' 2022
#'
#' This function gets future predicted sea-level data for the nearest NOAA tide
#' gauge based on calculations provided in Sweet et al. 2022 for the 5th
#' National Climate Assessment and then calculates future predicted rates of
#' sea-level rise by a specific decade. Defaults to predicted future rates for
#' the decade between 2090 and 2100.
#'
#' @inheritParams get_future_sea_level_data
#' @param decade number (optional). The end year of the decade for predicted
#'   future slr rates. Must be a multiple of 10 years (e.g. 2040, 2070, etc.)
#'   and only works for decades between 2030 to 2150.
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
#' @seealso [get_future_sea_level_data()]
#'
#' @returns A data frame with the future predicted sea-level rates (column
#'   "future_slr_rate" in mm/yr) by the end of the specified 10-year period
#'   based on Sweet et al. 2022.
#'
#' @import dplyr
#' @importFrom readr read_csv
#'
#' @export
#'
#' @examples
#' get_future_slr_rate(park = "ASIS", scenario_percentile = "MED")
#'
get_future_slr_rate <- function(park, nauset = FALSE, decade = 2100, scenario_percentile = NULL) {

    if (decade < 2030 | decade > 2150) {
        stop("must pick a decade that ends after 2030 and before 2150")
    }

    future_slr_data <- get_future_sea_level_data(park = park, nauset = nauset, scenario_percentile = scenario_percentile)

    data <- future_slr_data %>%
        {if (!is.null(scenario_percentile))
            filter(., str_detect(Scenario, scenario_percentile))
        else
            .} %>%
        mutate(.,
           future_slr_rate = (!!sym(paste0("rsl_", decade, "_mm")) - !!sym(paste0("rsl_", decade - 10, "_mm")))/10,
           object_type = "future slr rate") %>%
        select(park_code:scenario_perc, !!sym(paste0("rsl_", decade, "_mm")), !!sym(paste0("rsl_", decade - 10, "_mm")), future_slr_rate, object_type)

    return(data)
}
