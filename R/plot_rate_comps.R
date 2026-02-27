#' Plot comparison of station- or site-level surface elevation change or
#' vertical accretion rates
#'
#' This function creates a 'TIE fighter' style plot that can be used to visually
#' compare station- or site-level linear rates of surface elevation change or
#' vertical accretion.
#'
#' Accepts either a data frame of either raw SET or MH data (`data` argument) or
#' a data frame of pre-calculated station- or site-level rates of surface
#' elevation change or vertical accretion (`rates` argument).
#'
#' @param data A data frame (optional). A data frame of raw SET or MH data. See
#'   details below for requirements.
#'
#' @param rates A data frame (optional). A data frame of station- or site-level
#'   rates of surface elevation change. See details below for requirements.
#'
#' @inheritParams calc_change_cumu
#'
#' @inheritSection calc_change_cumu Data Requirements
#'
#' @inheritSection calc_change_cumu Details
#'
#' @section Data Requirements for pre-calculated rates of change:
#'
#'   This function takes a data frame of either raw SET data (`data`) or a
#'   user-created data frame of station- or site-level rates of surface
#'   elevation change (`rates`).
#'
#'   If supplying a user created data frame to  `rates` and `level = station`,
#'   must be a data frame of station-level rates of surface elevation change or
#'   vertical accretion with 1 row per station and the following columns, named
#'   exactly: station_code, rate, and rate_se. The "rate" and "rate_se" columns
#'   should represent the station-level rates and their standard errors
#'   (respectively).
#'
#'   If supplying a user created data frame to `rates` and `level = site`, must
#'   be a data frame of site-level rates of surface elevation change or vertical
#'   accretion with 1 row per site and the following columns, named exactly:
#'   site_name, rate, and rate_se. The "rate" and "rate_se" columns should
#'   represent the site-level rates and their standard errors (respectively).
#'
#' @inherit plot_cumu note
#'
#' @return a ggplot object: depending on the type of data supplied, x-axis
#'   values are rates of either (a) cumulative surface elevation change or (b)
#'   vertical accretion. If `level = "station"` (default), the y-axis is station
#'   codes. If `level = "site"`, the y-axis is site names.
#'
#' @seealso \code{\link{calc_change_cumu}}, \code{\link{calc_linear_rates}},
#'   \code{\link{plot_cumu}}
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#'
#' # Default plot gives station-level rates
#' plot_rate_comps(data = example_sets)
#'
#' # Example with a user-supplied data frame of station-level rates
#' example_rates <- data.frame("station" = c("station_1", "station_2", "station_3"),
#'                             "rate" = c(3.2, 4.0, 5.4),
#'                             "se_rate" = c(1, 0.5, 0.25))
#'
#' plot_rate_comps(rates = example_rates)
#'
#' # Site-level rates
#' plot_rate_comps(data = example_sets, level = "site")
#'
plot_rate_comps <- function(data = NULL, rates = NULL, level = "station"){

    if (level == "station") {

        # if supplying df of rates, make sure that the specified columns exist
        if (!is.null(rates)) {
            if (!"station_code" %in% colnames(rates) | !"rate" %in% colnames(rates) | !"rate_se" %in% colnames(rates)) {
                stop(paste0("columns 'station_code', 'rate', and/or 'rate_se' were not found in '",
                            deparse(substitute(rates)), "'"))
            } else if (!is.numeric(rates$rate)) {
                rates_data <- rates %>%
                    mutate(rate = as.numeric(rate))
            } else if (!is.numeric(rates$rate_se)) {
                rates_data <- rates %>%
                    mutate(rate_se = as.numeric(rate_se))
            } else {
                rates_data <- rates
            }

            raw_data_type <- "unknown"

        } else if (is.null(rates)) {

            # make sure the data is valid SET or MH data
            raw_data_type <- detect_data_type(data)

            if (raw_data_type != "SET" & raw_data_type != "MH") {
                stop(paste0("Data must be valid SET or MH data. See 'data requirements'."))
            } else {
                rates_data <- calc_linear_rates(data, level = "station") # if supplying a raw data df, use calc_linear_rates to get rates
            }
        }

        # assemble plot
        groups <- rates_data %>%
            attr(., "groups") %>%
            select(-c(network_code, park_code, site_name, ".rows")) %>%
            colnames()

        rates_data <- rates_data %>%
            tidyr::unite("grouping", all_of(groups), remove = FALSE)

        ggplot2::ggplot(data = rates_data, aes(x = rate, y = grouping)) +
            ggplot2::geom_vline(aes(xintercept = 0), color = "gray70", linetype = "dashed") +
            ggplot2::geom_errorbar(aes(y = grouping, xmin = rate - rate_se, xmax = rate + rate_se), color = "gray55", linewidth = 1) +
            ggplot2::geom_point(size = 3, color = "red3") +
            {if(raw_data_type == "SET")
                ggplot2::labs(title = "Rates of surface elevation change ± 1 standard error (mm/yr)", x = "Rate of surface elevation change (mm/yr)", y = "Station")
                else if(raw_data_type == "MH")
                    ggplot2::labs(title = "Rates of vertical accretion change ± 1 standard error (mm/yr)", x = "Rate of vertical accretion (mm/yr)", y = "Station")
                else if(raw_data_type == "unknown")
                    ggplot2::labs(title = "Rates of change ± 1 standard error (mm/yr)", x = "Rate of change (mm/yr)", y = "Station")}

    } else if (level == "site") {

        # if supplying df of rates, make sure that the specified columns exist
        if (!is.null(rates)) {
            if (!"site_name" %in% colnames(rates) | !"rate" %in% colnames(rates) | !"rate_se" %in% colnames(rates)) {
                stop(paste0("columns 'site_name', 'rate', and/or 'rate_se' were not found in '",
                            deparse(substitute(rates)), "'"))
            } else if (!is.numeric(rates$rate)) {
                rates_data <- rates %>%
                    mutate(rate = as.numeric(rate))
            } else if (!is.numeric(rates$rate_se)) {
                rates_dates <- rates %>%
                    mutate(rate_se = as.numeric(rate_se))
            } else {
                rates_data <- rates
            }

            raw_data_type <- "unknown"

        } else if (is.null(rates)) {

            # make sure the data is valid SET or MH data
            raw_data_type <- detect_data_type(data)

            if (raw_data_type != "SET" & raw_data_type != "MH") {
                stop(paste0("Data must be SET or MH data. See 'data requirements'."))
            } else {
                rates_data <- calc_linear_rates(data, level = "site") # if supplying a raw data df, use calc_linear_rates to get rates
            }
        }

        # assemble plot

        groups <- rates_data %>%
            attr(., "groups") %>%
            select(-c(network_code, park_code, ".rows")) %>%
            colnames()

        rates_data <- rates_data %>%
            tidyr::unite("grouping", all_of(groups), remove = FALSE)

        ggplot2::ggplot(data = rates_data, aes(x = rate, y = grouping)) +
            ggplot2::geom_vline(aes(xintercept = 0), color = "gray70", linetype = "dashed") +
            ggplot2::geom_errorbar(aes(y = grouping, xmin = rate - rate_se, xmax = rate + rate_se), color = "gray55", linewidth = 1) +
            ggplot2::geom_point(size = 3, color = "red3") +
            {if (raw_data_type == "SET")
                ggplot2::labs(title = "Rates of surface elevation change ± 1 standard error (mm/yr)", x = "Rate of surface elevation change (mm/yr)", y = "Site")
                else if (raw_data_type == "MH")
                    ggplot2::labs(title = "Rates of vertical accretion ± 1 standard error (mm/yr)", x = "Rate of vertical accretion (mm/yr)", y = "Site")
                else if (raw_data_type == "unknown")
                    ggplot2::labs(title = "Rates of change ± 1 standard error (mm/yr)", x = "Rate of change (mm/yr)", y = "Site")}

    }
}
