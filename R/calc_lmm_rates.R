#' Calculate station- or site-level rates of change from SET or MH data using
#' generalized linear mixed models
#'
#' This function takes a data frame of raw SET or MH data and calculates a rate
#' of surface elevation change (for SET data) or vertical accretion (for MH
#' data) at either the station- or site-level using a generalized linear mixed
#' model as implemented in the package \pkg{nlme}.
#'
#' @inheritParams calc_change_cumu
#' @param drop_SET_direction boolean. Optional; for use with level = "station"
#'   if the model(s) fail to converge.
#' @param drop_station_code boolean. Optional; for use with level = "site" if
#'   the model(s) fail to converge.
#'
#' @inheritSection calc_change_cumu Data Requirements
#'
#' @details For station-level SET data, random intercepts are calculated for
#'   each pin nested within SET direction. For site-level SET data, random
#'   intercepts are calculated for each pin nested within each SET direction
#'   nested within each SET station. For station-level MH data, random
#'   intercepts are calculated for each duplicate marker horizon (i.e., plot A,
#'   B, C, etc.). For site-level MH data, random intercepts are calculated for
#'   each duplicate marker horizon nested within each SET station. For MH data,
#'   duplicate core readings ('core_measurement_number') are first averaged to
#'   the marker horizon-level.
#'
#' @returns For SET data, returns a data frame of station- or site-level rates
#'   of surface elevation change. For MH data, returns a data frame of station-
#'   or site-level rates of surface accretion. Note that units for rates of
#'   change are mm/yr.
#'
#' @note Stations with fewer than 3 measurement dates are excluded from the
#'   calculation of rates.
#'
#' @seealso \code{\link{calc_linear_rates}}, \code{\link[nlme]{lme}}
#'
#' @export
#'
#' @import purrr
#' @import nlme
#' @import dplyr
#' @importFrom performance r2
#' @importFrom tidyr fill
#' @importFrom tibble add_row
#'
#' @examples
#' # Defaults to station-level rates
#' calc_lmm_rates(example_sets)
#'
#' # Site-level rates
#' calc_lmm_rates(example_mh, level = "site")
#'
#' # Can also be used with pipes to include/exclude specific stations, dates, etc.
#' ## exclude station M11-3 from calculations of rates
#' example_sets %>%
#'     filter(station_code != "M11-3") %>%
#'     calc_lmm_rates(., level = "site")
#'
#' ## Exclude data from after 2016 from calculations of rates
#' example_sets %>%
#'     filter(event_date_UTC < as.Date("2016-01-01")) %>%
#'     calc_lmm_rates(., level = "station")
#'
#' ## Define custom groups for calculating rates
#' example_sets %>%
#'     group_by(set_type) %>%
#'     calc_lmm_rates(., level = "site")
#'
calc_lmm_rates <- function(data, level = "station", override_site_corrections = FALSE, drop_SET_direction = FALSE, drop_station_code = FALSE){

    # determine if the data is SET or MH
    data_type <- detect_data_type(data)

    if (data_type != "SET" & data_type != "MH") {
        stop(paste0("Data must be either valid SET or MH data. See 'data requirements' in the documentation for `calc_change_cumu()`."))
    } else if (data_type == "SET") {

        # use linear mixed model regression to get a rate of change for each station
        lmm_rates_set <- data %>%
            # apply proper station groupings for NCBN data
            { if(override_site_corrections == FALSE)
                mutate(.,site_name = NPSETr::correct_site_groups(station_code = station_code, site_name = site_name))

                else .} %>%

            # convert to standardized pin heights to account for 6" extensions used at some of these networks' sites
            group_by(., network_code, park_code, site_name, station_code, SET_direction, pin_position, .add = TRUE) %>%
            filter(!is.na(pin_height_mm)) %>%
            mutate(pin_height_mm = if_else(network_code %in% c("NCBN", "NCRN", "NETN", "SFCN") & !is.na(SET_offset_mm) & !is.na(pin_length_mm),
                                           1000 + (SET_offset_mm-(pin_length_mm - pin_height_mm)),
                                           pin_height_mm)) %>%

            # first get cumulative change for each pin
            mutate(event_date_UTC = as.Date(event_date_UTC),
                   first_pin_height = pin_height_mm[event_date_UTC == min(event_date_UTC[!is.na(pin_height_mm)])],
                   cumu = pin_height_mm - first_pin_height) %>%


            # limit calculation of rates to stations with at least 3
            # measurements - this is the min # needed to calculate a linear rate
            mutate(date_count = n_distinct(event_date_UTC)) %>%
            filter(date_count >= 3) %>%
            select(-date_count) %>%

            # nest data and run model
            ungroup() %>%
            {if (level == "station")
                group_by(., network_code, park_code, site_name, station_code)
                else if (level == "site")
                    group_by(., network_code, park_code, site_name)} %>%
            mutate(.,
                   data_type = "SET",
                   first_date = min(event_date_UTC[!is.na(cumu)]), # convert dates to decimal year since first date
                   date_num = as.numeric(event_date_UTC - first_date)/365.25) %>%
            tidyr::nest() %>%
            {if (level == "station")
                {if (drop_SET_direction == FALSE)
                    mutate(.,
                        lmm_model = purrr::map(data, ~nlme::lme(cumu ~ date_num, random = ~1|SET_direction/pin_position, data = .x)))
                    else if (drop_SET_direction == TRUE)
                        mutate(.,
                               lmm_model = purrr::map(data, ~nlme::lme(cumu ~ date_num, random = ~1|pin_position, data = .x)))}
                else if (level == "site")
                    {if (drop_station_code == FALSE)
                            mutate(.,
                                lmm_model = purrr::map(data, ~nlme::lme(cumu ~ date_num, random = ~1|station_code/SET_direction/pin_position, data = .x)))
                        else if (drop_station_code == TRUE)
                            mutate(.,
                                lmm_model = purrr::map(data, ~nlme::lme(cumu ~ date_num, random = ~1|SET_direction/pin_position, data = .x)))
                        }} %>%
            mutate(.,
                   lmm_model_summary = purrr::map(lmm_model, ~summary(.)),
                   rate = purrr::map_dbl(lmm_model_summary, ~coefficients(.)[2]),
                   intc = purrr::map_dbl(lmm_model_summary, ~coefficients(.)[1]),
                   rate_se = purrr::map_dbl(lmm_model_summary, ~coefficients(.)[4]),
                   rate_r2 = purrr::map(lmm_model, ~performance::r2(.)),
                   rate_p = purrr::map_dbl(lmm_model, ~summary(.)$tTable[, "p-value"][2]),
                   ci = purrr::map(lmm_model, ~intervals(., level = 0.95, which = "fixed")[1]$fixed),
                   ci_low = purrr::map_dbl(ci, ~.[2]),
                   ci_high =  purrr::map_dbl(ci, ~.[6]),
                   ci_abs_value = abs(ci_high - rate),
                   date_count = purrr::map_int(data, ~n_distinct(.x$event_date_UTC))
            ) %>%
            {if (level == "station")
                mutate(.,
                       rate_level = "station")
                else if (level == "site")
                    mutate(.,
                           rate_level = "site")}

        return(lmm_rates_set)

    } else if (data_type == "MH"){

        # first calculate cumulative change for each station
        lmm_rates_mh <- suppressMessages(data %>%
                                             # apply proper station groupings for NCBN data
                                             { if(override_site_corrections == FALSE)
                                                 mutate(.,site_name = NPSETr::correct_site_groups(station_code = station_code, site_name = site_name))
                                                 else .} %>%

                                             # first average all core measurements from each date
                                             mutate(., event_date_UTC = as.Date(event_date_UTC),
                                                    established_date = as.Date(established_date)) %>%
                                             group_by(network_code, park_code, site_name, station_code, marker_horizon_name, event_date_UTC, established_date, .add = TRUE) %>%
                                             summarise(cumu = mean(core_measurement_depth_mm, na.rm = TRUE)) %>%

                                             # find stations/sites where established_date is not the first row
                                             drop_groups2(., event_date_UTC) %>%
                                             drop_groups2(., established_date) %>%
                                             mutate(first_date = event_date_UTC[event_date_UTC == min(event_date_UTC[!is.na(cumu)])],
                                                    first_date_match = if_else(first_date == established_date, "y", "n")) %>%

                                             { . ->> detect_replacement_mh} %>% # get intermediate df that shows if there any replacement MHs

                                             group_modify(~{
                                                 if(all(.x$first_date_match == "y")) # if established_date = first row, do nothing
                                                     .x
                                                 else if(any(.x$first_date_match == "n")) # if established date is not first row, add a blank row
                                                     tibble::add_row(.x, first_date_match = "n", .before = 0)
                                             }) %>%
                                             mutate(across(c(cumu), # fill in cumu with 0 for the blank rows
                                                           ~if_else(if_all(.cols = c(
                                                               event_date_UTC,
                                                               established_date,
                                                               cumu,
                                                               first_date),
                                                               .f = is.na),
                                                               0,
                                                               cumu))) %>%
                                             tidyr::fill(established_date, .direction = "up") %>% # fill established date in new first rows using the station's established date value in other rows
                                             mutate(first_date = if_else(first_date_match == "n", established_date, first_date),
                                                    event_date_UTC = if_else(is.na(event_date_UTC) & first_date_match == "n" & cumu == 0, established_date, event_date_UTC), # fill event_date_UTC in new first rows using the station's established date
                                                    group_min_first_date = min(established_date, na.rm = TRUE)) %>% # add last value from previous group of MH plots to the replacement plots
                                             arrange(network_code, park_code, site_name, station_code, event_date_UTC) %>%
                                             mutate(previous_plot_group_cumu = replace(cumu, established_date != group_min_first_date, NA)) %>%
                                             tidyr::fill(previous_plot_group_cumu) %>%
                                             mutate(cumu = if_else(established_date != group_min_first_date, cumu + previous_plot_group_cumu, cumu),
                                                    data_type = "MH",
                                                    date_num = as.numeric(event_date_UTC - first_date)/365.25)) %>% # convert dates to decimal year since first date

            # limit calculation of rates to stations with at least 3
            # measurements - this is the min # needed to calculate a linear rate
            mutate(date_count = n_distinct(event_date_UTC)) %>%
            filter(date_count >= 3) %>%
            select(-c(date_count)) %>%

            # nest data and run model
            ungroup() %>%
            {if (level == "station")
                group_by(., network_code, park_code, site_name, station_code)
                else if (level == "site")
                    group_by(., network_code, park_code, site_name)} %>%
            mutate(.,
                   first_date = min(event_date_UTC[!is.na(cumu)]), # convert dates to decimal year since first date
                   date_num = as.numeric(event_date_UTC - first_date)/365.25) %>%
            tidyr::nest() %>%
            {if (level == "station")
                mutate(.,
                       lmm_model = purrr::map(data, ~nlme::lme(cumu ~ date_num, random = ~1|marker_horizon_name, data = .x)))
                else if (level == "site")
                    {if (drop_station_code == FALSE)
                        mutate(.,
                            lmm_model = purrr::map(data, ~nlme::lme(cumu ~ date_num, random = ~1|station_code/marker_horizon_name, data = .x)))
                        else if (drop_station_code == TRUE)
                            mutate(.,
                                   lmm_model = purrr::map(data, ~nlme::lme(cumu ~ date_num, random = ~1|marker_horizon_name, data = .x)))
                    }} %>%
            mutate(.,
                   lmm_model_summary = purrr::map(lmm_model, ~summary(.)),
                   rate = purrr::map_dbl(lmm_model_summary, ~coefficients(.)[2]),
                   intc = purrr::map_dbl(lmm_model_summary, ~coefficients(.)[1]),
                   rate_se = purrr::map_dbl(lmm_model_summary, ~coefficients(.)[4]),
                   rate_r2 = purrr::map(lmm_model, ~performance::r2(.)),
                   rate_p = purrr::map_dbl(lmm_model, ~summary(.)$tTable[, "p-value"][2]),
                   ci = purrr::map(lmm_model, ~intervals(., level = 0.95, which = "fixed")[1]$fixed),
                   ci_low = purrr::map_dbl(ci, ~.[2]),
                   ci_high =  purrr::map_dbl(ci, ~.[6]),
                   ci_abs_value = abs(ci_high - rate),
                   date_count = purrr::map_int(data, ~n_distinct(.x$event_date_UTC))
            ) %>%
            {if (level == "station")
                mutate(.,
                       rate_level = "station")
                else if (level == "site")
                    mutate(.,
                           rate_level = "site")}

        return(lmm_rates_mh)
    }
}
