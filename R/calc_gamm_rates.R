#' Calculate station- or site-level rates of change from SET or MH data using
#' generalized additive mixed models (GAMM).
#'
#' This function takes a data frame of raw SET or MH data and calculates a rate
#' of surface elevation change (for SET data) or vertical accretion (for MH
#' data) at either the station- or site-level using a generalized additive mixed
#' models (GAMM) model as implemented in the package \pkg{mgcv}.
#'
#' @inheritParams calc_gam_rates
#'
#' @inheritSection calc_change_cumu Data Requirements
#'
#' @details For station-level SET data, random effect smooths are calculated for
#'   each pin nested within SET direction. For site-level SET data, random
#'   effect smooths are calculated for each pin nested within each SET direction
#'   nested within each SET station. For station-level MH data, random effect
#'   smooths are calculated for each duplicate marker horizon (i.e., plot A, B,
#'   C, etc.). For site-level MH data, random effect smooths are calculated for
#'   each duplicate marker horizon nested within each SET station. For MH data,
#'   duplicate core readings ('core_measurement_number') are first averaged to
#'   the marker horizon-level. GAMM models use thin-plate regression splines
#'   s(bs = "tp") for the smooth term for time and random effects s(bs = "re")
#'   for the various random effect terms - see \code{\link[mgcv]{smooth.terms}}.
#'   Note that rates of change, standard errors, and confidence intervals are
#'   calculated as the mean of 200-equally spaced first derivatives of the
#'   function representing the smooth term for time via the 'derivatives'
#'   function from the gratia package - see \code{\link[gratia]{derivatives}}.
#'
#' @returns For SET data, returns a data frame of station- or site-level rates
#'   of surface elevation change. For MH data, returns a data frame of station-
#'   or site-level rates of surface accretion. Note that units for rates of
#'   change are mm/yr.
#'
#' @inherit calc_lmm_rates note
#'
#' @references Feher, L.C., Osland, M.J., Johnson, D.J., Grace, J.B.,
#'   Guntenspergen, G.R., Stewart, D.R., Coronado-Molina, C., and Sklar, F.H.,
#'   2024. Nonlinear patterns of surface elevation change in coastal wetlands:
#'   the value of generalized additive models for quantifying rates of change.
#'   Estuaries and Coasts, 47:1893-1902.
#'   \url{https://doi.org/10.1007/s12237-023-01268-w}
#'
#' @seealso \code{\link{calc_lmm_rates}},
#'   \code{\link{calc_gam_rates}}, \code{\link[mgcv]{gam}},
#'   \code{\link[gratia]{derivatives}}
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#' @import mgcv
#' @importFrom gratia derivatives
#' @importFrom tibble add_row
#' @importFrom tidyr fill
#'
#' @examples
#' # Defaults to station-level rates
#' calc_gamm_rates(example_sets)
#'
#' # Site-level cumulative change
#' calc_gamm_rates(example_mh, level = "site")
#'
#' # Can also be used with pipes to include/exclude specific stations, dates, etc.
#' ## exclude station M11-3 from calculations of gamm rates
#' example_sets %>%
#'     filter(station_code != "M11-3") %>%
#'     calc_gamm_rates(., level = "site")
#'
#' ## Exclude data from after 2016 from calculations of gamm rates
#' example_sets %>%
#'     filter(event_date_UTC < as.Date("2016-01-01")) %>%
#'     calc_gamm_rates(., level = "station")
#'
#' ## Define custom groups for calculating gamm rates
#' example_sets %>%
#'     group_by(set_type) %>%
#'     calc_gamm_rates(., level = "site")
#'
calc_gamm_rates <- function(data, level = "station", kval = 10, override_site_corrections = FALSE){

    # determine if the data is SET or MH
    data_type <- detect_data_type(data)

    if (data_type != "SET" & data_type != "MH") {
        stop(paste0("Data must be either valid SET or MH data. See 'data requirements' in the documentation for `calc_change_cumu()`."))
    } else if (data_type == "SET") {

        # use linear mixed model regression to get a rate of change for each station
        gamm_rates_set <- data %>%
            # apply proper station groupings for NCBN data
            { if(override_site_corrections == FALSE)
                mutate(.,site_name = correct_site_groups(station_code = station_code, site_name = site_name))

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

            # make pin_position and pin_position nested within SET_direction factors for use with s(bs = "re")
            mutate(SET_direction = as.factor(SET_direction),
                   pin_position = as.factor(paste0(SET_direction, pin_position))) %>%

            # nest data and run model
            ungroup() %>%
            {if (level == "station")
                group_by(., network_code, park_code, site_name, station_code)
                else if (level == "site")
                    mutate(.,
                           station_code = as.factor(station_code), # make station_code a factor for use with s(bs = "re")
                           SET_direction = as.factor(paste0(station_code, SET_direction)),
                           pin_position = as.factor(paste0(station_code, SET_direction, pin_position))) %>% # make pin_position nested within SET_direction nested within station_code a factor for use with s(bs = "re")
                    group_by(network_code, park_code, site_name)} %>%
            mutate(.,
                   data_type = "SET",
                   first_date = min(event_date_UTC[!is.na(cumu)]), # convert dates to decimal year since first date
                   date_num = as.numeric(event_date_UTC - first_date)/365.25) %>%

            tidyr::nest() %>%
            {if (level == "station")
                mutate(.,
                       gamm_model = purrr::map(data, ~mgcv::gam(cumu ~  s(date_num, k = kval) + s(SET_direction, pin_position, bs = "re") + s(SET_direction, bs = "re") + s(pin_position, bs = "re"), method = "REML", data = .x)))
                else if (level == "site")
                    mutate(.,
                           gamm_model = purrr::map(data, ~mgcv::gam(cumu ~ s(date_num, k = kval) + s(station_code, SET_direction, pin_position, bs = "re") + s(station_code, bs = "re") + s(SET_direction, bs = "re") + s(pin_position, bs = "re"), method = "REML", data = .x)))} %>%
            mutate(.,
                   gamm_model_summary = purrr::map(gamm_model, ~summary(.)),
                   deriv = purrr::map(gamm_model, ~gratia::derivatives(., n = 200)),
                   rate = purrr::map_dbl(deriv, ~mean(.$.derivative)),
                   intc = purrr::map_dbl(gamm_model_summary, ~.$p.coeff),
                   rate_se = purrr::map_dbl(deriv, ~mean(.$.se)),
                   rate_r2 = purrr::map_dbl(gamm_model_summary, ~.$r.sq),
                   rate_p = purrr::map_dbl(gamm_model_summary, ~.$s.pv[1]),
                   ci_low = purrr::map_dbl(deriv, ~mean(.$.lower_ci)),
                   ci_high =  purrr::map_dbl(deriv, ~mean(.$.upper_ci)),
                   ci_abs_value = abs(ci_high - rate),
                   edf = purrr::map_dbl(gamm_model_summary, ~.$edf[1]),
                   date_count = purrr::map_int(data, ~n_distinct(.x$event_date_UTC))
            ) %>%
            {if (level == "station")
                mutate(.,
                       rate_level = "station")
                else if (level == "site")
                    mutate(.,
                           rate_level = "site")}

        return(gamm_rates_set)

    } else if (data_type == "MH"){

        # first calculate cumulative change for each station
        gamm_rates_mh <- suppressMessages(data %>%
                                             # apply proper station groupings for NCBN data
                                             { if(override_site_corrections == FALSE)
                                                 mutate(.,site_name = correct_site_groups(station_code = station_code, site_name = site_name))
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

            # make marker_horizon_name a factor for use with s(bs = "re")
            mutate(marker_horizon_name = as.factor(marker_horizon_name)) %>%

            # nest data and run model
            ungroup() %>%
            {if (level == "station")
                group_by(., network_code, park_code, site_name, station_code)
                else if (level == "site")
                    mutate(.,
                           station_code = as.factor(station_code),
                           marker_horizon_name = as.factor(paste0(station_code, marker_horizon_name))) %>% # make marker_horizon_name nested within station_code a factor for use with s(bs = "re")
                    group_by(., network_code, park_code, site_name)} %>%
            mutate(.,
                   first_date = min(event_date_UTC[!is.na(cumu)]), # convert dates to decimal year since first date
                   date_num = as.numeric(event_date_UTC - first_date)/365.25) %>%
            tidyr::nest() %>%
            {if (level == "station")
                mutate(.,
                       gamm_model = purrr::map(data, ~mgcv::gam(cumu ~ s(date_num, k = kval) + s(marker_horizon_name, bs = "re"), method = "REML", data = .x)))
                else if (level == "site")
                    mutate(.,
                           gamm_model = purrr::map(data, ~mgcv::gam(cumu ~ s(date_num, k = kval) + s(station_code, marker_horizon_name, bs = "re") + s(station_code, bs = "re") + s(marker_horizon_name, bs = "re"), method = "REML", data = .x)))} %>%
            mutate(.,
                   gamm_model_summary = purrr::map(gamm_model, ~summary(.)),
                   deriv = purrr::map(gamm_model, ~gratia::derivatives(., n = 200)),
                   rate = purrr::map_dbl(deriv, ~mean(.$.derivative)),
                   intc = purrr::map_dbl(gamm_model_summary, ~.$p.coeff),
                   rate_se = purrr::map_dbl(deriv, ~mean(.$.se)),
                   rate_r2 = purrr::map_dbl(gamm_model_summary, ~.$r.sq),
                   rate_p = purrr::map_dbl(gamm_model_summary, ~.$s.pv[1]),
                   ci_low = purrr::map_dbl(deriv, ~mean(.$.lower_ci)),
                   ci_high =  purrr::map_dbl(deriv, ~mean(.$.upper_ci)),
                   ci_abs_value = abs(ci_high - rate),
                   edf = purrr::map_dbl(gamm_model_summary, ~.$edf[1]),
                   date_count = purrr::map_int(data, ~n_distinct(.x$event_date_UTC))
            ) %>%
            {if (level == "station")
                mutate(.,
                       rate_level = "station")
                else if (level == "site")
                    mutate(.,
                           rate_level = "site")}

        return(gamm_rates_mh)
    }
}
