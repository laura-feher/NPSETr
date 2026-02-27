#' Calculate station- or site-level rates of change from SET or MH data using
#' generalized additive models (GAM)
#'
#' This function takes a data frame of raw SET or MH data and calculates a rate
#' of surface elevation change (for SET data) or vertical accretion (for MH
#' data) at either the station- or site-level using a GAM model as implemented
#' in the package \pkg{mgcv}.
#'
#' @inheritParams calc_change_cumu
#' @param kval The basis dimension for the smooth term for time; defaults to 10.
#'   See \code{\link[mgcv]{choose.k}}.
#'
#' @inheritSection calc_change_cumu Data Requirements
#'
#' @details Prior to calculation of rates, cumulative change is calculated via
#'   the function \code{\link{calc_change_cumu}} - see function
#'   documentation for details. GAM models use thin-plate regression splines (bs
#'   = "tp") - see \code{\link[mgcv]{smooth.terms}}. Note that rates of change,
#'   standard errors, and confidence intervals are calculated as the mean of
#'   200-equally spaced first derivatives of the function representing the
#'   smooth term via the 'derivatives' function from the gratia package - see
#'   \code{\link[gratia]{derivatives}}.
#'
#' @returns For SET data, returns a data frame of station- or site-level rates
#'   of surface elevation change. For MH data, returns a data frame of station-
#'   or site-level rates of surface accretion. Note that units for rates of
#'   change are mm/yr.
#'
#' @inherit calc_linear_rates note
#'
#' @references Feher, L.C., Osland, M.J., Johnson, D.J., Grace, J.B.,
#'   Guntenspergen, G.R., Stewart, D.R., Coronado-Molina, C., and Sklar, F.H.,
#'   2024. Nonlinear patterns of surface elevation change in coastal wetlands:
#'   the value of generalized additive models for quantifying rates of change.
#'   Estuaries and Coasts, 47:1893-1902.
#'   \url{https://doi.org/10.1007/s12237-023-01268-w}
#'
#' @seealso \code{\link{calc_change_cumu}},
#'   \code{\link{calc_linear_rates}},
#'   \code{\link{calc_lmm_rates}},
#'   \code{\link{calc_gamm_rates}}, \code{\link[mgcv]{gam}},
#'   \code{\link[gratia]{derivatives}}
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#' @import mgcv
#' @importFrom gratia derivatives
#'
#' @examples
#' # Defaults to station-level rates
#' calc_gam_rates(example_sets)
#'
#' # Site-level cumulative change
#' calc_gam_rates(example_mh, level = "site")
#'
#' # Can also be used with pipes to include/exclude specific stations, dates, etc.
#' ## exclude station M11-3 from calculations of gam rates
#' example_sets %>%
#'     filter(station_code != "M11-3") %>%
#'     calc_gam_rates(., level = "site")
#'
#' ## Exclude data from after 2016 from calculations of gam rates
#' example_sets %>%
#'     filter(event_date_UTC < as.Date("2016-01-01")) %>%
#'     calc_gam_rates(., level = "station")
#'
#' ## Define custom groups for calculating gam rates
#' example_sets %>%
#'     group_by(set_type) %>%
#'     calc_gam_rates(., level = "site")
#'
calc_gam_rates <- function(data, level = "station", kval = 10, override_site_corrections = FALSE){

    # determine if the data is SET or MH
    data_type <- detect_data_type(data)

    if (data_type != "SET" & data_type != "MH") {
        stop(paste0("Data must be either valid SET or MH data. See 'data requirements' in the documentation for `calc_change_cumu()`."))
    } else if (data_type == "SET") {

        # use gam to get a rate of change for each station
        gam_rates_set <- {if (level == "station")
        { if(override_site_corrections == FALSE)
            calc_change_cumu(data, level = "station")
            else
                calc_change_cumu(data, level = "station", override_site_corrections = TRUE)} %>% # first calculate cumulative change for each station
                group_by(., network_code, park_code, site_name, station_code, .add = TRUE)

            else if (level == "site")
            { if(override_site_corrections == FALSE)
                calc_change_cumu(data, level = "site")
                else
                    calc_change_cumu(data, level = "site", override_site_corrections = TRUE)} %>%
                group_by(., network_code, park_code, site_name, .add = TRUE)
        } %>%

            # limit calculation of rates to stations with at least 3
            # measurements - this is the min # needed to calculate a rate
            mutate(date_count = n_distinct(event_date_UTC)) %>%
            filter(date_count >= 3) %>%
            select(-date_count) %>%

            # nest data and run model
            tidyr::nest() %>%
            mutate(gam_model = purrr::map(data, ~mgcv::gam(mean_cumu ~ s(date_num, k = kval), data = ., method = "REML")),
                   gam_model_summary = purrr::map(gam_model, ~summary(.)),
                   deriv = purrr::map(gam_model, ~gratia::derivatives(., n = 200)),
                   rate = purrr::map_dbl(deriv, ~mean(.$.derivative)),
                   rate_se = purrr::map_dbl(deriv, ~mean(.$.se)),
                   intc = purrr::map_dbl(gam_model_summary, ~.$p.coeff),
                   rate_r2 = purrr::map_dbl(gam_model_summary, ~.$r.sq),
                   rate_p = purrr::map_dbl(gam_model_summary, ~.$s.pv),
                   ci_low = purrr::map_dbl(deriv, ~mean(.$.lower_ci)),
                   ci_high =  purrr::map_dbl(deriv, ~mean(.$.upper_ci)),
                   ci_abs_value = abs(ci_high - rate),
                   edf = purrr::map_dbl(gam_model_summary, ~.$edf),
                   date_count = purrr::map_int(data, ~n_distinct(.x$event_date_UTC))
            ) %>%
            {if (level == "station")
                mutate(.,
                       rate_level = "station")
                else if (level == "site")
                    mutate(.,
                           rate_level = "site")}

        return(gam_rates_set)

    } else if (data_type == "MH"){

        # first calculate cumulative change for each station
        gam_rates_mh <- {if (level == "station")
        { if(override_site_corrections == FALSE)
            calc_change_cumu(data, level = "station")
            else
                calc_change_cumu(data, level = "station", override_site_corrections = TRUE)} %>%
                group_by(., network_code, park_code, site_name, station_code, .add = TRUE)
            else if (level == "site")
            { if(override_site_corrections == FALSE)
                calc_change_cumu(data, level = "site")
                else
                    calc_change_cumu(data, level = "site", override_site_corrections = TRUE)} %>%
                group_by(., network_code, park_code, site_name, .add = TRUE)
        } %>%

            # limit calculation of rates to stations with at least 3
            # measurements - this is the min # needed to calculate a rate
            mutate(date_count = n_distinct(event_date_UTC)) %>%
            filter(date_count >= 3) %>%
            select(-c(date_count)) %>%

            # nest data and run model
            tidyr::nest() %>%
            mutate(gam_model = purrr::map(data, ~mgcv::gam(mean_cumu ~ s(date_num, k = kval), data = ., method = "REML")),
                   gam_model_summary = purrr::map(gam_model, ~summary(.)),
                   deriv = purrr::map(gam_model, ~gratia::derivatives(., n = 200)),
                   rate = purrr::map_dbl(deriv, ~mean(.$.derivative)),
                   rate_se = purrr::map_dbl(deriv, ~mean(.$.se)),
                   intc = purrr::map_dbl(gam_model_summary, ~.$p.coeff),
                   rate_r2 = purrr::map_dbl(gam_model_summary, ~.$r.sq),
                   rate_p = purrr::map_dbl(gam_model_summary, ~.$s.pv),
                   ci_low = purrr::map_dbl(deriv, ~mean(.$.lower_ci)),
                   ci_high =  purrr::map_dbl(deriv, ~mean(.$.upper_ci)),
                   ci_abs_value = abs(ci_high - rate),
                   edf = purrr::map_dbl(gam_model_summary, ~.$edf),
                   date_count = purrr::map_int(data, ~n_distinct(.x$event_date_UTC))
            ) %>%
            {if (level == "station")
                mutate(.,
                       rate_level = "station")
                else if (level == "site")
                    mutate(.,
                           rate_level = "site")}

        return(gam_rates_mh)
    }
}
