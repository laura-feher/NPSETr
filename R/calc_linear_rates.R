#' Calculate station- or site-level linear rates of change from SET or MH data
#'
#' This function takes a data frame of raw SET or MH data and calculates a
#' linear rate of surface elevation change (for SET data) or vertical accretion
#' (for MH data) at either the station- or site-level.
#'
#' @inheritParams calc_change_cumu
#'
#' @inheritSection calc_change_cumu Data Requirements
#'
#' @inheritSection calc_change_cumu Details
#'
#' @return For SET data, returns a data frame of station- or site-level rates of
#'   surface elevation change. For MH data, returns a data frame of station- or
#'   site-level rates of surface accretion. Note that units for rates of change
#'   are mm/yr.
#'
#' @note Stations with fewer than 3 measurement dates are excluded from the
#'   calculation of rates. Cumulative change is calculated via the function
#'   \code{\link{calc_change_cumu}} - see function documentation for details.
#'
#' @seealso \code{\link{calc_change_cumu}}
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#'
#' @examples
#' # Defaults to station-level rates
#' calc_linear_rates(example_sets)
#'
#' # Site-level cumulative change
#' calc_linear_rates(example_mh, level = "site")
#'
#' # Can also be used with pipes to include/exclude specific stations, dates, etc.
#' ## exclude station M11-3 from calculations of linear rates
#' example_sets %>%
#'     filter(station_code != "M11-3") %>%
#'     calc_linear_rates(., level = "site")
#'
#' ## Exclude data from after 2016 from calculations of linear rates
#' example_sets %>%
#'     filter(event_date_UTC < as.Date("2016-01-01")) %>%
#'     calc_linear_rates(., level = "station")
#'
#' ## Define custom groups for calculating linear rates
#' example_sets %>%
#'     group_by(set_type) %>%
#'     calc_linear_rates(., level = "site")
#'
calc_linear_rates <- function(data, level = "station", override_site_corrections = FALSE){

    # determine if the data is SET or MH
    data_type <- NPSETr::detect_data_type(data)

    if (data_type != "SET" & data_type != "MH") {
        stop(paste0("Data must be either valid SET or MH data. See 'data requirements' in the documentation for `calc_change_cumu()`."))
    } else if (data_type == "SET") {

        # use linear regression to get a rate of change for each station
        linear_rates_set <- {if (level == "station")
        { if(override_site_corrections == FALSE)
            NPSETr::calc_change_cumu(data, level = "station")
            else
                NPSETr::calc_change_cumu(data, level = "station", override_site_corrections = TRUE)} %>% # first calculate cumulative change for each station
                group_by(., network_code, park_code, site_name, station_code, .add = TRUE)

            else if (level == "site")
            { if(override_site_corrections == FALSE)
                NPSETr::calc_change_cumu(data, level = "site")
                else
                    NPSETr::calc_change_cumu(data, level = "site", override_site_corrections = TRUE)} %>%
                group_by(., network_code, park_code, site_name, .add = TRUE)
        } %>%

            # limit calculation of rates to stations with at least 3
            # measurements - this is the min # needed to calculate a linear rate
            mutate(date_count = n_distinct(event_date_UTC)) %>%
            filter(date_count >= 3) %>%
            select(-date_count) %>%

            # nest data and run model
            tidyr::nest() %>%
            mutate(lm_model = purrr::map(data, ~lm(mean_cumu ~ date_num, data = .)),
                   lm_model_summary = purrr::map(lm_model, ~summary(.)),
                   rate = purrr::map_dbl(lm_model, ~coefficients(.)[['date_num']]),
                   intc = purrr::map_dbl(lm_model, ~coefficients(.)[["(Intercept)"]]),
                   rate_se = purrr::map_dbl(lm_model_summary, ~.$coefficients[['date_num', 'Std. Error']]),
                   rate_r2 = purrr::map_dbl(lm_model_summary, ~.$r.squared),
                   rate_p = purrr::map_dbl(lm_model_summary, ~.$coefficients[['date_num', 'Pr(>|t|)']]),
                   ci = purrr::map(lm_model, ~as.data.frame(confint(., parm = c("date_num"), level = 0.95))),
                   ci_low = purrr::map_dbl(ci, ~.$`2.5 %`),
                   ci_high =  purrr::map_dbl(ci, ~.$`97.5 %`),
                   ci_abs_value = abs(ci_high - rate),
                   date_count = purrr::map_int(data, ~n_distinct(.x$event_date_UTC))
            ) %>%
            {if (level == "station")
                mutate(.,
                       rate_level = "station")
                else if (level == "site")
                    mutate(.,
                           rate_level = "site")}

        return(linear_rates_set)
        # returns a data frame with linear rates for each station or site in the supplied data frame. Output columns include:
        # -lm_model: the linear model used to estimate the rate
        # -lm_model_summary: the summary of lm_model via summary(lm_model)
        # -rate: the rate of surface elevation change estimated from the model in mm/yr
        # -intc: the model intercept
        # -rate_se: the standard error of the coefficient for the rate of surface elevation change
        # -rate_r2: the model r2
        # -rate_p: the model p-value
        # -ci: a list of the lower and upper 95% confidence intervals for the rate of surface elevation change
        # -ci_low/ci_high: the lower and upper 95% confidence intervals for the rate of surface elevation change
        # -ci_abs_value: the 95% confidence interval absolute value
        # -date_count: a count of the # of SET measurements at each station/site
        # -rate-level: whether the rate of surface elevation change is for station-level or site-level data

    } else if (data_type == "MH"){

        # first calculate cumulative change for each station
        linear_rates_mh <- {if (level == "station")
        { if(override_site_corrections == FALSE)
            NPSETr::calc_change_cumu(data, level = "station")
            else
                NPSETr::calc_change_cumu(data, level = "station", override_site_corrections = TRUE)} %>%
                group_by(., network_code, park_code, site_name, station_code, .add = TRUE)
            else if (level == "site")
            { if(override_site_corrections == FALSE)
                NPSETr::calc_change_cumu(data, level = "site")
                else
                    NPSETr::calc_change_cumu(data, level = "site", override_site_corrections = TRUE)} %>%
                group_by(., network_code, park_code, site_name, .add = TRUE)
        } %>%

            # limit calculation of rates to stations with at least 3
            # measurements - this is the min # needed to calculate a linear rate
            mutate(date_count = n_distinct(event_date_UTC)) %>%
            filter(date_count >= 3) %>%
            select(-c(date_count)) %>%

            # nest data and run model
            tidyr::nest() %>%
            mutate(lm_model = purrr::map(data, ~lm(mean_cumu ~ date_num, data = .)),
                   lm_model_summary = purrr::map(lm_model, ~summary(.)),
                   rate = purrr::map_dbl(lm_model, ~coefficients(.)[['date_num']]),
                   intc = purrr::map_dbl(lm_model, ~coefficients(.)[["(Intercept)"]]),
                   rate_se = purrr::map_dbl(lm_model_summary, ~.$coefficients[['date_num', 'Std. Error']]),
                   rate_r2 = purrr::map_dbl(lm_model_summary, ~.$r.squared),
                   rate_p = purrr::map_dbl(lm_model_summary, ~.$coefficients[['date_num', "Pr(>|t|)"]]),
                   ci = purrr::map(lm_model, ~as.data.frame(confint(., parm = c("date_num"), level = 0.95))),
                   ci_low = purrr::map_dbl(ci, ~.$`2.5 %`),
                   ci_high = purrr::map_dbl(ci, ~.$`97.5 %`),
                   ci_abs_value = abs(ci_high - rate),
                   date_count = purrr::map_int(data, ~n_distinct(.x$event_date_UTC))
            ) %>%
            {if (level == "station")
                mutate(.,
                       rate_level = "station")
                else if (level == "site")
                    mutate(.,
                           rate_level = "site")}

        return(linear_rates_mh)
        # returns a data frame with linear rates for each station or site in the supplied data frame. Output columns include:
        # -lm_model: the linear model used to estimate the rate
        # -lm_model_summary: the summary of lm_model via summary(lm_model)
        # -rate: the rate of accretion estimated from the model in mm/yr
        # -intc: the model intercept
        # -rate_se: the standard error of the coefficient for the rate of accretion
        # -rate_r2: the model r2
        # -rate_p: the model p-value
        # -ci: a list of the lower and upper 95% confidence intervals for the rate of accretion
        # -ci_low/ci_high: the lower and upper 95% confidence intervals for the rate of accretion
        # -ci_abs_value: the 95% confidence interval absolute value
        # -date_count: a count of the # of MH measurements at each station/site
        # -rate-level: whether the rate of accretion is for station-level or site-level data
    }
}
