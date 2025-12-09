#' Calculate station- or site-level cumulative change for SET or MH data
#'
#' This function takes a data frame of raw SET or MH data and calculates
#' cumulative surface elevation change (for SET data) or vertical accretion (for
#' MH data) on each measurement date at either the station- or site-level.
#'
#' @param data data frame. A data frame of raw SET or MH data. See details below
#'   for requirements.
#'
#' @param level string (optional). Level at which to calculate rates of surface
#'   elevation change or vertical accretion. One of:
#'   * `"station"`: (default) station-level rates of surface elevation change.
#'   * `"site"`: site-level rates of surface elevation change.
#'
#' @param override_site_corrections logical (optional). If TRUE, don't use the
#'   site groupings specified in utils.R since these are specific to the USGS
#'   project.
#'
#' @section Data Requirements:
#'
#'   This function takes a data frame of either raw SET or MH data (`data`).
#'
#'   SET data must have 1 row per pin reading and the following columns, named
#'   exactly: event_date_UTC, network_code, park_code, site_name, station_code,
#'   SET_direction, pin_position, SET_offset_mm, pin_length_mm, and
#'   pin_height_mm. Note that SET_offset_mm and pin_length_mm can be empty (aka
#'   blank) but the columns must be included in the data frame. See
#'   `example_sets`.
#'
#'   MH data must have 1 row per core measurement and the following columns,
#'   named exactly: event_date_UTC, network_code, park_code, site_name,
#'   marker_horizon_name, core_measurement_number, core_measurement_depth_mm,
#'   and established_date. See `example_mh`.
#'
#' @section Details:
#'
#'   For SET data, pin-level cumulative change is first calculated as the
#'   difference between each pin reading and the reading from the earliest date
#'   that was not NA. Note that for data from NCBN, NCRN, and NETN, pin heights
#'   are first converted to standardized pin heights using the formula 1000 +
#'   (SET_offset_mm - (pin_length_mm - pin_height_mm)) to account for the 6"
#'   extensions that were added to some benchmarks within these networks' parks
#'   in 2023/24. Then, cumulative pin changes are averaged to the arm-level on
#'   each date, and then the cumulative arm-level changes are averaged up to the
#'   station-level on each date. If level = "site" is used, station-level
#'   cumulative change will be averaged up to the site-level.
#'
#'   For MH data, duplicate core readings ('core_measurement_number') are first
#'   averaged to the marker horizon-level. Then, average marker horizon depths
#'   are averaged up to the station-level on each date, excluding NAs. If level
#'   = "site" is used, station-level cumulative change will be averaged up to
#'   the site-level.
#'
#' @return For SET data, returns a data frame of station- or site-level
#'   cumulative surface elevation change. For MH data, returns a data frame of
#'   station- or site-level vertical accretion.
#'
#' @export
#'
#' @importFrom tidyr fill
#'
#' @examples
#' # Defaults to station-level cumulative change
#' calc_change_cumu(example_sets)
#'
#' # Site-level cumulative change
#' calc_change_cumu(example_mh, level = "site")
#'
#' # Define custom groups for calculating cumulative change
#' example_sets %>%
#'     group_by(set_type) %>%
#'     calc_change_cumu(., level = "site")
#'
calc_change_cumu <- function(data, level = "station", override_site_corrections = FALSE) {

    # determine if the data is SET or MH
    data_type <- detect_data_type(data)

    ## do calculations based on data type

    if (data_type == "SET") {

        change_cumu_set <- data %>%
            # apply proper station groupings for NCBN data
            { if(override_site_corrections == FALSE)
                mutate(.,site_name = correct_site_groups(station_code = station_code, site_name = site_name))

                else .} %>%

            # convert to standardized pin heights to account for 6" extensions used at some of these networks' sites
            group_by(., network_code, park_code, site_name, station_code, SET_direction, pin_position, .add = TRUE) %>%
            filter(!is.na(pin_height_mm)) %>%
            mutate(pin_height_mm = if_else(network_code %in% c("NCBN", "NCRN", "NETN") & !is.na(SET_offset_mm) & !is.na(pin_length_mm),
                                           1000 + (SET_offset_mm-(pin_length_mm - pin_height_mm)),
                                           pin_height_mm)) %>%

            # first get cumulative change for each pin
            mutate(event_date_UTC = as.Date(event_date_UTC),
                   first_pin_height = pin_height_mm[event_date_UTC == min(event_date_UTC[!is.na(pin_height_mm)])],
                   cumu = pin_height_mm - first_pin_height) %>%

            # average cumulative pin change up to the arm-level
            group_by(network_code, park_code, site_name, station_code, SET_direction, event_date_UTC, .add = TRUE) %>%
            drop_groups2(., pin_position) %>%
            summarize(mean_cumu = mean(cumu, na.rm = TRUE)) %>%

            #average cumulative arm-level change up to the station-level
            group_by(network_code, park_code, site_name, station_code, event_date_UTC, .add = TRUE) %>%
            drop_groups2(., SET_direction) %>%
            select(mean_value = mean_cumu) %>%
            summarize(mean_cumu = mean(mean_value, na.rm = TRUE),
                      sd_cumu = sd(mean_value, na.rm = TRUE),
                      se_cumu = sd(mean_value, na.rm = TRUE)/sqrt(length(!is.na(mean_value)))) %>%
            mutate(data_type = "SET") %>%

            {if (level == "station")
                .
                else if (level == "site")
                    # average cumulative station-level change up to the site-level
                    group_by(., network_code, park_code, site_name, .add = TRUE) %>%
                    drop_groups2(., event_date_UTC) %>%
                    drop_groups2(., station_code) %>%
                    arrange(network_code, park_code, site_name, event_date_UTC) %>%
                    mutate(.,
                           date_group = cumsum(c(1, diff.Date(event_date_UTC)) >= 31) # group dates that are less than 31 days apart
                    ) %>%
                    group_by(., network_code, park_code, site_name, date_group, .add = TRUE) %>%
                    mutate(event_date_UTC = min(event_date_UTC)) %>%
                    group_by(., network_code, park_code, site_name, event_date_UTC, .add = TRUE) %>%
                    drop_groups2(., date_group) %>%
                    select(mean_value = mean_cumu) %>%
                    summarize(mean_cumu = mean(mean_value, na.rm = TRUE),
                              sd_cumu = sd(mean_value, na.rm = TRUE),
                              se_cumu = sd(mean_value, na.rm = TRUE)/sqrt(length(!is.na(mean_value)))) %>%
                    mutate(data_type = "SET")
            }

        return(change_cumu_set)

    } else if (data_type == "MH"){

        change_cumu_mh <- data %>%
            # apply proper station groupings for NCBN data
            { if(override_site_corrections == FALSE)
                mutate(.,site_name = correct_site_groups(station_code = station_code, site_name = site_name))

                else .} %>%

            # first average all core measurements from each date
            mutate(., event_date_UTC = as.Date(event_date_UTC),
                   established_date = as.Date(established_date)) %>%
            group_by(network_code, park_code, site_name, station_code, marker_horizon_name, event_date_UTC, established_date, .add = TRUE) %>%
            summarise(cumu = mean(core_measurement_depth_mm, na.rm = TRUE)) %>%

            # average the core measurements from each plot up to the station-level
            group_by(network_code, park_code, site_name, station_code, event_date_UTC, established_date, .add = TRUE) %>%
            drop_groups2(., marker_horizon_name) %>%
            summarise(mean_cumu = mean(cumu, na.rm = TRUE),
                      sd_cumu = sd(cumu, na.rm = TRUE),
                      se_cumu = sd(cumu, na.rm = TRUE)/sqrt(length(!is.na(cumu)))) %>%
            mutate(mean_cumu = if_else(is.nan(mean_cumu), NA_real_, mean_cumu)) %>%

            # find stations/sites where established_date is not the first row
            drop_groups2(., event_date_UTC) %>%
            drop_groups2(., established_date) %>%
            mutate(first_date = event_date_UTC[event_date_UTC == min(event_date_UTC[!is.na(mean_cumu)])],
                   first_date_match = if_else(first_date == established_date, "y", "n")) %>%
            group_modify(~{
                if(all(.x$first_date_match == "y")) # if established_date = first row, do nothing
                    .x
                else if(any(.x$first_date_match == "n")) # if established date is not first row, add a blank row
                    tibble::add_row(.x, first_date_match = "n", .before = 0)
            }) %>%
            mutate(across(c(mean_cumu, sd_cumu, se_cumu), # fill in mean, sd, and se, columns with 0 for the blank rows
                          ~if_else(if_all(.cols = c(
                              event_date_UTC,
                              established_date,
                              mean_cumu,
                              sd_cumu,
                              se_cumu,
                              first_date),
                              .f = is.na),
                              0,
                              mean_cumu))) %>%
            tidyr::fill(established_date, .direction = "up") %>% # fill established date in new first rows using the station's established date value in other rows
            mutate(first_date = if_else(first_date_match == "n", established_date, first_date),
                   event_date_UTC = if_else(is.na(event_date_UTC) & first_date_match == "n" & mean_cumu == 0, established_date, event_date_UTC), # fill event_date_UTC in new first rows using the station's established date
                   group_min_first_date = min(established_date, na.rm = TRUE)) %>% # add last value from previous group of MH plots to the replacement plots
            arrange(network_code, park_code, site_name, station_code, event_date_UTC) %>%
            mutate(previous_plot_group_cumu = replace(mean_cumu, established_date != group_min_first_date, NA)) %>%
            fill(previous_plot_group_cumu) %>%
            mutate(mean_cumu = if_else(established_date != group_min_first_date, mean_cumu + previous_plot_group_cumu, mean_cumu),
                   data_type = "MH") %>%

            {if (level == "station")
                .
                else if (level == "site")
                    # average the station-level measurements up to the site-level
                    group_by(., network_code, park_code, site_name, .add = TRUE) %>%
                    drop_groups2(., event_date_UTC) %>%
                    drop_groups2(., station_code) %>%
                    arrange(network_code, park_code, site_name, event_date_UTC) %>%

                    # group dates that are less than 31 days apart
                    mutate(date_group = cumsum(c(1, diff.Date(event_date_UTC)) >= 31)
                    ) %>%
                    group_by(., network_code, park_code, site_name, date_group, .add = TRUE) %>%
                    mutate(event_date_UTC = min(event_date_UTC)) %>%

                    # add last value from previous group of MH plots to the replacement plots
                    group_by(., network_code, park_code, site_name, .add = TRUE) %>%
                    drop_groups2(., date_group) %>%
                    mutate(group_min_first_date = min(established_date, na.rm = TRUE)) %>%
                    arrange(network_code, park_code, site_name, event_date_UTC) %>%
                    mutate(previous_plot_group_cumu = replace(mean_cumu, established_date != group_min_first_date, NA)) %>%
                    fill(previous_plot_group_cumu) %>%
                    mutate(mean_cumu = if_else(established_date != group_min_first_date, mean_cumu + previous_plot_group_cumu, mean_cumu))  %>%

                    # finally - average up to site-level
                    group_by(., network_code, park_code, site_name, event_date_UTC, .add = TRUE) %>%
                    drop_groups2(., date_group) %>%
                    select(mean_value = mean_cumu, first_date, group_min_first_date) %>%
                    summarize(mean_cumu = mean(mean_value, na.rm = TRUE),
                              sd_cumu = sd(mean_value, na.rm = TRUE),
                              se_cumu = sd(mean_value, na.rm = TRUE)/sqrt(length(!is.na(mean_value))),
                              first_date = min(group_min_first_date)) %>%
                    mutate(mean_cumu = if_else(is.nan(mean_cumu), NA_real_, mean_cumu),
                           data_type = "MH")

            }

        return(change_cumu_mh)
    }
}
