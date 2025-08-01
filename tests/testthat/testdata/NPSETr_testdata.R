set_df <- load_set_data(network_code = "NCBN")
mh_df <- load_mh_data(network_code = "NCBN")

library(tidyverse)
test_set_data <- set_df %>%
    filter(park_code == "ASIS" & observation_type == "Standard") %>%
    select(network_code, park_code, park_name, site_name, station_code, station_name, event_date_UTC, SET_instrument,
           SET_direction, pin_position, pin_height_mm, SET_offset_mm, pin_length_mm)

saveRDS(test_set_data, here::here("tests", "testthat", "testdata", "test_write_set_mh_data.rds"))
saveRDS(test_set_data, here::here("tests", "testthat", "testdata", "df_2025-06-04.rds"))
write_csv(test_set_data, here::here("tests", "testthat", "testdata", "test_list_calc_change_cumu_set.csv"))
openxlsx::write.xlsx(test_set_data, here::here("tests", "testthat", "testdata", "test_list_calc_change_cumu_set.xlsx"))

test_mh_data <- mh_df %>%
    filter(park_code == "ASIS") %>%
    select(network_code, park_code, park_name, site_name, station_code, station_name, event_date_UTC, marker_horizon_name,
           core_measurement_number, core_measurement_depth_mm, established_date)

write_csv(test_mh_data, here::here("tests", "testthat", "testdata", "test_list_calc_change_cumu_mh.csv"))
openxlsx::write.xlsx(test_mh_data, here::here("tests", "testthat", "testdata", "test_list_calc_change_cumu_mh.xlsx"))

test_message_calc_change_cumu <- test_set_data %>%
    select(-SET_offset_mm)
write_csv(test_message_calc_change_cumu, here::here("tests", "testthat", "testdata", "test_message_calc_change_cumu.csv"))

test_blank_calc_change_cumu <- test_set_data %>%
    mutate(SET_offset_mm = "",
           pin_length_mm = "")
write_csv(test_blank_calc_change_cumu, here::here("tests", "testthat", "testdata", "test_blank_calc_change_cumu.csv"))

test_grouping_calc_change_cumu <- test_set_data %>%
    mutate(SET_group = if_else(station_code == "M6-4", "group A", "group B")) %>%
    group_by(SET_group)
saveRDS(test_grouping_calc_change_cumu, here::here("tests", "testthat", "testdata", "test_grouping_calc_change_cumu.rds"))

test_raw_SET_data <- test_set_data
write_csv(test_raw_SET_data, here::here("tests", "testthat", "testdata", "test_raw_SET_data.csv"))

test_raw_MH_data <- test_mh_data
write_csv(test_raw_MH_data, here::here("tests", "testthat", "testdata", "test_raw_MH_data.csv"))

test_plot_rate_comps_set_station_rates <- calc_linear_rates(test_set_data, level = "station")
saveRDS(test_plot_rate_comps_set_station_rates, here::here("tests", "testthat", "testdata", "test_plot_rate_comps_set_station_rates.rds"))

test_plot_rate_comps_set_site_rates <- calc_linear_rates(test_set_data, level = "site")
saveRDS(test_plot_rate_comps_set_site_rates, here::here("tests", "testthat", "testdata", "test_plot_rate_comps_set_site_rates.rds"))

test_plot_rate_comps_set_site_rates_chr_col <- calc_linear_rates(test_set_data, level = "site") %>%
    mutate(rate = as.character(rate))
saveRDS(test_plot_rate_comps_set_site_rates_chr_col, here::here("tests", "testthat", "testdata", "test_plot_rate_comps_set_site_rates_chr_col.rds"))

test_write_slr_data <- get_sea_level_data(park = "ASIS", start_year = "2001", end_year = "2019")
saveRDS(test_write_slr_data, here::here("tests", "testthat", "testdata", "test_write_slr_data.rds"))
write_slr_data(test_write_slr_data, dest_folder = here::here("tests", "testthat", "testdata"))
