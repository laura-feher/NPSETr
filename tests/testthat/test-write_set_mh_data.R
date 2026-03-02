test_that("error about creating folders shows", {
    df <- readRDS(test_path("testdata", "test_write_set_mh_data.rds"))
    expect_error(write_set_mh_data(df, dest_folder = "C:/tests"), "Destination folder does not exist. To create it automatically, set create_folders to TRUE.")
})

test_that("error about overwriting data shows", {
    df <- readRDS(test_path("testdata", "df_2025-06-04.rds"))
    expect_error(write_set_mh_data(df, dest_folder = "C:/Users/lfeher/OneDrive - DOI/NPSETr/tests/testthat/testdata"), "Saving data in the folder provided would overwrite existing data. To automatically overwrite existing data, set overwrite to TRUE.")
})

test_that("writes a data frame of raw SET data to csv in the specified destination folder", {
    set_df_write_test <- readr::read_csv(test_path("testdata", "test_raw_SET_data.csv"))
    test_dest_folder <- test_path("testdata")
    write_set_mh_data(set_df_write_test, dest_folder = test_dest_folder, create_folders = FALSE, overwrite = TRUE)
    current_date <- Sys.Date()
    test_file_name <- paste0("set_df_write_test", "_", current_date, ".csv")
    expect_true(test_file_name %in% list.files(test_dest_folder))
})

test_that("writes a data frame of linear rates of change to csv in the specified destination folder", {
    rates_df_write_test <- readr::read_csv(test_path("testdata", "test_raw_SET_data.csv")) %>%
        calc_linear_rates(., level = "site")
    test_dest_folder <- test_path("testdata")
    write_set_mh_data(rates_df_write_test, dest_folder = test_dest_folder, create_folders = FALSE, overwrite = TRUE)
    current_date <- Sys.Date()
    test_file_name <- paste0("rates_df_write_test", "_", current_date, ".csv")
    expect_true(test_file_name %in% list.files(test_dest_folder))

})
