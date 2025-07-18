test_that("error about creating folders shows", {
    df <- readRDS(test_path("testdata", "test_write_slr_data.rds"))
    expect_error(write_slr_data(df, dest_folder = "C:/tests"), "Destination folder does not exist. To create it automatically, set create_folders to TRUE.")
})

test_that("error about overwriting data shows", {
    df <- readRDS(test_path("testdata", "test_write_slr_data.rds"))
    expect_error(write_slr_data(df, dest_folder = "C:/Users/lfeher/OneDrive - DOI/NCBN SETr/NPSETr/tests/testthat/testdata"), "Saving data in the folder provided would overwrite existing data. To automatically overwrite existing data, set overwrite to TRUE.")
})

test_that("writes a data frame of sea-level rise data and rate to csv in the specified destination folder", {
    df <- readRDS(test_path("testdata", "test_write_slr_data.rds"))
    test_dest_folder <- test_path("testdata")
    write_slr_data(df, dest_folder = test_dest_folder, create_folders = FALSE, overwrite = TRUE)
    current_date <- Sys.Date()
    test_file_name_slr_data <- paste0("df_slr_data_", current_date, ".csv")
    test_file_name_slr_rate <- paste0("df_slr_rate_", current_date, ".csv")
    expect_true(test_file_name_slr_data %in% list.files(test_dest_folder))
    expect_true(test_file_name_slr_rate %in% list.files(test_dest_folder))
})
