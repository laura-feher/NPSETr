test_that("returns a data frame of future sea level data", suppressWarnings({
    expect_true(is.data.frame(get_future_sea_level_data(park = "ASIS")))
}))
