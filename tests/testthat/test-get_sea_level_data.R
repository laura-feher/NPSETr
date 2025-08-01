test_that("returns a data frame of sea level data", suppressWarnings({
    expect_true(is.data.frame(get_sea_level_data(park = "ASIS")))
}))
