test_that("returns a data frame with future sea level rate", suppressWarnings({
    expect_true(is.data.frame(get_future_slr_rate(park = "ASIS")))
}))
