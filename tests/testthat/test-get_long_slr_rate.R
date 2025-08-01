test_that("returns a data frame with long-term sea level rate", suppressWarnings({
    expect_true(is.data.frame(get_long_slr_rate(park = "ASIS")))
}))
