test_that("returns a data frame with recent sea level rate", suppressWarnings({
    expect_true(is.data.frame(get_recent_slr_rate(park = "ASIS")))
}))
