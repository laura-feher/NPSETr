test_that("calc_gam_rates returns a data frame with gam rates", {
    df <- example_sets
    expect_true(!is.null(calc_gam_rates(df)$gam_model))
})
