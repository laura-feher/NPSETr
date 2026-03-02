test_that("calc_linear_rates returns a data frame with linear rates", {
    df <- example_sets
    expect_true(!is.null(calc_linear_rates(df)$lm_model))
})
