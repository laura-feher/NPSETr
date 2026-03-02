test_that("calc_lmm_rates returns a data frame with lmm rates", {
    df <- example_sets
    expect_true(!is.null(calc_lmm_rates(df)$lmm_model))
})
