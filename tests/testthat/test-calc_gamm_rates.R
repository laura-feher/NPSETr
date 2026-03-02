test_that("calc_gamm_rates returns a data frame with gamm rates", {
    df <- example_sets %>%
        filter(station_code == "M11-1")
    expect_true(!is.null(calc_gamm_rates(df)$gamm_model))
})
