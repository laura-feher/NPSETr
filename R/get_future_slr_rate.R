get_future_slr_rate <- function(park, nauset = FALSE, decade = 2100, scenario_percentile = NULL) {

    if (decade < 2030 | decade > 2150) {
        stop("must pick a decade that ends after 2030 and before 2150")
    }

    future_slr_data <- get_future_sea_level_data(park = park, nauset = nauset, scenario_percentile = scenario_percentile)

    data <- future_slr_data %>%
        {if (!is.null(scenario_percentile))
            filter(., str_detect(Scenario, scenario_percentile))
        else
            .} %>%
        mutate(.,
           future_slr_rate = (!!sym(paste0("rsl_", decade, "_mm")) - !!sym(paste0("rsl_", decade - 10, "_mm")))/10,
           object_type = "future slr rate") %>%
        select(park_code:scenario_perc, !!sym(paste0("rsl_", decade, "_mm")), !!sym(paste0("rsl_", decade - 10, "_mm")), future_slr_rate, object_type)

    return(data)
}
