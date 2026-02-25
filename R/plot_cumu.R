#' Plot station- or site-level cumulative surface elevation change or vertical
#' accretion
#'
#' Creates a faceted plot of cumulative surface elevation change and/or vertical
#' accretion at each station or site. Optionally calculates and displays linear
#' rates of change.
#'
#' @param SET_data data frame (must supply either `SET_data` or `MH_data`). A
#'   data frame of raw SET data. See details below for requirements. If both SET
#'   and MH data are supplied, plots both.
#'
#' @param MH_data data frame (must supply either `SET_data` or `MH_data`). A
#'   data frame of raw MH data. See details below for requirements. If both SET
#'   and MH data are supplied, plots both.
#'
#' @param level string (optional). Level at which to calculate rates of surface
#'   elevation change and/or vertical accretion. One of:
#'   * `"station"` (default) station-level rates of surface elevation change.
#'   * `"site"` site-level rates of surface elevation change.
#'
#' @param rate_type string (optional); Calculate linear rate of change and
#'   include in the plot? Use rate_type = `"linear"` to display station- (level
#'   = `"station"`) or site-level (level = `"site"`) rates of surface elevation
#'   change or vertical accretion in mm/yr.
#'
#' @param columns integer (optional). Number of columns you want in the faceted
#'   output; defaults to 4. Utilizes the `ncol` argument of
#'   [ggplot2::facet_wrap].
#'
#' @param pointsize number (optional). Size of points on the plot; defaults to
#'   3.5. Utilizes the `size` argument of [ggplot2::geom_point].
#'
#' @param plot_scales string (optional). Do you want axis scales to be the same in
#'   all facets ("fixed") or to vary between facets? Defaults to using the same
#'   scale between all panels. Utilizes the `scales` parameter of
#'   [ggplot2::facet_wrap]. One of:
#'   * `"fixed"`: default; use the same axis scales in all panels.
#'   * `"free_x"`: allow x-axis scales to vary between panels.
#'   * `"free_y"`: allow y-axis scales to vary between panels.
#'   * `"free"`: allow both x- and y-axis scales to vary between panels.
#'
#' @inheritSection calc_change_cumu Data Requirements
#'
#' @inheritSection calc_change_cumu Details
#'
#' @section Note:
#'
#'   Cumulative change is calculated via the function [calc_change_cumu] and
#'   linear rates of change are calculated via the function [calc_linear_rates].
#' See function documentation for details.
#'
#' @return A ggplot object: x-axis is date; depending on the type of data
#'   supplied, y-axis is either (a) cumulative surface elevation change and/or
#'   (b) vertical accretion. `level = "station"` (default) returns a plot with
#'   one panel per station, whereas `level = "site"` returns a plot with one
#'   panel per site.
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom ggh4x scale_listed
#' @importFrom tidyr unite
#'
#' @examples
#' # Station-level cumulative change
#' plot_cumu(SET_data = example_sets)
#'
#' # Combined plot of site-level SET and MH cumulative change
#' plot_cumu(SET_data = example_sets, MH_data = example_mh, level = "site")
#'
#' # Change the number of columns, point size, or scaling of x- and/or y-axis
#' plot_cumu(SET_data = example_sets, MH_data = example_mh, columns = 2,
#' pointsize = 1, plot_scales = "free")
#'
#' # Apply a date filter to both SET and MH, and then plot
#' df_list <- list("SET" = example_sets, "MH" = example_mh) %>%
#'     map(., ~.x %>%
#'         filter(event_date_UTC < as.Date("2016-01-01")) %>%
#'         nest()) %>%
#'     list_cbind()
#'
#' plot_cumu(SET_data = unnest(df_list$SET, cols = "data"), MH_data =
#' unnest(df_list$MH, cols = "data"))
#'
#' # Use ggplot2-style "layers" to modify the plot to your liking
#' plot_cumu(SET_data = example_sets) +
#'     theme(
#'         axis.text = element_text(color = "red")
#'         )
#'
plot_cumu <- function(SET_data = NULL, MH_data = NULL, level = "station", rate_type = NULL, columns = 4, pointsize = 2, plot_scales = "fixed") {

    base_plot <- function(df, data_type) {

        if (data_type == "SET") {
            title_lab <- "Cumulative surface elevation change by "
            y_lab <- "Cumulative surface elevation change (mm) "
            line_color <- "lightsteelblue4"
            smooth_color <- "steelblue4"
            point_fill_color <- "lightsteelblue1"
            point_outline_color <- "steelblue3"
        } else if (data_type == "MH") {
            title_lab <- "Cumulative vertical accretion by "
            y_lab <- "Cumulative vertical accretion (mm) "
            line_color <- "indianred4"
            smooth_color <- "tomato4"
            point_fill_color <- "indianred1"
            point_outline_color <- "tomato3"
        }

        ggplot2::ggplot(df, aes(x = event_date_UTC, y = mean_cumu)) +
            ggplot2::geom_line(color = line_color) +
            # ggplot2::geom_smooth(formula = y~x, se = FALSE, method = "lm", color = smooth_color, linewidth = 1) +
            ggplot2::geom_errorbar(aes(x = event_date_UTC, ymin = mean_cumu - se_cumu, ymax = mean_cumu + se_cumu)) +
            ggplot2::geom_point(shape = 21, fill = point_fill_color, color = point_outline_color, size = pointsize, alpha = 0.9) +
            ggplot2::facet_wrap(~grouping, ncol = columns, scales = plot_scales) +
            ggplot2::labs(title = paste0(title_lab, level), x = "Date", y = y_lab) +
            ggplot2::theme_classic()
    }

    if (!is.null(SET_data) & is.null(MH_data)) {

        # make sure SET_data is SET data
        data_type <- detect_data_type(SET_data)

        if (data_type != "SET") {
            stop(paste0("SET_data must be valid SET data. See 'data requirements' in the documentation for `calc_change_cumu()`."))
        } else {
            if (level == "station") {

                groups <- SET_data %>%
                    calc_change_cumu(., level = "station") %>%
                    attr(., "groups") %>%
                    select(-c(network_code, park_code, site_name, ".rows")) %>%
                    colnames()

                # plot
                p <- SET_data %>%
                    calc_change_cumu(., level = "station") %>%
                    tidyr::unite("grouping", all_of(groups), remove = FALSE) %>%
                    base_plot(., data_type = data_type)

                if (is.null(rate_type)) {
                    p
                } else if (rate_type == "linear") {

                    set_rates <- plot_rate_labels(data = SET_data, level = level, groups = groups, data_type = data_type)

                    p +
                        ggplot2::geom_text(data = set_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = rate_label), hjust = -0.1, vjust = 1.5) +
                        ggplot2::geom_text(data = set_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = r2p_label), parse = T, hjust = -0.2, vjust = 2)
                }

            } else if (level == "site") {

                groups <- SET_data %>%
                    calc_change_cumu(., level = "site") %>%
                    attr(., "groups") %>%
                    select(-c(network_code, park_code, ".rows")) %>%
                    colnames()

                #plot
                p <- SET_data %>%
                    calc_change_cumu(., level = "site") %>%
                    tidyr::unite("grouping", all_of(groups), remove = FALSE) %>%
                    base_plot(., data_type = data_type)

                if (is.null(rate_type)) {
                    p
                } else if (rate_type == "linear") {

                    set_rates <- plot_rate_labels(data = SET_data, level = level, groups = groups, data_type = data_type)

                    p +
                        ggplot2::geom_text(data = set_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = rate_label), hjust = -0.1, vjust = 1.5) +
                        ggplot2::geom_text(data = set_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = r2p_label), parse = T, hjust = -0.2, vjust = 2)
                }
            }
        }
    } else if (is.null(SET_data) & !is.null(MH_data)) {

        # make sure MH_data is MH data
        data_type <- detect_data_type(MH_data)

        if (data_type != "MH") {
            stop(paste0("MH_data must be valid MH data. See 'data requirements' in the documentation for `calc_change_cumu()`."))
        } else {
            if (level == "station") {

                groups <- MH_data %>%
                    calc_change_cumu(., level = "station") %>%
                    attr(., "groups") %>%
                    select(-c(network_code, park_code, site_name, ".rows")) %>%
                    colnames()

                # plot
                p <- MH_data %>%
                    calc_change_cumu(., level = "station") %>%
                    tidyr::unite("grouping", all_of(groups), remove = FALSE) %>%
                    base_plot(., data_type = data_type)

                if (is.null(rate_type)) {
                    p
                } else if (rate_type == "linear") {

                    mh_rates <- plot_rate_labels(data = MH_data, level = level, groups = groups, data_type = data_type)

                    p +
                        ggplot2::geom_text(data = mh_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = rate_label), hjust = -0.1, vjust = 1.5) +
                        ggplot2::geom_text(data = mh_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = r2p_label), parse = T, hjust = -0.2, vjust = 2)
                }

            } else if (level == "site") {

                groups <- MH_data %>%
                    calc_change_cumu(., level = "site") %>%
                    attr(., "groups") %>%
                    select(-c(network_code, park_code, ".rows")) %>%
                    colnames()

                # plot
                p <- MH_data %>%
                    calc_change_cumu(., level = "site") %>%
                    tidyr::unite("grouping", all_of(groups), remove = FALSE) %>%
                    base_plot(., data_type = data_type)

                if (is.null(rate_type)) {
                    p
                } else if (rate_type == "linear") {

                    mh_rates <- plot_rate_labels(data = MH_data, level = level, groups = groups, data_type = data_type)

                    p +
                        ggplot2::geom_text(data = mh_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = rate_label), hjust = -0.1, vjust = 1.5) +
                        ggplot2::geom_text(data = mh_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = r2p_label), parse = T, hjust = -0.2, vjust = 2)
                }
            }
        }
    } else if (!is.null(SET_data) & !is.null(MH_data)) {

        # make sure SET_data is SET data
        data_type_SET <- detect_data_type(SET_data)

        # make sure MH_data is MH data
        data_type_MH <- detect_data_type(MH_data)

        if (data_type_SET != "SET") {
            stop(paste0("SET_data must be a valid SET data frame. See 'data requirements' in the documentation for `calc_change_cumu()`."))
        } else if (data_type_MH != "MH") {
            stop(paste0("MH_data must be a valid MH data frame. See 'data requirements' in the documentation for `calc_change_cumu()`."))
        } else if (data_type_SET != "SET" & data_type_MH != "MH") {
            stop(paste0("SET_data must be a valid SET data frame and MH_data must be a valid MH data frame. See 'data requirements' in the documentation for `calc_change_cumu()`."))
        } else {

            SET_MH_base_plot <- function(df) {

                suppressWarnings(
                    ggplot2::ggplot(df, aes(x = event_date_UTC, y = mean_cumu, group = data_type)) +
                        ggplot2::geom_line(aes(color1 = data_type)) +
                        # gggplot2::geom_smooth(aes(color2 = data_type), formula = y~x, se = FALSE, method = 'lm', linewidth = 1) +
                        ggplot2::geom_errorbar(aes(x = event_date_UTC, ymin = mean_cumu - se_cumu, ymax = mean_cumu + se_cumu)) +
                        ggplot2::geom_point(aes(fill = data_type, color3 = data_type), shape = 21, size = pointsize, alpha = 0.9) +
                        ggplot2::facet_wrap(~grouping, ncol = columns, scales = plot_scales) +
                        ggh4x::scale_listed(scalelist = list(
                            ggplot2::scale_colour_manual(values = c('lightsteelblue4', 'indianred4'), aesthetics = "color1", breaks = c("SET", "MH")),
                            # ggplot2::scale_colour_manual(values = c('steelblue4', 'tomato4'), aesthetics = "color2", breaks = c("SET", "MH")),
                            ggplot2::scale_colour_manual(values = c('steelblue3', 'tomato3'), aesthetics = "color3", breaks = c("SET", "MH")),
                            ggplot2::scale_fill_manual(values = c('lightsteelblue1', 'indianred1'), breaks = c("SET", "MH"))
                        # ), replaces = c("color", "color", "color", "fill")) +
                        ), replaces = c("color", "color", "fill")) +
                        ggplot2::labs(title = paste0('Cumulative surface elevation change and\nvertical accretion by ', level), x = 'Date', y = 'Cumulative surface elevation change and\nvertical accretion (mm)') +
                        ggplot2::theme_classic() +
                        ggplot2::theme(
                            legend.title = element_blank()
                        )
                )
            }

            if (level == "station") {

                # calculative cumulative change for SET and MH data
                SET_groups <- SET_data %>%
                    calc_change_cumu(., level = "station") %>%
                    attr(., "groups") %>%
                    select(-c(network_code, park_code, site_name, ".rows")) %>%
                    colnames()

                MH_groups <- MH_data %>%
                    calc_change_cumu(., level = "station") %>%
                    attr(., "groups") %>%
                    select(-c(network_code, park_code, site_name, ".rows")) %>%
                    colnames()

                if (identical(SET_groups, MH_groups) == FALSE) {
                    stop(print("SET and MH data must have the same grouping in order to plot them together."))
                } else {

                    plot_df_SET <- SET_data %>%
                        calc_change_cumu(., level = "station") %>%
                        tidyr::unite("grouping", all_of(SET_groups), remove = FALSE) %>%
                        mutate(data_type = "SET")

                    plot_df_MH <- MH_data %>%
                        calc_change_cumu(., level = "station") %>%
                        tidyr::unite("grouping", all_of(SET_groups), remove = FALSE) %>%
                        mutate(data_type = "MH")

                    plot_df <- bind_rows(plot_df_SET, plot_df_MH)

                    # plot data
                    p <- SET_MH_base_plot(df = plot_df)

                    if (is.null(rate_type)) {
                        p
                    } else if (rate_type == "linear") {

                        set_rates <- plot_rate_labels(data = SET_data, level = level, groups = SET_groups, data_type = "SET") %>%
                            mutate(data_type = "SET")

                        mh_rates <- plot_rate_labels(data = MH_data, level = level, groups = MH_groups, data_type = "MH") %>%
                            mutate(data_type = "MH")

                        p +
                            ggplot2::geom_text(data = set_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = rate_label), hjust = -0.1, vjust = 1.5) +
                            ggplot2::geom_text(data = set_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = r2p_label), parse = T, hjust = -0.2, vjust = 2.1) +
                            ggplot2::geom_text(data = mh_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = rate_label), hjust = -0.1, vjust = 6) +
                            ggplot2::geom_text(data = mh_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = r2p_label), parse = T, hjust = -0.15, vjust = 4.9)
                    }
                }

            } else if (level == "site") {

                # calculative cumulative change for SET and MH data
                SET_groups <- SET_data %>%
                    calc_change_cumu(., level = "site") %>%
                    attr(., "groups") %>%
                    select(-c(network_code, park_code, ".rows")) %>%
                    colnames()

                MH_groups <- MH_data %>%
                    calc_change_cumu(., level = "site") %>%
                    attr(., "groups") %>%
                    select(-c(network_code, park_code, ".rows")) %>%
                    colnames()

                if (identical(SET_groups, MH_groups) == FALSE) {
                    stop(print("SET and MH data must have the same grouping in order to plot them together."))
                } else {

                    plot_df_SET <- SET_data %>%
                        calc_change_cumu(., level = "site") %>%
                        tidyr::unite("grouping", all_of(SET_groups), remove = FALSE) %>%
                        mutate(data_type = "SET")

                    plot_df_MH <- MH_data %>%
                        calc_change_cumu(., level = "site") %>%
                        tidyr::unite("grouping", all_of(SET_groups), remove = FALSE) %>%
                        mutate(data_type = "MH")

                    plot_df <- bind_rows(plot_df_SET, plot_df_MH)

                    # plot data
                    p <- SET_MH_base_plot(df = plot_df)

                    if (is.null(rate_type)) {
                        p
                    } else if (rate_type == "linear") {

                        set_rates <- plot_rate_labels(data = SET_data, level = level, groups = SET_groups, data_type = "SET") %>%
                            mutate(data_type = "SET")

                        mh_rates <- plot_rate_labels(data = MH_data, level = level, groups = MH_groups, data_type = "MH") %>%
                            mutate(data_type = "MH")

                        p +
                            ggplot2::geom_text(data = set_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = rate_label), hjust = -0.1, vjust = 1.5) +
                            ggplot2::geom_text(data = set_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = r2p_label), parse = T, hjust = -0.2, vjust = 2.1) +
                            ggplot2::geom_text(data = mh_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = rate_label), hjust = -0.1, vjust = 6) +
                            ggplot2::geom_text(data = mh_rates, aes(x = structure(-Inf, class = "Date"), y = Inf, label = r2p_label), parse = T, hjust = -0.15, vjust = 4.9)
                    }
                }
            }
        }
    }
}
