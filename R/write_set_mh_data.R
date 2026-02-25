#' Save raw SET/MH data, cumulative change, or linear rates of change to a csv file
#'
#' This function saves a data frame to a csv file. Can be a data frame of
#' raw SET/MH data produced by `load_set_data` or `load_mh_data`, a data frame
#' of cumulative change produced by `calc_change_cumu`, or a data frame of
#' calculated linear rates produced by `calc_linear_rates`.
#'
#' @param data data frame. Can be a data frame of raw SET/MH data produced by
#'   `load_set_data` or `load_mh_data`, a data frame of cumulative change
#'   produced by `calc_change_cumu`, or a data frame of calculated linear rates
#'   produced by `calc_linear_rates`.
#'
#' @param dest_folder string (optional). The folder where you want the file to
#'   be saved. Defaults to the current working directory.
#'
#' @param create_folders boolean (TRUE/FALSE). If the folder specified in
#'   `dest_folder` doesn't exist, do you want to create it? Defaults to FALSE.
#'
#' @param overwrite boolean (TRUE/FALSE). If a file with the same name already
#'   exists in `dest_folder`, do you want to overwrite it? Defaults to FALSE.
#'
#' @returns Saves a data frame to a csv. The file name will be the name of the
#'   data frame supplied to `data` suffixed with the current date e.g.
#'   "colo_set_data_2025-06-04.csv".
#'
#' @export
#'
#' @import dplyr
#' @importFrom readr write_csv
#'
#' @examples
#' \dontrun{
#' # Load SET data from COLO and write to csv
#'
#' colo_set_data <- load_set_data(park = "COLO")
#'
#' write_set_mh_data(
#'     data = colo_set_data,
#'     dest_folder = "C:/Documents/SET_data",
#'     create_folders = TRUE,
#'     overwrite = FALSE
#'     )
#'
#' # Calculate station-level cumulative change and write to csv in the current working directory
#'
#' colo_cumu_change <- calc_change_cumu(colo_set_data, level = "station")
#'
#' write_set_mh_data(
#'     data = colo_cumu_change,
#'     overwrite = TRUE)
#'
#' # Calculate site-level linear rates of change and write to csv in the current working directory
#'
#' colo_site_set_rates <- calc_linear_rates(colo_set_data, level = "site")
#'
#' write_set_mh_data(
#'     data = colo_site_set_rates
#'     )
#' }
#'
write_set_mh_data <- function(data, dest_folder = NULL, create_folders = FALSE, overwrite = FALSE) {

    # Adapted from WritePACNVeg by Jake Gross https://github.com/jakegross808/pacn-veg-package

    file_name <- deparse(substitute(data))
    current_date <- Sys.Date()

    # remove any nested or list columns created with calc_linear rates
    df <- data %>%
        select(-where(is.list))

    if (is.null(dest_folder)) {
        dest_folder <- getwd()
    } else {
        dest_folder <- normalizePath(dest_folder, mustWork = FALSE)
    }

    file_path <- file.path(dest_folder, paste0(file_name, "_", current_date,".csv"))

    if (!dir.exists(dest_folder)) {
        if (create_folders == TRUE) {
            dir.create(dest_folder)
        } else {
            stop("Destination folder does not exist. To create it automatically, set create_folders to TRUE.")
        }
    }

    if (!overwrite & any(file.exists(file_path))) {
        stop("Saving data in the folder provided would overwrite existing data. To automatically overwrite existing data, set overwrite to TRUE.")
    }

    message(paste("Writing", file_path))
    suppressMessages(readr::write_csv(df, file_path, na = "", append = FALSE, col_names = TRUE))

    message("Done writing to CSV")

}
