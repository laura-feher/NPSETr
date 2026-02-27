#' Save SLR data and rates to a csv file
#'
#' This function saves a data frame produced by \code{\link{get_sea_level_data}}. Creates
#' one csv file containing the sea-level data and another csv file containing
#' the calculated rate of sea-level rise.
#'
#' @inheritParams write_set_mh_data
#' @param data list. Specifically a list of 2 data frames produced by
#'   \code{\link{get_sea_level_data}}.
#'
#' @returns Saves the SLR data and calculated SLR rate to two separate csv
#'   files. The file names will be the name of the data frame supplied to `data`
#'   suffixed with "_slr_data" or "_slr_rate" and the current date e.g.
#'   "asis_slr_data_2025-06-05.csv" and "asis_slr_rate_2025-06-05.csv".
#'
#' @seealso \code{\link{get_sea_level_data}}
#'
#' @export
#'
#' @import dplyr
#' @importFrom readr write_csv
#'
#' @examples
#' \dontrun{
#' # Load SLR data for ASIS
#'
#' asis <- get_sea_level_data(park = "ASIS")
#'
#' write_slr_data(
#'     data = asis,
#'     dest_folder = "C:/Documents/SLR_data",
#'     create_folders = TRUE,
#'     overwrite = FALSE
#'     )
#' }
#'
write_slr_data <- function(data, dest_folder = NULL, create_folders = FALSE, overwrite = FALSE) {

    # Adapted from WritePACNVeg by Jake Gross https://github.com/jakegross808/pacn-veg-package

    file_name <- deparse(substitute(data))
    current_date <- Sys.Date()
    object_type <- unique(data$object)[1]

    # remove any nested or list columns created with calc_linear rates
    dat <- data %>%
        select(-where(is.list))

    if (is.null(dest_folder)) {
        dest_folder <- getwd()
    } else {
        dest_folder <- normalizePath(dest_folder, mustWork = FALSE)
    }

    if (object_type == "sea level data") {
        file_type_string <- "_sea_level_data_"
    } else if (object_type == "future sea level data") {
        file_type_string <- "_future_sea_level_data_"
    } else if (object_type == "long term slr rate") {
        file_type_string <- "_longterm_slr_rate_"
    } else if (object_type == "recent slr rate") {
        file_type_string <- "_recent_slr_rate_"
    } else if (object_type == "future slr rate") {
        file_type_string <- "_future_slr_rate_"
    }

    file_path <- file.path(dest_folder, paste0(file_name, file_type_string, current_date, ".csv"))

    if (!dir.exists(dest_folder)) {
        if (create_folders == TRUE) {
            dir.create(dest_folder)
        } else {
            stop("Destination folder does not exist. To create it automatically, set create_folders to TRUE.")
        }
    }

    if (!overwrite & any(file.exists(c(file_path)))) {
        stop("Saving data in the folder provided would overwrite existing data. To automatically overwrite existing data, set overwrite to TRUE.")
    }

    message(paste("Writing", file_path))
    suppressMessages(readr::write_csv(dat, file_path, na = "", append = FALSE, col_names = TRUE))

    message("Done writing to CSV")

}
