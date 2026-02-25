#' Download/Load raw marker horizon data from the NPS SET data package, NPS SET database, or a saved file
#'
#' This function can be used to download and/or load raw marker
#' horizon data. Defaults to downloading data from the most recent SET data
#' package if neither `file_path =` or `db_server =` are specified.
#'
#' @inheritParams load_set_data
#'
#' @return A data frame of raw MH data.
#'
#' @details Note that you must be on VPN and have access to the back-end of NPS
#'   I&M SET database for the database option to work. Contact Laura Feher
#'   (lfeher at NPS.gov) for the required connection strings.
#'
#' @inheritSection calc_change_cumu Data Requirements
#'
#' @inheritSection calc_change_cumu Details
#'
#' @export
#'
#' @import dplyr
#' @import DBI
#' @import odbc
#' @import readr
#' @import readxl
#' @import NPSutils
#' @import stringr
#'
#' @examples
#' \dontrun{
#' # Download and load raw MH data for ASIS from the most recent data package
#' df <- load_mh_data(park = "ASIS")
#'
#' # Download and load raw MH data for NCBN parks from the online database
#' df <- load_mh_data(network_code = "NCBN", db_server = "example.server")
#'
#' # Load raw MH data from a saved csv file
#' df <- load_mh_data(file_path = "C:/Documents/Data/my_MH_data.csv")
#'
#' # Use `here::here` to load raw MH data from a relative file path
#' df <- load_mh_data(file_path = here::here("Data", "my_MH_data.csv"))
#' }
#'
load_mh_data <- function(park = NULL, network_code = NULL, file_path = NULL, db_server = NULL, db_port = NULL){

    # Get MH data from the most recent data package
    if (is.null(file_path) & is.null(db_server)) {

        # get_new_version_id checks if 2309427 (2025 DP) is the newest reference version of the SET DP. If its not the newest, it will set the ref_code to the newest version.
        if (is.null(NPSutils::get_new_version_id(2309427))) {
            ref_code <- 2309427
        } else {
            ref_code <- NPSutils::get_new_version_id(2309427)
        }

        # download the SET data package
        NPSutils::get_data_package(ref_code)

        data_list <- NPSutils::load_data_package(ref_code)

        data <- data_list %>%
            purrr::keep(., stringr::str_detect(names(.), "marker_horizon_data")) %>%
            purrr::pluck(., 1) %>%
            { if (!is.null(park) & !is.null(network_code))
                filter(., park_code == toupper(park) & network_code == toupper(network_code) & dpl == "Accepted")

                else if (!is.null(park) & is.null(network_code))
                    filter(., park_code == toupper(park) & dpl == "Accepted")

                else if (is.null(park) & !is.null(network_code))
                    filter(., network_code == toupper(network_code) & dpl == "Accepted")

                else if (is.null(park) & is.null(network_code))
                    filter(., dpl == "Accepted")
            }
    }

    else if (is.null(file_path) & !is.null(db_server)) {

        # Set up connection variables
        database_name <- "SET"
        database_driver <- "ODBC Driver 17 for SQL Server"

        # Build a new connection and connect to the database server
        con <- DBI::dbConnect(odbc::odbc(),
                              Driver = database_driver,
                              Server = db_server,
                              Database = "SET",
                              Trusted_Connection = "Yes",
                              Port = db_port)

        # get data from SET db and filter to a specific park
        if (!is.null(park) & is.null(network_code)) {
            dataf <- DBI::dbSendQuery(con, 'SELECT * FROM ssrs.vw_dbx_marker_horizon_data WHERE park_code = ?')
            data <- DBI::dbBind(dataf, list(toupper(park)))
            data <- DBI::dbFetch(data)
            DBI::dbClearResult(dataf)
        }

        # get data from SET db and filter to a specific I&M network
        else if (!is.null(network_code) & is.null(park)) {
            dataf <- DBI::dbSendQuery(con, 'SELECT * FROM ssrs.vw_dbx_marker_horizon_data WHERE network_code = ?')
            data <- DBI::dbBind(dataf, list(toupper(network_code)))
            data <- DBI::dbFetch(data)
            DBI::dbClearResult(dataf)
        }

        # if both park and network are specified, ignore network_code and filter to park_code
        else if (!is.null(park) & !is.null(network_code)) {
            dataf <- DBI::dbSendQuery(con, 'SELECT * FROM ssrs.vw_dbx_marker_horizon_data WHERE park_code = ?')
            data <- DBI::dbBind(dataf, list(toupper(park)))
            data <- DBI::dbFetch(data)
            DBI::dbClearResult(dataf)
        }

        # or return the whole database
        else if (is.null(park) & is.null(network_code)) {
            data <- DBI::dbGetQuery(con, 'SELECT * FROM ssrs.vw_dbx_marker_horizon_data')
        }

        # close database connection to free up resources
        DBI::dbDisconnect(con)
    }

    else if (!is.null(file_path) & stringr::str_detect(file_path, ".csv") & is.null(db_server)) {
        data <- readr::read_csv(file_path)

        cols <- colnames(data)

        ## conditions: have correct columns in data frame
        ## stop and give an informative message if this isn't met
        req_clms <- c("event_date_UTC", "network_code", "park_code", "site_name", "station_code", "marker_horizon_name", "core_measurement_number", "core_measurement_depth_mm", "established_date")

        if (sum(req_clms %in% names(data)) != length(req_clms)){
            stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
        }
    }

    else if (!is.null(file_path) & (stringr::str_detect(file_path, ".xls") | stringr::str_detect(file_path, ".xlsx")) & is.null(db_server)) {
        data <- readxl::read_excel(file_path)

        cols <- colnames(data)

        ## conditions: have correct columns in data frame
        ## stop and give an informative message if this isn't met
        req_clms <- c("event_date_UTC", "network_code", "park_code", "site_name", "station_code", "marker_horizon_name", "core_measurement_number", "core_measurement_depth_mm", "established_date")

        if (sum(req_clms %in% names(data)) != length(req_clms)){
            stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
        }
    }

    else if (!is.null(file_path) & !is.null(db_server)) {
        stop(paste("Please define either a database server or a file of raw SET data, not both."))
    }

    return(data)
}
