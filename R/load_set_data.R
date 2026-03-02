#' Download/Load raw SET data from a NPS data package, NPS SET database, or
#' saved file
#'
#' This function can be used to download and/or load SET data. Defaults to
#' downloading data from the most recent SET data package if neither `file_path
#' =` or `db_server =` are specified. Uses functions from the \pkg{NPSutils}
#' package to download data packages.
#'
#' @param park string (optional); A 4 character park code (e.g., "ASIS", "GATE")
#'   that can be used to filter results from the NPS SET data package or NPS SET
#'   database to a specific park. If not specified, will return data for all
#'   parks.
#'
#' @param network_code string (optional); A 4 character network code (e.g.,
#'   "NCBN", "NCRN") that can be used to filter results from the NPS SET data
#'   package or NPS SET database to a specific I&M network. If not specified,
#'   will return data for all networks.
#'
#' @param file_path string (optional); Instead of downloading the most recent
#'   SET data package or connecting to the NPS SET database, you can supply a
#'   file path to load data from a saved .csv, .xlsx, or .xls file. Can supply
#'   either a full file path or use `here::here` for a relative file path. See
#'   details below for requirements.
#'
#' @param db_server string (optional); A connection string for accessing the NPS
#'   I&M SET database back-end.
#'
#' @param db_port number (optional); A port value for accessing the NPS I&M SET
#'   database back-end.
#'
#' @note If loading in your own data from csv, see
#'   \code{\link{calc_change_cumu}} for data requirements.
#'
#' @details Note that you must be on VPN and have access to the back-end of NPS
#'   I&M SET database for the database option to work. Contact Laura Feher
#'   (lfeher at NPS.gov) for the required connection strings.
#'
#' @return A data frame of raw SET data.
#'
#' @seealso \code{\link{load_mh_data}}
#'
#' @export
#'
#' @import dplyr
#' @import DBI
#' @import odbc
#' @import readr
#' @import readxl
#' @import stringr
#'
#' @examples
#' \dontrun{
#' # Download and load raw SET data for ASIS from the most recent data package
#' df <- load_set_data(park = "ASIS")
#'
#' # Download and load raw SET data for NCBN parks from the online database
#' df <- load_set_data(network_code = "NCBN", db_server = "example.server")
#'
#' # Load raw SET data from a saved csv file
#' df <- load_set_data(file_path = "C:/Documents/Data/my_SET_data.csv")
#'
#' # Use `here::here` to load raw SET data from a relative file path
#' df <- load_set_data(file_path = here::here("Data", "my_SET_data.csv"))
#' }
#'
load_set_data <- function(park = NULL, network_code = NULL, file_path = NULL, db_server = NULL, db_port = NULL){

    # Get SET data from the most recent data package
    if (is.null(file_path) & is.null(db_server)) {

        # get_new_version_id checks if 2309427 (2025 DP) is the newest reference version of the SET DP. If its not the newest, it will set the ref_code to the new version.
        if (is.null(NPSutils::get_new_version_id(2309427))) {
            ref_code <- 2309427
        } else {
            ref_code <- NPSutils::get_new_version_id(2309427)
        }

        # download the SET data package
        NPSutils::get_data_package(ref_code)

        data_list <- NPSutils::load_data_package(ref_code)

        data <- data_list %>%
            purrr::keep(., stringr::str_detect(names(.), "pin_data")) %>%
            purrr::pluck(., 1) %>%
            { if (!is.null(park) & !is.null(network_code))
                filter(., park_code %in% toupper(park) & network_code %in% toupper(network_code) & dpl_label == "Accepted")

                else if (!is.null(park) & is.null(network_code))
                    filter(., park_code %in% toupper(park) & dpl_label == "Accepted")

                else if (is.null(park) & !is.null(network_code))
                    filter(., network_code %in% toupper(network_code) & dpl_label == "Accepted")

                else if (is.null(park) & is.null(network_code))
                    filter(., dpl_label == "Accepted")
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
            dataf <- DBI::dbSendQuery(con, 'SELECT * FROM ssrs.vw_dbx_SET_data_FINAL WHERE park_code = ?')
            data <- DBI::dbBind(dataf, list(toupper(park)))
            data <- DBI::dbFetch(data)
            DBI::dbClearResult(dataf)
        }

        # get data from SET db and filter to a specific I&M network
        else if (!is.null(network_code) & is.null(park)) {
            dataf <- DBI::dbSendQuery(con, 'SELECT * FROM ssrs.vw_dbx_SET_data_FINAL WHERE network_code = ?')
            data <- DBI::dbBind(dataf, list(toupper(network_code)))
            data <- DBI::dbFetch(data)
            DBI::dbClearResult(dataf)
        }

        # if both park and network are specified, ignore network_code and filter to park_code
        else if (!is.null(park) & !is.null(network_code)) {
            dataf <- DBI::dbSendQuery(con, 'SELECT * FROM ssrs.vw_dbx_SET_data_FINAL WHERE park_code = ?')
            data <- DBI::dbBind(dataf, list(toupper(park)))
            data <- DBI::dbFetch(data)
            DBI::dbClearResult(dataf)
        }

        # or return the whole database
        else if (is.null(park) & is.null(network_code)) {
            data <- DBI::dbGetQuery(con, 'SELECT * FROM ssrs.vw_dbx_SET_data_FINAL')
        }

        # close database connection to free up resources
        DBI::dbDisconnect(con)
    }

    else if (!is.null(file_path) & stringr::str_detect(file_path, ".csv") & is.null(db_server)) {
        data <- readr::read_csv(file_path)

        cols <- colnames(data)

        ## conditions: have correct columns in data frame
        ## stop and give an informative message if this isn't met
        req_clms <- c("event_date_UTC", "network_code", "park_code", "site_name", "station_code", "SET_direction", "pin_position", "pin_height_mm", "SET_offset_mm", "pin_length_mm")

        if (sum(req_clms %in% names(data)) != length(req_clms)){
            stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
        }
    }

    else if (!is.null(file_path) & (stringr::str_detect(file_path, ".xls") | stringr::str_detect(file_path, ".xlsx")) & is.null(db_server)) {
        data <- readxl::read_excel(file_path)

        cols <- colnames(data)

        ## conditions: have correct columns in data frame
        ## stop and give an informative message if this isn't met
        req_clms <- c("event_date_UTC", "network_code", "park_code", "site_name", "station_code", "SET_direction", "pin_position", "pin_height_mm", "SET_offset_mm", "pin_length_mm")

        if (sum(req_clms %in% names(data)) != length(req_clms)){
            stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
        }
    }

    else if (!is.null(file_path) & !is.null(db_server)) {
        stop(paste("Please define either a database server or a file of raw SET data, not both."))
    }

    return(data)

}
