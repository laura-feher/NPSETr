#' Get park-specific sea-level data from NOAA tides and currents
#'
#' This function downloads sea-level data for the nearest NOAA tide gauge from
#' the tides and currents website (https://tidesandcurrents.noaa.gov/sltrends/).
#'
#' @param park A string (required). A capitalized four-letter park code. One of:
#'  * `"ACAD"`: Acadia National Park - gauge 8413320 Bar Harbor, ME.
#'  * `"BOHA"`: Boston Harbor Islands National Recreation Area - gauge 8443970 Boston, MA.
#'  * `"CACO"`: Cape Cod National Seashore - 8443970 (Boston, MA) or 8449130 for Nauset (Nantucket Island, MA).
#'  * `"FIIS"`: Fire Island National Seashore - gauge 8531680 Sandy Hook, NJ.
#'  * `"GATE"`: Gateway National Recreation Area - gauge 8531680 Sandy Hook, NJ.
#'  * `"NACE"`: National Capital Area East - gauge 8594900 Washington, DC.
#'  * `"GWMP"`: George Washington Memorial Parkway - gauge 8594900 Washington, DC.
#'  * `"ASIS"`: Assateague Island National Seashore - gauge 8570283 (Ocean City Pier, MD) for long-term/recent SLR, gauge 8570280 (Ocean City Inlet, MD) for future SLR .
#'  * `"COLO"`: Colonial National Historical Park - gauge 8638610 Sewells Point, VA.
#'  * `"CAHA"`: Cape Hatteras National Seashore - gauge 8652587 (Oregon Inlet Marina, NC) for long-term/recent SLR, gauge 8654400 (Cape Hatteras, NC) for future SLR.
#'  * `"CALO"`: Cape Lookout National Seashore - gauge 8656483 Beaufort, NC.
#'  * `"FOPU"`: Fort Pulaski National Monument - gauge 8670870 Fort Pulaski, GA.
#'  * `"FOFR"`: Fort Frederica National Monument - gauge 8720030 Fernandina Beach, GA.
#'  * `"CUIS"`: Cumberland Island National Seashore - gauge 8720030 Fernandina Beach, GA.
#'  * `"TIMU"`: Timucuan Ecological and Historic Preserve - gauge 8720218 Mayport, FL (Bar Pilots Dock).
#'  * `"FOMA"`: Fort Matanzas National Monument - gauge 8720218 Mayport, FL (Bar Pilots Dock).
#'  * `"CANA"`: Canaveral National Seashore - gauge 8721604 Trident Pier, FL.
#'  * `"BISC"`: Biscayne National Park - gauge 8723214 Virginia Key, FL.
#'  * `"VIIS"`: Virgin Islands National Park - gauge 9751639 Charlotte Amalie, St. Thomas, USVI.
#'  * `"SARI"`: Salt River Bay National Historic Park and Ecological Preserve - gauge 9751401 Lime Tree Bay, St. Croix, USVI.
#'
#' @param nauset Logical (optional). Used to select the Nauset gauge for CACO;
#'   defaults to FALSE.
#'
#' @param start_year A four digit year (optional). The starting year that you
#'   want to get sea-level data for. If start_year is not supplied (default),
#'   returns data for the entire period of record for that station.
#'
#' @param end_year An four digit year (optional). The ending year that you want
#'   to get sea-level data for. If end_year is not supplied (default), returns
#'   data for the entire period of record for that station.
#'
#' @details Sea-level data is downloaded from the NOAA tides and currents
#'   website https://tidesandcurrents.noaa.gov/. Note that monthly mean sea
#'   level values are relative to the most recent mean sea-level datum
#'   established by CO-OPS
#'   (https://tidesandcurrents.noaa.gov/datum_options.html). The column "yr" in
#'   the output represents decimal years since the first date.
#'
#' @returns A data frame with the relative sea-level data downloaded from the
#'   NOAA tides and currents website.
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#' @importFrom readr read_csv
#' @importFrom tidyr nest
#'
#' @examples
#' get_sea_level_data(park = "ASIS")
#'
#' get_sea_level_data(park = "CACO", nauset = TRUE)
#'
#' get_sea_level_data(park = "COLO", start_year = 2008, end_year = 2018)
#'
get_sea_level_data <- function (park, nauset = FALSE, start_year = NULL, end_year = NULL){

    noaa_ids <- noaa_tide_gauges(park = park, nauset = nauset)

    suppressWarnings(dat <- as.data.frame(readr::read_csv(paste0("https://tidesandcurrents.noaa.gov/sltrends/data/", noaa_ids$station_num, "_meantrend.csv"),
                                         skip = 5, show_col_types = FALSE)))

    data <- dat %>%
        mutate(date = as.Date(paste0(Month, "/", "1/", Year), format = "%m/%d/%Y")) %>%
        {if (!is.null(start_year) & is.null(end_year))
            filter(., Year >= start_year)
            else if(!is.null(end_year) & is.null(start_year))
                filter(., Year <= end_year)
            else if(!is.null(start_year) & !is.null(end_year))
                filter(., Year >= start_year & Year <= end_year)
            else .} %>%
        mutate(.,
               park_code = park,
               station_num = noaa_ids$station_num,
               yr = as.numeric(date - min(date))/365.25,
               object_type = "sea level data",
               across(c(Monthly_MSL, Linear_Trend, High_Conf., Low_Conf.), ~.x * 1000, .names = "{.col}_mm")) %>% # convert to millimeters for comparisons to SET/MH data
        select(park_code, station_num, Year, Month, date, yr, Monthly_MSL_mm, Linear_Trend_mm, High_Conf._mm, Low_Conf._mm, object_type)

    return(data)
}
