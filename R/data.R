#' Example SET data
#'
#' A sample of data from M11 at ASIS
#'
#' @name example_sets
#' @format A data frame with 3888 rows and 11 variables:
#' \describe{
#'   \item{event_date_UTC}{Date UTC; measurement date, yyyy-mm-dd format}
#'   \item{network_code}{chr;, 4-letter I&M network code}
#'   \item{park_code}{chr; 4-letter NPS park code}
#'   \item{site_name}{chr; site name}
#'   \item{station_code}{chr; station/SET code}
#'   \item{SET_direction}{chr; one of four arm positions ("A", "B", "C", or "D")}
#'   \item{pin_position}{int; one of nine pins on each arm}
#'   \item{SET_offset_mm}{num; the height of the SET arm above the benchmark in mm}
#'   \item{pin_length_mm}{num; the length of the pins in mm}
#'   \item{pin_height_mm}{num; height of pin above arm, in mm}
#'   \item{set_type}{chr; the type of SET benchmark}
#' }
"example_sets"

#' Example MH data
#'
#' A sample of marker horizon data from M11 at ASIS
#'
#' @name example_mh
#' @format A data frame with 664 rows and 9 variables:
#' \describe{
#'   \item{event_date_UTC}{Date UTC; measurement date, yyyy-mm-dd format}
#'   \item{network_code}{chr;, 4-letter I&M network code}
#'   \item{park_code}{chr; 4-letter NPS park code}
#'   \item{site_name}{chr; site name}
#'   \item{station_code}{chr; station code}
#'   \item{marker_horizon_name}{chr; marker horizon replicate name}
#'   \item{core_measurement_number}{dbl; number of the measurement taken from a single core}
#'   \item{core_measurement_depth}{dbl; measured depth to the marker horizon in mm}
#'   \item{established_date}{Date UTC; date that the marker horizon plot was established, yyyy-mm-dd format}
#' }
"example_mh"

#' Future sea level projections from Sweet et al. 2022
#'
#' Future sea level projections from Sweet et al. 2022 pulled from "./data/Sea_Level_Rise_Datasets_2022/SLR_TF U.S. Sea Level Projects.csv".
#'
#' @name future_slr_projections_sweet_2022
#' @format A data frame with 664 rows and 9 variables:
#' \describe{
#'   \item{PSMSL Site}{chr; site name}
#'   \item{PSMSL ID}{dbl;, for GMSL, 0; for tide gauge locations, the Permanant Service for Mean Sea Level (http://www.psmsl.org/) Tide Gauge Identification (ID) Number; for grid points, 10E9 + (10E5 x (90 - latitude)) + (longitude x 10)}
#'   \item{NOAA Name}{chr; the station name for tide gauge locations}
#'   \item{NOAA ID}{chr; site name}
#'   \item{Scenario}{chr; For each of the 5 GMSL scenarios (identified by the rise amounts in meters by 2100--0.3 m , 0.5 m. 1.0 m, 1.5 m and 2.0 m), there is a low, medium (med) and high value, corresponding to the 17th, 50th, and 83rd percentiles.}
#'   \item{RSL2005 (cm)}{dbl; predicted relative sea level by 2005 in centimeters}
#'   \item{RSL2020 (cm)}{dbl; predicted relative sea level by 2020 in centimeters}
#'   \item{RSL2030 (cm)}{dbl; predicted relative sea level by 2030 in centimeters}
#'   \item{RSL2040 (cm)}{dbl; predicted relative sea level by 2040 in centimeters}
#'   \item{RSL2050 (cm)}{dbl; predicted relative sea level by 2050 in centimeters}
#'   \item{RSL2060 (cm)}{dbl; predicted relative sea level by 2060 in centimeters}
#'   \item{RSL2070 (cm)}{dbl; predicted relative sea level by 2070 in centimeters}
#'   \item{RSL2080 (cm)}{dbl; predicted relative sea level by 2080 in centimeters}
#'   \item{RSL2090 (cm)}{dbl; predicted relative sea level by 2090 in centimeters}
#'   \item{RSL2100 (cm)}{dbl; predicted relative sea level by 2100 in centimeters}
#'   \item{RSL2110 (cm)}{dbl; predicted relative sea level by 2110 in centimeters}
#'   \item{RSL2120 (cm)}{dbl; predicted relative sea level by 2120 in centimeters}
#'   \item{RSL2130 (cm)}{dbl; predicted relative sea level by 2130 in centimeters}
#'   \item{RSL2140 (cm)}{dbl; predicted relative sea level by 2140 in centimeters}
#'   \item{RSL2150 (cm)}{dbl; predicted relative sea level by 2150 in centimeters}
#' }
"future_slr_projections_sweet_2022"
