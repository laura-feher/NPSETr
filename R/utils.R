detect_data_type <- function(data){

    cols <- colnames(data)

    if("pin_height_mm" %in% cols){

        ## conditions: is SET data and has correct columns in data frame
        req_clms <- c("event_date_UTC", "network_code", "park_code", "site_name", "station_code", "SET_direction", "pin_position", "pin_height_mm", "SET_offset_mm", "pin_length_mm")

        ## stop and give an informative message if this isn't met
        if(sum(req_clms %in% names(data)) != length(req_clms)){
            stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
        }

        return("SET")

    } else if("core_measurement_depth_mm" %in% cols){

        ## conditions: is MH data and has correct columns in data frame
        req_clms <- c("event_date_UTC", "network_code", "park_code", "site_name", "station_code", "marker_horizon_name", "core_measurement_number", "core_measurement_depth_mm", "established_date")

        ## stop and give an informative message if this isn't met
        if(sum(req_clms %in% names(data)) != length(req_clms)){
            stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
        }

        return("MH")

    } else {
        stop(paste("Data must include a column with either SET data or MH data. See 'data requirements' in the documentation for `calc_change_cumu()`."))
    }
}

detect_set_data <- function(data){

    cols <- colnames(data)

    if("pin_height_mm" %in% cols){

        ## conditions: is SET data and has correct columns in data frame
        return("SET")
        req_clms <- c("event_date_UTC", "network_code", "park_code", "site_name", "station_code", "SET_direction", "pin_position", "pin_height_mm", "SET_offset_mm", "pin_length_mm")

        ## stop and give an informative message if this isn't met
        if(sum(req_clms %in% names(data)) != length(req_clms)){
            stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
        }

    }
}

detect_mh_data <- function(data){

    cols <- colnames(data)

    if("core_measurement_depth_mm" %in% cols){

        ## conditions: is MH data and has correct columns in data frame
        return("MH")
        req_clms <- c("event_date_UTC", "network_code", "park_code", "site_name", "station_code", "marker_horizon_name", "core_measurement_number", "core_measurement_depth_mm", "established_date")

        ## stop and give an informative message if this isn't met
        if(sum(req_clms %in% names(data)) != length(req_clms)){
            stop(paste("Your data frame must have the following columns, with these names, but is missing at least one:", paste(req_clms, collapse = ", ")))
        }
    }
}

# remove a single grouping level
drop_groups2 = function(data, ...) {

    groups = map(groups(data), quo)
    drop = quos(...)

    if(any(!drop %in% groups)) {

                      paste(drop[!drop %in% groups], collapse=", ")
    }

    data %>% group_by(!!!setdiff(groups, drop))

}

# groups the stations into their correct sites for calculating cumulative change and rates of change
correct_site_groups <- function(station_code, site_name) {
    case_when(
        # station_code %in% c("M11-3", "M5-2", "M6-4", "M8-4") ~ "Fenced stations", # not sure if I want to group these like this or not - causes issues when grouping dates and getting cumu. change at the site-level
        station_code %in% c("G1", "G2", "G3", "GUT1", "GUT2", "GUT3") ~ "Herring River Gut",
        station_code %in% c("HighToss1", "HighToss2", "HighToss3") ~ "Herring River High Toss",
        station_code %in% c("Phrag1", "Phrag2", "Phrag3") ~ "Herring River Phrag",
        station_code %in% c("HH1", "HH2", "HH3", "HH4", "HH5", "HH6") ~ "Hatches Harbor Inside Dyke Original SETs",
        station_code %in% c("HH7", "HH8", "HH9") ~ "Hatches Harbor Outside Dyke Original SETs",
        station_code %in% c("H4", "H5", "H6") ~ "Hatches Harbor Inside Dyke Deep RSETs",
        station_code %in% c("H1", "H2", "H3") ~ "Hatches Harbor Outside Dyke Deep RSETs",
        station_code %in% c("EE1S", "EE2S", "EE3S") ~ "Elders East Shallow SETs",
        station_code %in% c("JR1S", "JR2S", "JR3S") ~ "Jamaica Bay Reference Shallow SETs",
        station_code %in% c("Creek1", "Creek2", "Creek3") ~ "Dyke Marsh Creek",
        station_code %in% c("Int1", "Int2", "Int3") ~ "Dyke Marsh Interior",
        station_code %in% c("River1", "River1A", "River2", "River3") ~ "Dyke Marsh River",
        TRUE ~ site_name
    )
}

plot_rate_labels <- function(data, level, groups, data_type) {

    if(data_type == "SET") {
        lab_prefix <- "SEC: "
    } else if(data_type == "MH") {
        lab_prefix <- "VA: "
    }

    data %>%
        calc_linear_rates(., level = level) %>%
        tidyr::unite("grouping", all_of(groups), remove = FALSE) %>%
        mutate(format_rate = if_else(abs(round(rate, 2)) >= 0.01, format(round(rate, 2), nsmall = 2), as.character(signif(rate))),
               format_rate_se = if_else(abs(round(rate_se, 2)) >= 0.01, format(round(rate_se, 2), nsmall = 2), as.character(signif(rate_se))),
               format_r2  = format(round(rate_r2, 2), nsmall =2),
               format_p = case_when(rate_p > 0.05 ~ "ns",
                                    rate_p <= 0.05 & rate_p > 0.01 ~ "0.05",
                                    rate_p <= 0.01 & rate_p > 0.001 ~ "0.01",
                                    rate_p <= 0.001 ~ "0.001")) %>%
        mutate(rate_label = paste0(lab_prefix, format_rate, " ± ", format_rate_se, " mm/yr"),
               r2p_label_sig = deparse(bquote(italic(r)^2~"="~.(format_r2)*plain(",")~italic(p)~"="~italic(.(format_p)))),
               r2p_label_ns = deparse(bquote(italic(r)^2~"="~.(format_r2)*plain(",")~italic(p)~"="~italic(.(format_p)))),
               r2p_label = if_else(rate_p >= 0.05, r2p_label_ns, r2p_label_sig)
        )
}

noaa_tide_gauges <- function(park, nauset) {

    valid_parks <- c("COLO", "ASIS", "GWMP", "NACE", "GATE", "FIIS", "CACO", "BOHA", "ACAD", "BISC", "SARI", "VIIS", "CANA", "CAHA", "CALO", "CUIS", "FOFR", "FOMA", "FOPU", "TIMU")

    if (!park %in% valid_parks) {
        stop(park, " is an invalid park code. Valid park codes are:\n\t",
             paste(valid_parks, collapse = ", "))
    }

    if (park == "COLO") {
        station_num <- 8638610
        PSMSL_Site <- "SEWELLS_POINT"
    } else if (park == "ASIS") {
        station_num <- 8570283
        PSMSL_Site <- "OCEAN_CITY_INLET"
    } else if (park == "GWMP" | park == "NACE") {
        station_num <- 8594900
        PSMSL_Site <- "WASHINGTON_DC"
    } else if (park == "GATE" | park == "FIIS") {
        station_num <- 8531680
        PSMSL_Site <- "SANDY_HOOK"
    } else if (park == "BOHA" | (park == "CACO" & !nauset)) {
        station_num <- 8443970
        PSMSL_Site <- "BOSTON"
    } else if (park == "ACAD") {
        station_num <- 8413320
        PSMSL_Site <- "BAR_HARBOR"
    } else if (park == "CACO" & nauset) {
        station_num <- 8449130 # used Nantucket for CACO-Nauset because the Chatham NOAA gauge didn't have a published SLR rate
        PSMSL_Site <- "NANTUCKET_ISLAND"
    } else if (park == "BISC") {
        station_num <- 8723214
        PSMSL_Site <- "VIRGINIA_KEY"
    } else if (park == "SARI") {
        station_num <- 9751401 # used Lime Tree Bay for SARI because the Christiansted NOAA gauge didn't have a published SLR rate
        PSMSL_Site <- "LIME_TREE_BAY"
    } else if (park == "VIIS") {
        station_num <- 9751639 # used Charlotte Amalie for VIIS because the Lameshur Bay NOAA gauge didn't have a published SLR rate
        PSMSL_Site <- "CHARLOTTE_AMALIE"
    } else if (park == "CANA") {
        station_num <- 8721604
        PSMSL_Site <- "TRIDENT_PIER"
    } else if (park == "CAHA") {
        station_num <- 8652587 # used Oregon Inlet for long-term rate at CAHA because the Cape Hatteras NOAA gauge didn't have a published SLR rate
        PSMSL_Site <- "CAPE_HATTERAS"
    } else if (park == "CALO") {
        station_num <- 8656483
        PSMSL_Site <- "BEAUFORT"
    } else if (park == "CUIS" | park == "FOFR") {
        station_num <- 8720030 # used Fernandina for CUIS and FOFR because the Kings Bay NOAA gauge didn't have a published SLR rate
        PSMSL_Site <- "FERNANDINA_BEACH"
    } else if (park == "FOMA" | park == "TIMU") {
        station_num <- 8720218
        PSMSL_Site <- "MAYPORT (BAR PILOTS DOCK)"
    } else if (park == "FOPU") {
        station_num <- 8670870
        PSMSL_Site <- "FORT_PULASKI"
    } else {
        stop("Please supply a valid 4 letter park code")
    }

    return(list("station_num" = station_num, "PSMSL_Site" = PSMSL_Site))

}
