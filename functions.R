############################ SCRIPT INFORMATION ################################

# Script Title: Component functions to the nc_covariates function.

# Script Author: Rory Macklin (rmacklin@birdscanada.org)

########################## LOAD NECESSARY PACKAGES #############################

if(system.file(package = "librarian") == "") {
  
  install.packages("librarian")
  
}

if(system.file(package = "remotes") == "") {
  
  install.packages("remotes")
  
}

if(system.file(package = "naturecounts") == "") {
  
  install.packages("naturecounts", 
                   repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
                             CRAN = 'https://cloud.r-project.org'))
  
}


remotes::install_github("bluegreen-labs/appeears", build_vignettes = TRUE)

librarian::shelf(naturecounts, tidyverse, sf, "USEPA/elevatr", terra, exactextractr, geodata,
                 biooracler, "rspatial/luna", landscapemetrics, measurements, appeears)


###################### PREEXISTING NATURECOUNTS FUNCTIONS ######################

doy_check <- function(s) {
  stp <- FALSE
  
  if (stringr::str_detect(s, "^[:digit:]+$")) {
    s <- as.numeric(s)
  }
  if (is.numeric(s)) {
    if (s < 0 | s > 366) {
      stp <- TRUE
    }
    if (round(s) != s) stp <- TRUE
  } else {
    s <- suppressWarnings(lubridate::ymd_hms(s, truncated = 4)) %>%
      lubridate::yday()
    if (is.na(s)) stp <- TRUE
  }
  if (stp) {
    stop(
      "Day of year must be either a date (YM or YMD), ",
      "or a whole number (1-366)",
      call. = FALSE
    )
  }
  s
}

have_pkg_check <- function(pkgs) {
  # TODO: remove suppression when rnaturalearth resolved
  
  failed <- purrr::map_lgl(pkgs, ~ !requireNamespace(.x, quietly = TRUE)) %>%
    suppressPackageStartupMessages()
  
  if (any(failed)) {
    if ("sf" %in% pkgs[!failed] && utils::packageVersion("sf") < "1.0-9") {
      pkgs[pkgs == "sf"] <- "sf (>v1.0-9)"
    }
    
    stop(
      "This function requires packages: '",
      paste0(pkgs[failed], collapse = "', '"),
      "'",
      "\nPlease install with `install.packages(\"",
      paste0(pkgs[failed], collapse = "\", \""),
      "\")` then try again",
      call. = FALSE
    )
  }
}

######################### NEW FORMATTING FUNCTIONS #############################

# New data table for sources of covariate data.

nc_covariate_table <- function() {
  
  cov.table <- data.frame(covariate_name = c("modis_lctype1", "modis_lctype2", "modis_lctype3", "modis_lctype4", "modis_lctype5", "modis_snow", "modis_ndvi", "modis_evi", "elevation", "worldclim_tavg", "worldclim_tmax", "worldclim_tmin", "worldclim_prec", "worldclim_srad", "worldclim_wind", "worldclim_vapr", "scanfi_biomass", "scanfi_closure", "scanfi_height", "scanfi_nfilc", "scanfi_balsamfir", "scanfi_blackspruce", "scanfi_douglasfir", "scanfi_jackpine", "scanfi_lodgepolepine", "scanfi_ponderosapine", "scanfi_tamarack", "scanfi_whiteredpine", "scanfi_broadleaf", "scanfi_otherconifer", "daymet_dayl", "daymet_prcp", "dayment_srad", "daymet_swe", "daymet_tmax", "daymet_tmin", "daymet_vp"),
                          covariate_source = c("MODIS Land Cover - IGBP global vegetation classification scheme", "MODIS Land Cover - University of Maryland (UMD) scheme", "MODIS Land Cover - MODIS-derived LAI/fPAR scheme", "MODIS Land Cover - MODIS-derived Net Primary Production scheme", "MODIS Land Cover - Plant Functional Type (PFT) scheme", "MODIS Snow Cover", "MODIS Vegetation Indices - Normalized Difference Vegetation Index", "MODIS Vegetation Indices - Enhanced Vegetation Index", "AWS Terrain Tiles Elevation (m)", "WorldClim - Monthly Average Temperature (degC), 1970-2000", "WorldClim - Monthly Maximum Temperature (degC), 1970-2000", "WorldClim - Monthly Minimum Temperature (degC), 1970-2000", "WorldClim - Monthly Precipitation (mm), 1970-2000", "WorldClim - Monthly Solar Radiation (kJ/m^2/day), 1970-2000", "WorldClim - Monthly Average Wind Speed (m/s), 1970-2000", "WorldClim - Monthly Average Water Vapor Pressure (kPa), 1970-2000", "SCANFI - Biomass (tons/ha)", "SCANFI - Crown closure (% covered by tree canopy)", "SCANFI - Height (m)", "SCANFI - NFI land cover class", "SCANFI - Balsam Fir cover proportion of total crown cover", "SCANFI - Black Spruce cover proportion of total crown cover", "SCANFI - Douglas Fir cover proportion of total crown cover", "SCANFI - Jack Pine cover proportion of total crown cover", "SCANFI - Lodgepole Pine cover proportion of total crown cover", "SCANFI - Ponderosa Pine cover proportion of total crown cover", "SCANFI - Tamarack cover proportion of total crown cover", "SCANFI - White and Red Pine cover proportion of total crown cover", "SCANFI - Broadleaf tree species cover proportion of total crown cover", "SCANFI - Other Conifer Species cover proportion of total crown cover", "Daymet - Daylength (s/day)", "Daymet - Precipitation (mm/day)", "Daymet - Shortwave radiation (W/m^2)", "Daymet - Snow water equivalent (kg/m^2)", "Daymet - Maximum air temperature (degrees C)", "Daymet - Minimum air temperature (degrees C)", "Daymet - Water vapor pressure (Pa)"),
                          covariate_source_specific = c(rep("MCD12Q1", times = 5), "MOD10A1", rep("MOD13A1", times = 2), NA, rep("WorldClim Ver. 2.1", times = 7), rep("SCANFI Ver. 1.2", times = 14), rep("DAYMET Ver. 004", times = 7)),
                          temporal_resolution = c(rep("Annual", times = 5), "Daily", rep("16-Day", times = 2), rep("Static", times = 22), rep("Daily", times = 7)),
                          spatial_resolution = c(rep("500 m", times = 8), "~600-800m", rep("~1 km^2", times = 7), rep("30 m", times = 14), rep("1 km", times = 7)),
                          via = c(rep("luna", times = 8), "elevatr", rep("geodata", times = 7), rep("Direct Download", times = 14), rep("appeears", times = 7)),
                          documentation = c(rep("http://doi.org/10.5067/MODIS/MCD12Q1.006", times = 5), "http://doi.org/10.5067/MODIS/MOD10A1.061", rep("https://doi.org/10.5067/MODIS/MOD13A1.061", times = 2), "https://github.com/USEPA/elevatr", rep("https://worldclim.org/data/worldclim21.html", times = 7), rep("https://doi.org/10.23687/18e6a919-53fd-41ce-b4e2-44a9707c52dc", times = 14), rep("https://doi.org/10.3334/ORNLDAAC/1840", times = 7)))
  
  return(cov.table)
  
  
}

# Function to check data format and pull the information we need for extracting
# from external sources.

covariate_fmt_check <- function(data) {
  
  # Check packages
  
  have_pkg_check(c("sf", "terra"))
  
  if("sf" %in% class(data)) {
    
    data_type <- "sf"
    
    data_geometry <- as.character(sf::st_geometry_type(data, by_geometry = FALSE))
    
    if(data_geometry == "GEOMETRY") {
      
      stop("Mixed sf geometries detected. Please provide a set of only POINT geometries or only POLYGON geometries.",
           call. = FALSE)
      
    }
    
    if(!(data_geometry %in% c("POINT", "POLYGON"))) {
      
      stop("sf object provided, but not a set of POINT or POLYGON geometries.",
           call. = FALSE)
      
    }
    
    return(list(type = data_type, geometry = data_geometry))
    
  } else if("SpatVector" %in% class(data)) {
    
    data_type <- "terra"
    
    data_geometry <- terra::geomtype(data)
    
    if(!(data_geometry %in% c("points", "polygons"))) {
      
      stop("terra object provided, but not a set of points or polygons.",
           call. = FALSE)
      
    }
    
    return(list(type = data_type, geometry = data_geometry))
    
  } else if(is.data.frame(data)) {
    
    data_type <- "data.frame"
    
    return(list(type = data_type))
    
  } else {
    
    stop("Invalid data format. Please provide data as either a dataframe, sf object with either `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry.",
         call. = FALSE)
    
  }
  
}

# Function to standardize formatting of data for extraction.

data_fmt <- function(data,
                     site_name = NULL,
                     coord_lon = NULL, # as in cosewic_ranges
                     coord_lat = NULL, # as in cosewic_ranges
                     date_year = NULL,
                     date_month = NULL,
                     date_day = NULL,
                     date_lubridate = NULL,
                     date_ordinal = NULL) {
  
  # Check packages
  
  have_pkg_check(c("dplyr", "rlang", "lubridate", "stringr", "sf", "terra", "tidyterra"))
  
  # Check data type - we need either a dataframe, sf points object, sf polygon,
  # or terra SpatVector.
  
  input_fmt <- covariate_fmt_check(data)
  
  if(input_fmt$type %in% c("sf", "terra") & (!is.null(coord_lon) | !is.null(coord_lat))) {
    
    warning("sf or terra object provided as well as a lat/long column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored.",
            call. = FALSE)
    
    coord_lon <- NULL
    coord_lat <- NULL
    
  }
  
  # Check that all specified column names are present in the data
  
  specified_cols <- c(site_name, coord_lon, coord_lat, date_year, date_month, date_day, date_lubridate, date_ordinal)
  
  specified_cols <- specified_cols[!is.null(specified_cols)]
  
  data_cols <- names(data)
    
  if(!(all(specified_cols %in% data_cols))) {
      
    stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
         call. = FALSE)
      
    }
  
  # Conform specified columns to naturecounts default column names
  
  if(!is.null(site_name)) {
    
    data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
    
  }
  
  data$SiteCode <- as.character(data$SiteCode)
  
  if(input_fmt$type == "data.frame") {
    
    if(!is.null(coord_lon)) {
      
      data <- dplyr::mutate(data, longitude = !!rlang::sym(coord_lon))
      
    }
    
    data$longitude <- as.numeric(data$longitude)
    
    if(!is.null(coord_lat)) {
      
      data <- dplyr::mutate(data, latitude = !!rlang::sym(coord_lat))
      
    }
    
    data$latitude <- as.numeric(data$latitude)
    
  }

  if(!is.null(date_year)) {
    
    data <- dplyr::mutate(data, survey_year = !!rlang::sym(date_year))
    
  }
  
  if(!is.null(date_month)) {
    
    data <- dplyr::mutate(data, survey_month = !!rlang::sym(date_month))
    
  }
  
  if(!is.null(date_day)) {
    
    data <- dplyr::mutate(data, survey_day = !!rlang::sym(date_day))
    
  }
  
  # If a date in lubridate or ordinal format is provided, make year, month and day columns.
  
  if(!is.null(date_lubridate)) {
    
    data <- dplyr::mutate(data, date = !!rlang::sym(date_lubridate))
    
    if(!lubridate::is.Date(data$date)) {
      
      stop("Column ", date_lubridate, " expected to be in lubridate `Date` format, but is not.",
           call. = FALSE)
      
    }
    
    if(!lubridate::is.instant(data$date)) {
      
      stop("Column ", date_lubridate, " expected to be a single instant in time, but is not.",
           call. = FALSE)
      
    }
    
    if(!all(data$date <= as.Date(Sys.Date()))) {
      
      stop("Some dates are in the future! Covariate data only available for data in the past.",
           call. = FALSE)
      
    }
    
    if(!is.null(date_year)  | !is.null(date_month) | !is.null(date_day) | !is.null(date_ordinal)) {
      
      date_cols <- c(date_lubridate, date_year, date_month, date_day, date_ordinal)
      date_cols <- date_cols[!is.null(date_cols)]
      
      warning(paste0("Multiple date column options provided including ", stringr::str_flatten_comma(date_cols), ". The data in ", date_lubridate, " will be used."),
              call. = FALSE)
      
    }
    
    data$survey_year <- lubridate::year(data$date)
    
    data$survey_month <- lubridate::month(data$date)
    
    data$survey_day <-lubridate::day(data$date)
    
    date_ordinal <- NULL
    
  }
  
  if(!is.null(date_ordinal)) {
    
    data <- dplyr::mutate(data, doy = !!rlang::sym(date_ordinal))
    
    if(!("survey_year" %in% names(data))) {
      
      stop("If providing an ordinal date, year data must accompany it. Please provide a column with associated year data using the `date_year` argument.",
           call. = FALSE)
      
    }
    
    
    for(i in data$doy) {
      
      doy_check(i)
      
    }
    
    if(!is.null(date_month) | !is.null(date_day)) {

      warning(paste0("Dates derived from ordinal dates will supercede provided month and/or day data"),
              call. = FALSE)
      
    }
    
    if(is.numeric(data$doy)) {
      
      data$date <- as.Date(paste0(data$survey_year, "-01-01")) + data$doy - 1
      
    }
    
    if(lubridate::is.Date(data$doy)) {
      
      data$date <- as.Date(paste0(data$survey_year, "-01-01")) + lubridate::yday(data$doy) - 1
      
    }
    
    data$survey_month <- lubridate::month(data$date)
    
    data$survey_day <- lubridate::day(data$date)
    
  }
  
  data$survey_year <- as.numeric(data$survey_year)
  
  data$survey_month <- as.numeric(data$survey_month)
  
  data$survey_day <- as.numeric(data$survey_month)
  
  if(input_fmt$type == "data.frame") {
    
    data <- dplyr::select(data, SiteCode, latitude, longitude, survey_year, survey_month, survey_day)
    
    if(NA %in% unique(data$latitude) | NA %in% unique(data$longitude)) {
      
      warning("Some sites missing latitude/longitude data will be ignored.",
              call. = FALSE)
      
      data <- dplyr::filter(data, !(is.na(latitude) | is.na(longitude)))
      
    }
  }
  
  # Handle missing SiteCodes
  
  if(TRUE %in% is.na(data$SiteCode)) {
    
    missing_sitecode <- data %>%
      dplyr::select(SiteCode, latitude, longitude) %>%
      dplyr::filter(is.na(SiteCode), !(is.na(latitude) | is.na(longitude))) %>%
      dplyr::distinct()
    
    for(i in 1:nrow(missing_sitecode)) {
      
      missing_sitecode$SiteCode[i] <- paste0("FilledSiteCode", i)
      
    }
    
    for(i in missing_sitecode$latitude) {
      
      for(j in missing_sitecode$longitude[missing_sitecode$latitude == i]) {
        
        data$SiteCode[data$latitude == i & data$longitude == j] <- missing_sitecode$SiteCode[missing_sitecode$latitude == i & missing_sitecode$longitude == j]
        
      }
      
    }
    
  }
  
  if(input_fmt$type == "data.frame") {
    
    data <- data %>%
      dplyr::distinct() %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      sf::st_transform("ESRI:102001")
    
  }
  
  if(input_fmt$type == "sf") {
    
    data <- dplyr::select(data, SiteCode, geometry, survey_year, survey_month, survey_day) %>%
      dplyr::distinct() %>%
      sf::st_transform("ESRI:102001")
    
  }
  
  if(input_fmt$type == "terra") {
    
    data <- tidyterra::select(data, SiteCode, survey_year, survey_month, survey_day) %>%
      tidyterra::distinct() %>%
      terra::project("ESRI:102001")
    
  }
  

  
  return(data)
  
}

# Function to buffer data by a specified radius

data_buff <- function(data,
                      buffer = FALSE,
                      buffer_radius = 500, 
                      buffer_units = "m") {
  
  if(buffer == TRUE) {
    
    # Check packages
    
    have_pkg_check(c("measurements", "sf", "terra"))
    
    # Check data is in the desired format
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Buffering requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    buffer_radius <- as.numeric(buffer_radius)
    
    if(!(buffer_units %in% c("m", "km", "ft", "yd", "mi", "naut_mi"))) {
      
      stop("Buffer units not recognized: please set buffer_units to one of 'm' [metres], 'km' [kilometers], 'ft' [feet], 'yd' [yards], 'mi' [miles], or 'naut_mi' [nautical miles].",
           call. = FALSE)
      
    }
    
    message("Buffering sites by ", buffer_radius, buffer_units, " radius", ifelse(buffer_radius == 500 & buffer_units == "m", " (default)", ""), ".")
    
    if(input_fmt$type == "sf") {
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        data <- sf::st_transform(data, "ESRI:102001")
        
      }
      
      if(input_fmt$geometry == "POLYGON") {
        
        warning("sf POLYGON geometry provided. Existing polygons will be buffered by an additional ", buffer_radius, buffer_units, ".",
                call. = FALSE)
        
      }
      
      data <- sf::st_buffer(data, measurements::conv_unit(x = buffer_radius, from = buffer_units, to = "m"))
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        data <- sf::st_transform(data, orig_crs)
        
      }
      
    }
    
    if(input_fmt$type == "terra") {
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        data <- terra::project(data, "ESRI:102001")
        
      }
      
      if(input_fmt$geometry == "polygons") {
        
        warning("terra polygons provided. Existing polygons will be buffered by an additional ", buffer_radius, buffer_units, ".",
                call. = FALSE)
        
      }
      
      data <- terra::buffer(data, measurements::conv_unit(x = buffer_radius, from = buffer_units, to = "m"))
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        data <- terra::project(data, orig_crs)
        
      }
    }
    
  }
  
  return(data)
  
}

############################ LANDCOVER FUNCTIONS ###############################

landcover_download <- function(data,
                               covariates = NULL,
                               ed_email = NULL,
                               ed_password = NULL,
                               site_name = NULL,
                               date_year = NULL,
                               dl_path = NULL) {
  
  
  if("modis_lctype1" %in% covariates | "modis_lctype2" %in% covariates | "modis_lctype3" %in% covariates | "modis_lctype4" %in% covariates | "modis_lctype5" %in% covariates) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "stringr", "sf", "terra", "luna", "landscapemetrics"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    if(is.null(ed_email) | is.null(ed_password)) {
      
      stop("MODIS data requested but Earthdata system login information not supplied. Please register at https://urs.earthdata.nasa.gov/users/new and supply using `ed_email` and `ed_password` parameters.",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name, date_year)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(!is.null(date_year)) {
      
      data <- dplyr::mutate(data, survey_year = !!rlang::sym(date_year))
      
    }
    
    data$survey_year <- as.numeric(data$survey_year)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_transform("ESRI:102001") %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      } else {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      }
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::project("ESRI:102001") %>%
          terra::buffer(20000)
        
      } else {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::buffer(20000)
        
      }
      
      data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
      
    }
    
    if(is.null(dl_path) & !dir.exists("./modis/MCD12Q1")) {
      
      dir.create("./modis/MCD12Q1", recursive = T)
      
    }
    
    if(!is.null(dl_path) & !dir.exists(paste0(dl_path, "/modis/MCD12Q1"))) {
      
      dir.create(paste0(dl_path, "/modis/MCD12Q1"), recursive = T)
      
    }
    
    modis.files <- luna::getNASA(product = "MCD12Q1",
                                 start = paste0(min(data$survey_year), "-01-01"),
                                 end = paste0(max(data$survey_year), "-12-31"),
                                 aoi = ext(project(study_area, "epsg:4326")),
                                 download = TRUE,
                                 overwrite = FALSE,
                                 path = ifelse(is.null(dl_path), "./modis/MCD12Q1", paste0(dl_path, "/modis/MCD12Q1")),
                                 username = ed_email,
                                 password = ed_password)
    
    return(modis.files)
    
  }
  
}

landcover_extract <- function(data,
                              covariates = NULL,
                              landcover_files = NULL,
                              site_name = NULL,
                              date_year = NULL,
                              dl_path = NULL,
                              retain = TRUE) {
  
  if("modis_lctype1" %in% covariates | "modis_lctype2" %in% covariates | "modis_lctype3" %in% covariates | "modis_lctype4" %in% covariates | "modis_lctype5" %in% covariates) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "stringr", "sf", "terra", "luna", "landscapemetrics"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    if(is.null(landcover_files)) {
      
      stop("No landcover files provided to extract from. Please provide a vector containing filepaths of all necessary MODIS files for your data. Data can be downloaded using landcover_download.",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name, date_year)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(!is.null(date_year)) {
      
      data <- dplyr::mutate(data, survey_year = !!rlang::sym(date_year))
      
    }
    
    data$survey_year <- as.numeric(data$survey_year)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
      
    }
    
    modis.files <- luna::modisDate(landcover_files)
    modis.files <- cbind(modis.files, as.data.frame(luna::modisExtent(modis.files$filename)))
  
    modis.files$year <- as.numeric(modis.files$year)
    
    modis.match <- data %>%
      dplyr::select(SiteCode, survey_year, geometry) %>%
      sf::st_transform(terra::crs(terra::rast(modis.files$filename[1])))
    
    if(buffered == TRUE) {
      
      suppressWarnings(
        
        modis.match <- cbind(modis.match, sf::st_coordinates(sf::st_centroid(modis.match)))
        
      )
      
      
    } else {
      
      modis.match <- cbind(modis.match, sf::st_coordinates(modis.match))
      
    }
    
    for(i in sort(unique(modis.match$survey_year))) {
      
      if(!(i %in% modis.files$year)) {
        
        warning(paste0("MODIS data not available for ", i, " - using data from nearest year (", unique(modis.files$year)[which(abs(i-unique(modis.files$year)) == min(abs(i-unique(modis.files$year))))], ")."),
                call. = FALSE)
        
      }
      
    }
    
    out_of_range <- c()
    
    for(i in unique(modis.match$SiteCode)) {
      
      for(j in unique(modis.match$survey_year[modis.match$SiteCode == i])) {
        
        tmp <- dplyr::filter(modis.match, SiteCode == i, survey_year == j) %>%
          dplyr::distinct()
        
        if(all(tmp$X > modis.files$xmax) | all(tmp$X < modis.files$xmin) | all(tmp$Y > modis.files$ymax) | all(tmp$Y < modis.files$ymin)) {
          
          warning("Site ", i, " falls outside of the spatial extent of the MODIS files provided. No value will be assigned.", 
                  call. = FALSE)
          
          out_of_range <- c(out_of_range, i)
          
        } else {
          
          suppressWarnings(
            
            if(!(j %in% modis.files$year)) {
              
              modis.match[modis.match$SiteCode == i & modis.match$survey_year == j, "filename"] <- modis.files$filename[modis.files$year == unique(modis.files$year)[which(abs(j-unique(modis.files$year)) == abs(min(j-unique(modis.files$year))))] & modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y]
              
            } else {
              
              modis.match[modis.match$SiteCode == i & modis.match$survey_year == j, "filename"] <- modis.files$filename[modis.files$year == tmp$survey_year & modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y]
              
            }
          
          )
        }
        
      }
      
      rm(tmp)
      
    }
    
    modis.classes <- list(modis_lctype1 = data.frame(class = c(1:17, 255), name = c("evergreen_needleleaf_forests", "evergreen_broadleaf_forests", "decidious_needleleaf_forests", "deciduous_broadleaf_forests", "mixed_forests", "closed_shrublands", "open_shrublands", "woody_savannas", "savannas", "grasslands", "permanent_wetlands", "croplands", "urban_builtup_lands", "cropland_natural_vegetation_mosaic", "permanent_snow_ice", "barren", "water_bodies", "unclassified")),
                          modis_lctype2 = data.frame(class = c(0:15, 255), name = c("water_bodies", "evergreen_needleleaf_forests", "evergreen_broadleaf_forests", "deciduous_needleleaf_forests", "deciduous_broadleaf_forests", "mixed_forests", "closed_shrublands", "open_shrublands", "woody_savannas", "savannas", "grasslands", "permanent_wetlands", "croplands", "urban_builtup_lands", "cropland_natural_vegetation_mosaic", "nonvegetated_lands", "unclassified")),
                          modis_lctype3 = data.frame(class = c(0:10, 255), name = c("water_bodies", "grasslands", "shrublands", "broadleaf_croplands", "savannas", "evergreen_broadleaf_forests", "deciduous_broadleaf_forests", "evergreen_needleleaf_forests", "deciduous_needleleaf_forests", "nonvegetated_lands", "urban_builtup_lands", "unclassified")),
                          modis_lctype4 = data.frame(class = c(0:8, 255), name = c("water_bodies", "evergreen_needleleaf_vegetation", "evergreen_broadleaf_vegetation", "deciduous_needleleaf_vegetation", "deciduous_broadleaf_vegetation", "annual_broadleaf_vegetation", "annual_grass_vegetation", "nonvegetated_lands", "urban_builtup_lands", "unclassified")),
                          modis_lctype5 = data.frame(class = c(0:11, 255), name = c("water_bodies", "evergreen_needleleaf_trees", "evergreen_broadleaf_trees", "deciduous_needleleaf_trees", "deciduous_broadleaf_trees", "shrub", "grass", "cereal_croplands", "broadleaf_croplands", "urban_builtup_lands", "permanent_snow_ice", "barren", "unclassified")))
    
    for(i in grep("modis_lc", covariates, value = T)) {
      
      index <- gsub("modis_lct", "LC_T", i)
      
      message(paste0("Calculating MODIS ", gsub("_", " ", index), "."))
      
      for(j in stats::na.omit(unique(modis.match$filename))) {
        
        pts_to_fill <- data[data$SiteCode %in% modis.match$SiteCode[modis.match$filename == j],]
        
        modis <- terra::rast(j)[index]
        
        for(k in unique(pts_to_fill$SiteCode)) {
          
          if(buffered == TRUE) {
            
            tmp <- data %>%
              dplyr::filter(SiteCode == k) %>%
              dplyr::select(SiteCode, geometry) %>%
              dplyr::distinct() %>%
              sf::st_transform(terra::crs(modis)) %>%
              terra::vect()
            
            modis_clip <- terra::crop(modis, tmp)
            
            modis_pland <- landscapemetrics::calculate_lsm(modis_clip, metric = "pland")
            
            for(l in modis_pland$class) {
              
              data[data$SiteCode == k & data$survey_year %in% modis.match$survey_year[modis.match$filename == j], paste0(index, "_", modis.classes[[i]]$name[modis.classes[[i]]$class == l])] <- modis_pland$value[modis_pland$class == l]
              
            }
            
            for(l in paste0(index, "_", modis.classes[[i]]$name[paste0(index, "_", modis.classes[[i]]$name) %in% names(data)])) {
              
              data[is.na(data[, l] %>% sf::st_drop_geometry()) & !(data$SiteCode %in% out_of_range), l] <- 0
              
            }
            
          } else {
            
            tmp <- data %>%
              filter(SiteCode == k) %>%
              select(SiteCode, geometry) %>%
              distinct() %>%
              sf::st_transform(terra::crs(modis))
            
            extr_table <- terra::extract(modis, tmp, fun = unique)[,index]
            
            if(class(extr_table) == "integer") {
              
              extr_table <- extr_table %>%
                as.data.frame()
              
              names(extr_table) <- "class"
              
              extr_table <- dplyr::left_join(extr_table, modis.classes[[i]], by = "class")
              
            } else {
              
              extr_table <- extr_table %>%
                as.data.frame() %>%
                dplyr::select(dplyr::all_of(index))
              
              names(extr_table) <- "class"
              
              extr_table <- dplyr::left_join(extr_table, modis.classes[[i]], by = "class")
              
            }
            
            tryCatch(data[data$SiteCode == k & data$survey_year %in% modis.match$survey_year[modis.match$filename == j], paste0(index, "_Class")] <- modis.classes[[i]]$name[modis.classes[[i]]$class == terra::extract(modis, tmp, fun = unique)[,index]],
                     warning = function(w) {
                       
                       if(conditionMessage(w) == "longer object length is not a multiple of shorter object length") {
                         
                         warning(paste0("MODIS ", index, ": Site ", k, " in year(s) ", stringr::str_flatten_comma(sort(unique(modis.match$survey_year[modis.match$filename == j]))), " touches multiple cells. nc_covariates returned `", suppressWarnings(modis.classes[[i]]$name[modis.classes[[i]]$class == terra::extract(modis, tmp, fun = unique)[,index]]), "` but possible values were `", stringr::str_flatten(extr_table$name, collapse = "`, `"), "`. Please examine to choose desired output and replace if necessary."),
                                 call. = FALSE)
                         
                       } else {
                         
                         warning(conditionMessage(w))
                         
                       }
                       
                     })
          }
        }
        
      }
      
      
      
      
    }
    
    if(retain == FALSE) {
      
      message(paste0("MODIS Land Cover extraction complete. Removing files."))
      
      file.remove(modis.files$filename)
      
    }
    
    rm(modis.files)
    
  }
  
  return(data)
  
}

######################### EXAMPLE LANDCOVER WORKFLOW ###########################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch

# Format and buffer data for extraction

landcover_data <- data_fmt(data)

landcover_data <- data_buff(landcover_data, buffer = TRUE)

# Enter EarthData Login info and download data

ed_email <- readline(prompt = "Enter EarthData email: ")

ed_pw <- readline(prompt = "Enter EarthData password: ")

modis_files <- landcover_download(landcover_data,
                                  covariates = "modis_lctype1",
                                  ed_email = ed_email,
                                  ed_password = ed_pw)

landcover_data <- landcover_extract(landcover_data,
                                    modis_files = modis_files,
                                    covariates = "modis_lctype1")

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[1] <- 60

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- landcover_extract(outofrange_data,
                                     modis_files = modis_files,
                                     covariates = "modis_lctype1")

# Tidy up

rm(ed_email, ed_pw, modis_files, landcover_data, outofrange_data)

########################### VEGETATION FUNCTIONS ###############################

vegetation_download <- function(data,
                                covariates = NULL,
                                ed_email = NULL,
                                ed_password = NULL,
                                site_name = NULL,
                                date_year = NULL,
                                date_month = NULL,
                                date_day = NULL,
                                dl_path = NULL) {
  
  
  if("modis_ndvi" %in% covariates | "modis_evi" %in% covariates) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "lubridate", "stringr", "sf", "terra", "luna", "exactextractr"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    if(is.null(ed_email) | is.null(ed_password)) {
      
      stop("MODIS data requested but Earthdata system login information not supplied. Please register at https://urs.earthdata.nasa.gov/users/new and supply using `ed_email` and `ed_password` parameters.",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name, date_year, date_month, date_day)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(!is.null(date_year)) {
      
      data <- dplyr::mutate(data, survey_year = !!rlang::sym(date_year))
      
    }
    
    data$survey_year <- as.numeric(data$survey_year)
    
    if(!is.null(date_month)) {
      
      data <- dplyr::mutate(data, survey_month = !!rlang::sym(date_month))
      
    }
    
    data$survey_month <- as.numeric(data$survey_month)
    
    if(!is.null(date_day)) {
      
      data <- dplyr::mutate(data, survey_day = !!rlang::sym(date_day))
      
    }
    
    data$survey_day <- as.numeric(data$survey_day)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_transform("ESRI:102001") %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      } else {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      }
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::project("ESRI:102001") %>%
          terra::buffer(20000)
        
      } else {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::buffer(20000)
        
      }
      
      data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
      
    }
    
    # Remove any observations missing year, month, or day data.
    
    if(TRUE %in% is.na(data$survey_year) | TRUE %in% is.na(data$survey_month) | TRUE %in% is.na(data$survey_day)) {
      
      warning("Missing date data detected. Complete year, month, and day data is needed for extraction. Observations missing date data will be dropped.",
              call. = FALSE)
      
      data <- data %>%
        dplyr::filter(!is.na(survey_year), !is.na(survey_month), !is.na(survey_day))
      
    }
    
    
    if(is.null(dl_path) & !dir.exists("./modis/MOD13A1")) {
      
      dir.create("./modis/MOD13A1", recursive = T)
      
    }
    
    if(!is.null(dl_path) & !dir.exists(paste0(dl_path, "/modis/MOD13A1"))) {
      
      dir.create(paste0(dl_path, "/modis/MOD13A1"), recursive = T)
      
    }
    
    for(i in c(FALSE, TRUE)) {
      
      modis.files <- list()
      
      tryCatch(
      
      modis.files[[as.character(min(data$survey_year))]] <- luna::getNASA(product = "MOD13A1",
                                                                          start = paste0(min(data$survey_year), "-", min(data$survey_month[data$survey_year == min(data$survey_year)]), "-01"),
                                                                          end = paste0(min(data$survey_year), "-", max(data$survey_month[data$survey_year == min(data$survey_year)]), ifelse(max(data$survey_month[data$survey_year == min(data$survey_year)]) %in% c(1, 3, 5, 7, 8, 10, 12), "-31", ifelse(max(data$survey_month[data$survey_year == min(data$survey_year)]) %in% c(4, 6, 9, 11), "-30", ifelse(lubridate::leap_year(min(data$survey_year)), "-29", "-28")))),
                                                                          aoi=project(study_area, "epsg:4326"),
                                                                          download=i,
                                                                          overwrite=FALSE,
                                                                          path=ifelse(is.null(dl_path), "./modis/MOD13A1", paste0(dl_path, "/modis/MCD12Q1")),
                                                                          username=ed_email,
                                                                          password=ed_password),
      warning = function(w) {
        
        if(conditionMessage(w) == "No results found") {
          
          if(!i) {
            
            warning(paste0("MODIS NDVI/EVI: no data found for year ", min(data$survey_year), ". Is it outside of the temporal coverage of this dataset?"),
                    call. = FALSE)
            
          }

        } else {
          
          warning(conditionMessage(w))
          
        }
        
      }
      )
      
      # Will need a way to generalize month formatting
      
      for(j in sort(unique(data$survey_year))[2:length(unique(data$survey_year))]) {
        
        tryCatch(
        modis.files[[as.character(j)]] <- luna::getNASA(product = "MOD13A1",
                                                        start = paste0(j, "-", min(data$survey_month[data$survey_year == j]), "-01"),
                                                        end = paste0(j, "-", max(data$survey_month[data$survey_year == j]), ifelse(max(data$survey_month[data$survey_year == j]) %in% c(1, 3, 5, 7, 8, 10, 12), "-31", ifelse(max(data$survey_month[data$survey_year == j]) %in% c(4, 6, 9, 11), "-30", ifelse(lubridate::leap_year(j) & max(data$survey_month[data$survey_year == j]) == 2, "-29", "-28")))),
                                                        aoi=project(study_area, "epsg:4326"),
                                                        download=i,
                                                        overwrite=FALSE,
                                                        path=ifelse(is.null(dl_path), "./modis/MOD13A1", paste0(dl_path, "/modis/MCD12Q1")),
                                                        username=ed_email,
                                                        password=ed_password),
        warning = function(w) {
          
          if(conditionMessage(w) == "No results found") {
            
            if(!i) {
              
              warning(paste0("MODIS NDVI/EVI: no data found for year ", j, ". Is it outside of the temporal coverage of this dataset?"),
                      call. = FALSE)
              
            }
            
          } else {
            
            warning(conditionMessage(w))
            
          }
          
          }
        
        )
        
      }
      
      modis.files <- unlist(modis.files, use.names = F)
      
      if(i == FALSE) {
        
        message(paste0("MODIS NDVI/EVI products are at a 16 day resolution, resulting in ", length(modis.files), " files to download for your data. This may take some time."))
        
      }
      
    }
    
    return(modis.files)
    
  }
  
}

vegetation_extract <- function(data,
                               covariates = NULL,
                               vegetation_files = NULL,
                               site_name = NULL,
                               date_year = NULL,
                               date_month = NULL,
                               date_day = NULL,
                               dl_path = NULL,
                               retain = TRUE) {
  
  
  if("modis_ndvi" %in% covariates | "modis_evi" %in% covariates) {
    
    orig_data <- data
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "lubridate", "stringr", "sf", "terra", "luna", "exactextractr"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    if(is.null(vegetation_files)) {
      
      stop("No vegetation files provided to extract from. Please provide a vector containing filepaths of all necessary MODIS files for your data. Data can be downloaded using landcover_download.",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name, date_year, date_month, date_day)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(!is.null(date_year)) {
      
      data <- dplyr::mutate(data, survey_year = !!rlang::sym(date_year))
      
    }
    
    data$survey_year <- as.numeric(data$survey_year)
    
    if(!is.null(date_month)) {
      
      data <- dplyr::mutate(data, survey_month = !!rlang::sym(date_month))
      
    }
    
    data$survey_month <- as.numeric(data$survey_month)
    
    if(!is.null(date_day)) {
      
      data <- dplyr::mutate(data, survey_day = !!rlang::sym(date_day))
      
    }
    
    data$survey_day <- as.numeric(data$survey_day)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
      
    }
    
    # Remove any observations missing year, month, or day data.
    
    if(TRUE %in% is.na(data$survey_year) | TRUE %in% is.na(data$survey_month) | TRUE %in% is.na(data$survey_day)) {
      
      warning("Missing date data detected. Complete year, month, and day data is needed for extraction. Observations missing date data will be dropped.",
              call. = FALSE)
      
      data <- data %>%
        dplyr::filter(!is.na(survey_year), !is.na(survey_month), !is.na(survey_day))
      
    }
    
    modis.files <- luna::modisDate(vegetation_files)
    
    modis.files$enddate <- modis.files$date + 16
    
    modis.files$year <- as.numeric(modis.files$year)
    modis.files$month <- as.numeric(modis.files$month)
    modis.files$day <- as.numeric(modis.files$day)
    
    modis.files$endyear <- lubridate::year(modis.files$enddate)
    modis.files$endmonth <- lubridate::month(modis.files$enddate)
    modis.files$endday <- lubridate::day(modis.files$enddate)
    
    modis.files$yday <- lubridate::yday(modis.files$date)
    modis.files$endyday <- lubridate::yday(modis.files$enddate)
    
    yearyearday <- function(yr, yd) {
      base <- as.Date(paste0(yr, "-01-01")) # take Jan 1 of year
      day <- base + yd - 1
    }
    
    modis.files$productiondate <- yearyearday(as.numeric(substr(modis.files$filename, 61-16, 61-13)), as.numeric(substr(modis.files$filename, 61-12, 61-10))) + lubridate::hms(paste0(substr(modis.files$filename, 61-9, 61-8), ":", substr(modis.files$filename, 61-7, 61-6), ":", substr(modis.files$filename, 61-5, 61-4))) # this may cause issues down the line. Reexamine?
    
    modis.files <- cbind(modis.files, as.data.frame(luna::modisExtent(modis.files$filename)))
    
    
    modis.match <- data %>%
      dplyr::mutate(date = as.Date(paste0(survey_year, "-", survey_month, "-", survey_day))) %>%
      dplyr::mutate(yday = lubridate::yday(date)) %>%
      dplyr::select(SiteCode, survey_year, yday, geometry) %>%
      sf::st_transform(terra::crs(terra::rast(modis.files$filename[1])))
    
    if(buffered == TRUE) {
      
      suppressWarnings(
        
        modis.match <- cbind(modis.match, sf::st_coordinates(sf::st_centroid(modis.match)))
        
      )
      
      
    } else {
      
      modis.match <- cbind(modis.match, sf::st_coordinates(modis.match))
      
    }
    
    warning_sites <- c()
    warning_years <- c()
    warning_dates <- c()
    
    for(i in unique(modis.match$SiteCode)) {
      
      for(j in unique(modis.match$survey_year[modis.match$SiteCode == i])) {
        
        for(k in unique(modis.match$yday[modis.match$SiteCode == i & modis.match$survey_year == j])) {
          
          tmp <- dplyr::filter(modis.match, SiteCode == i, survey_year == j, yday == k)

          if(nrow(modis.files[modis.files$year == tmp$survey_year & modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y & modis.files$yday <= tmp$yday & modis.files$endyday > tmp$yday,]) == 0) {
            
            spatial_check <- ifelse(nrow(modis.files[modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y,]) > 0, TRUE, FALSE)
            year_check <- ifelse(nrow(modis.files[modis.files$year == tmp$survey_year,]) > 0, TRUE, FALSE)
            yday_check <- ifelse(nrow(modis.files[modis.files$yday <= tmp$yday & modis.files$endyday > tmp$yday,]) > 0, TRUE, FALSE)
            
            if(!(spatial_check)) {
              
              warning_sites <- c(warning_sites, i)
              
            }
            
            if(!(year_check)) {
              
              warning_years <- c(warning_years, j)
              
            }
            
            if(spatial_check & year_check & !(yday_check)) {
              
              warning_dates <- c(warning_dates, yearyearday(j, k))
              
            }
                
          } else {
              
            suppressWarnings(
              
              {
                poss.files <- modis.files[modis.files$year == tmp$survey_year & modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y & modis.files$yday <= tmp$yday & modis.files$endyday > tmp$yday,]
              
                modis.match[modis.match$SiteCode == i & modis.match$survey_year == j & modis.match$yday == k, "filename"] <- poss.files$filename[poss.files$productiondate == max(poss.files$productiondate)]
              
                }
              
            )
            
            }
          
        }
        
      }
      
    }
    
    if(length(warning_sites) > 0) {
      
      if(length(warning_sites) == 1) {
        
        warning("Site ", stringr::str_flatten_comma(unique(warning_sites)), " falls outside of the spatial extent of the files provided. No value will be returned.",
                call. = FALSE)
      } else {
        
        warning("Sites ", stringr::str_flatten_comma(unique(warning_sites)), " fall outside of the spatial extent of the files provided. No value will be returned.",
                call. = FALSE)
        
      }
    
    }
    
    warning_sites <- sort(unique(warning_sites))
    warning_years <- sort(unique(warning_years))
    warning_dates <- sort(unique(warning_dates))
    
    if(length(warning_years) > 0) {
      
      if(length(warning_years) == 1) {
        
        warning("Observations from year ", stringr::str_flatten_comma(unique(warning_years)), " fall outside of the temporal extent of the files provided. Is it in a year where data is unavailable from this dataset? No value will be returned.",
                call. = FALSE)
        
      } else {
        
        warning("Observations from years ", stringr::str_flatten_comma(unique(warning_years)), " fall outside of the temporal extent of the files provided. Is it in a year where data is unavailable from this dataset? No value will be returned.",
                call. = FALSE)
        
      }
      
    }
    
    if(length(warning_dates) > 0) {

      warning("Observations on ", stringr::str_flatten_comma(unique(warning_dates)), " fall outside of the temporal extent of the files provided. You have provided data for this year but not this 16-day window. No value will be returned.",
                call. = FALSE)
      
    }
    
    modis.match <- dplyr::filter(modis.match, !is.na(filename))
    
    data$yday <- paste0(data$survey_year, "-", data$survey_month, "-", data$survey_day) %>%
      as.Date() %>%
      lubridate::yday()
    
    for(i in `if`("modis_ndvi" %in% covariates, `if`("modis_evi" %in% covariates, c("modis_ndvi", "modis_evi"), "modis_ndvi"), "modis_evi")) {
      
      message(paste0("Calculating MODIS ", ifelse(i == "modis_ndvi", "NDVI", "EVI"), "."))
      
      index <- ifelse(i == "modis_ndvi", "\"500m 16 days NDVI\"", "\"500m 16 days EVI\"")
      
      for(j in unique(modis.match$filename)) {
        
        pts_to_fill <- data[data$SiteCode %in% modis.match$SiteCode[modis.match$filename == j] & data$survey_year %in% modis.match$survey_year[modis.match$filename == j] & data$yday %in% modis.match$yday[modis.match$filename == j],]
        
        modis <- terra::rast(j)[index]
        
        for(k in unique(pts_to_fill$SiteCode)) {
          
          if(buffered == TRUE) {
            
            tmp <- data %>%
              dplyr::filter(SiteCode == k, survey_year %in% modis.match$survey_year[modis.match$filename == j]) %>%
              dplyr::select(SiteCode, geometry) %>%
              dplyr::distinct() %>%
              sf::st_transform(terra::crs(modis))
            
            modis_clip <- terra::crop(modis, tmp)
            
            data[data$SiteCode == k & data$survey_year == modis.match$survey_year[modis.match$filename == j & modis.match$SiteCode == k] & data$yday %in% modis.match$yday[modis.match$filename == j & modis.match$SiteCode == k], ifelse(i == "modis_ndvi", "ndvi", "evi")] <- exactextractr::exact_extract(modis_clip, tmp, fun = "mean")
            
          } else {
            
            tmp <- data %>%
              dplyr::filter(SiteCode == k, survey_year %in% modis.match$survey_year[modis.match$filename == j]) %>%
              dplyr::select(SiteCode, geometry) %>%
              dplyr::distinct() %>%
              sf::st_transform(terra::crs(modis)) %>%
              terra::vect()
            
            data[data$SiteCode == k & data$survey_year == modis.match$survey_year[modis.match$filename == j & modis.match$SiteCode == k] & data$yday %in% modis.match$yday[modis.match$filename == j & modis.match$SiteCode == k], ifelse(i == "modis_ndvi", "ndvi", "evi")] <- terra::extract(modis, tmp, fun = "mean")[,index]
            
          }
        }
        
      }
      
    }
    
    data <- dplyr::select(data, -yday)
      
    if(retain == FALSE) {
      
      message(paste0("MODIS NDVI/EVI extraction complete. Removing files."))
      
      file.remove(modis.files$filename)
      
    }
    
    rm(modis.files) 

    
  }
  
  return(data)
  
}

######################## EXAMPLE VEGETATION WORKFLOW ###########################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020))

# Format and buffer data for extraction

vegetation_data <- data_fmt(data)

vegetation_data <- data_buff(vegetation_data, buffer = TRUE)

# Enter EarthData Login info and download data

ed_email <- readline(prompt = "Enter EarthData email: ")

ed_pw <- readline(prompt = "Enter EarthData password: ")

vegetation_files <- vegetation_download(vegetation_data,
                                        covariates = "modis_evi",
                                        ed_email = ed_email,
                                        ed_password = ed_pw)

vegetation_data <- vegetation_extract(vegetation_data,
                                      vegetation_files = vegetation_files,
                                      covariates = "modis_evi")

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[2] <- 60

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- vegetation_extract(outofrange_data,
                                      vegetation_files = vegetation_files,
                                      covariates = "modis_evi")

# Tidy up

rm(ed_email, ed_pw, vegetation_files, vegetation_data, outofrange_data)

############################ ELEVATION FUNCTIONS ###############################

elevation_download <- function(data,
                               covariates = NULL,
                               site_name = NULL,
                               z = 7,
                               src = "aws") {
  
  
  if("elevation" %in% covariates) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "stringr", "sf", "terra", "elevatr", "exactextractr"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      data <- sf::st_as_sf(data)
      
    }
    
    elev <- elevatr::get_elev_raster(locations = data,
                                     z = z,
                                     prj = sf::st_crs(data),
                                     src = src, # In future, check other sources. Others more appropriate for CDN users?
                                     neg_to_na = TRUE, # Turn ocean tiles with negative elevation to NAs.
                                     expand = 20000,
                                     verbose = F) %>%
      terra::rast()
   
    
    return(elev)
    
  }
  
}

elevation_extract <- function(data,
                              elevation_data = NULL,
                              covariates = NULL,
                              site_name = NULL) {
  if("elevation" %in% covariates) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "stringr", "sf", "terra", "elevatr", "exactextractr"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      data <- sf::st_as_sf(data)
      
    }
    
    elev <- elevation_data
    
    for(i in unique(data$SiteCode)) {
      
      tmp <- data %>%
        dplyr::filter(SiteCode == i) %>%
        dplyr::select(SiteCode, geometry) %>%
        dplyr::distinct()
      
      if(!terra::is.related(elev, terra::vect(tmp), relation = "intersects")) {
        
        warning("Site ", i, " falls outside of the spatial extent of the elevation rasters provided. No value will be returned.",
                call. = FALSE)
        
      } else if(terra::is.related(elev, terra::vect(tmp), relation = "intersects") & !terra::is.related(elev, terra::vect(tmp), relation = "contains")) {
        
        warning("Site ", i, "'s buffered area is only partially contained by the spatial extent of the elevation rasters provided. Returned mean elevation value will be derived from the available values.",
                call. = FALSE)
        
      } else {
        
        if(buffered == FALSE) {
          
          data[data$SiteCode == i, "elevation"] <- terra::extract(x = elev,
                                                                  y = tmp,
                                                                  fun = "mean")[,names(elev)]
          
          
        } else {
          
          data[data$SiteCode == i, "elevation"] <- exactextractr::exact_extract(x = elev,
                                                                                y = tmp,
                                                                                fun = "mean",
                                                                                progress = FALSE)
          
        }
        
      }
      
    }
    
    if(TRUE %in% is.na(data$elevation)) {
      
      warning("AWS Elevation: some points are close to shore, and so fall into cells with negative elevation (below sea level). For these cells, the nearest positive elevation has been used.",
              call. = FALSE)
      
      for(i in unique(data$SiteCode[is.na(data$elevation)])) {
        
        tmp <- data %>%
          dplyr::filter(SiteCode == i) %>%
          dplyr::select(SiteCode, geometry) %>%
          dplyr::distinct() %>%
          sf::st_buffer(2500)
        
        if(terra::is.related(elev, terra::vect(tmp), relation = "intersects")) {
          
          elev_crop <- terra::crop(elev, vect(tmp)) %>%
            terra::as.points()
          
          data$elevation[data$SiteCode == i] <- terra::values(elev_crop[terra::nearest(terra::vect(tmp), elev_crop)$to_id])
          
        }

      }
      
    }
    
    
  }
  
  return(data)
  
}

######################## EXAMPLE ELEVATION WORKFLOW ############################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020))

# Format and buffer data for extraction

elevation_data <- data_fmt(data)

elevation_data <- data_buff(elevation_data, buffer = FALSE)

elevation_rasts <- elevation_download(elevation_data,
                                      covariates = "elevation")

elevation_data <- elevation_extract(elevation_data,
                                    elevation_data = elevation_rasts,
                                    covariates = "elevation")

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[2] <- 60

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- elevation_extract(outofrange_data,
                                     elevation_data = elevation_rasts,
                                     covariates = "elevation")

# Tidy up

rm(data, elevation_rasts, elevation_data, outofrange_data)

########################### WORLDCLIM FUNCTIONS ################################

worldclim_download <- function(data,
                               covariates = NULL,
                               site_name = NULL,
                               countries = "Canada",
                               res = 0.5,
                               dl_path = NULL) {
  
  
  if(length(grep("worldclim_", covariates)) > 0) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "stringr", "sf", "terra", "geodata", "exactextractr"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
      
    }
    
    if(is.null(dl_path) & !dir.exists("./worldclim")) {
      
      dir.create("./worldclim", recursive = T)
      
    }
    
    if(!is.null(dl_path) & !dir.exists(paste0(dl_path, "/worldclim"))) {
      
      dir.create(paste0(dl_path, "/worldclim"), recursive = T)
      
    }
    
    clim_vars <- gsub(pattern = "worldclim_", replacement = "", grep("worldclim_", covariates, value = T))
    
    clim <- list()
    
    #### NEED TO CHECK THAT MONTH DATA IS VALID
    
    for(i in clim_vars) {
      
      for(j in countries) {
        
        country_code <- geodata::country_codes()
        
        country_code <- country_code$ISO3[country_code$NAME == j]
        
        if(!file.exists(ifelse(is.null(dl_path), paste0("./worldclim/climate/wc2.1_country/", country_code, "_wc2.1_30s_", i, ".tif"), paste0(dl_path, "/worldclim/climate/wc2.1_country/", country_code, "_wc2.1_30s_", i, ".tif")))) {
          
          clim[[i]][[j]] <- geodata::worldclim_country(var = i, 
                                                  country = j, ### ADD WAY TO INCORPORATE OTHER COUNTRIES?
                                                  res = res,
                                                  path = ifelse(is.null(dl_path), "./worldclim", paste0(dl_path, "/worldclim")))
          
        } else {
          
          clim[[i]][[j]] <- terra::rast(ifelse(is.null(dl_path), paste0("./worldclim/climate/wc2.1_country/", country_code, "_wc2.1_30s_", i, ".tif"), paste0(dl_path, "/worldclim/climate/wc2.1_country/", country_code, "_wc2.1_30s_", i, ".tif")))
          
        }
        
      }
      
      clim[[i]] <- terra::sprc(clim[[i]])
      
      clim[[i]] <- terra::merge(clim[[i]])
      
    }
   
    return(clim)
     
  }
  
}

worldclim_extract <- function(data,
                              worldclim_data = NULL,
                              covariates = NULL,
                              site_name = NULL,
                              retain = TRUE) {
  
  if(length(grep("worldclim_", covariates)) > 0) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "stringr", "sf", "terra", "geodata", "exactextractr"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs(clim[[i]]))) {
        
        study_area <- data[]
        
      }
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_transform("ESRI:102001") %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      } else {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      }
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::project("ESRI:102001") %>%
          terra::buffer(20000)
        
      } else {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::buffer(20000)
        
      }
      
      data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
      
    }
    
    clim <- worldclim_data
    
    for(i in names(worldclim_data)) {
      
      for(j in unique(data$SiteCode)) {
        
        tmp <- data %>%
          dplyr::filter(SiteCode == j) %>%
          dplyr::select(SiteCode, survey_month, geometry) %>%
          dplyr::distinct() %>%
          sf::st_transform(terra::crs(clim[[i]]))
        
        for(k in unique(data$survey_month[data$SiteCode == j])) {
          
          layername <- paste0(substr(names(clim[[i]])[1], start = 1, stop = nchar(names(clim[[i]])[1])-1), k)
          
          if(which(unique(data$survey_month[data$SiteCode == j]) == k) == 1) {
            
            if(!terra::is.related(clim[[i]], terra::vect(tmp), relation = "intersects")) {
              
              warning("WorldClim [", i, "]: site ", j, " falls outside of the spatial extent of the WorldClim rasters provided. No value will be returned.",
                      call. = FALSE)
              
            } else if(terra::is.related(clim[[i]], terra::vect(tmp), relation = "intersects") & !terra::is.related(clim[[i]], terra::vect(tmp), relation = "contains")) {
              
              warning("WorldClim [", i, "]: site ", j, "'s buffered area is only partially contained by the spatial extent of the WorldClim rasters provided. Returned mean ", i, " value will be derived from the available values.",
                      call. = FALSE)
              
            } else {
              
              if(buffered == TRUE) {
                
                data[data$SiteCode == j & data$survey_month == k, i] <- exactextractr::exact_extract(x = clim[[i]][[layername]], 
                                                                                                     y = tmp %>% dplyr::filter(survey_month == k), 
                                                                                                     fun = "mean")
                
              } else {
                
                data[data$SiteCode == j & data$survey_month == k, i] <- terra::extract(x = clim[[i]][[layername]], 
                                                                                       y = tmp %>% dplyr::filter(survey_month == k), 
                                                                                       fun = "mean",
                                                                                       na.rm = TRUE)[,layername]
                
              }
            }
            
          } else {
            
            if(terra::is.related(clim[[i]], terra::vect(tmp), relation = "intersects")) {
              
              if(buffered == TRUE) {
                
                data[data$SiteCode == j & data$survey_month == k, i] <- exactextractr::exact_extract(x = clim[[i]][[layername]], 
                                                                                                     y = tmp %>% dplyr::filter(survey_month == k), 
                                                                                                     fun = "mean")
                
              } else {
                
                data[data$SiteCode == j & data$survey_month == k, i] <- terra::extract(x = clim[[i]][[layername]], 
                                                                                       y = tmp %>% dplyr::filter(survey_month == k), 
                                                                                       fun = "mean",
                                                                                       na.rm = TRUE)[,layername]
                
              }
            }
            
          }
          
        }
        
      }
      
      if(TRUE %in% is.na(data[,i])) {

        for(j in unique(data$SiteCode[is.na(data[,i])])) {
          
          for(k in unique(data$survey_month[data$SiteCode == j])) {
            
            layername <- paste0(substr(names(clim[[i]])[1], start = 1, stop = nchar(names(clim[[i]])[1])-1), k)
            
            tmp <- data %>%
              dplyr::filter(SiteCode == j) %>%
              dplyr::select(SiteCode, survey_month, geometry) %>%
              dplyr::distinct() %>%
              sf::st_transform(terra::crs(clim[[i]]))
            
            if(terra::is.related(clim[[i]], terra::vect(tmp), relation = "intersects")) {
                
              if(which(unique(data$SiteCode[is.na(data$SiteCode)]) == j) == 1) {
                
                warning(paste0("WorldClim [", i, "]: some points are close to shore, and so fall outside of raster coverage. For these cells, the nearest cell value has been used."),
                        call. = FALSE)
                
              }
              
                tmp <- data %>%
                  dplyr::filter(SiteCode == j, survey_month == k) %>%
                  dplyr::select(SiteCode, survey_month, geometry) %>%
                  dplyr::distinct() %>%
                  sf::st_buffer(2500) %>%
                  sf::st_transform(terra::crs(clim[[i]]))
                
                clim_crop <- terra::crop(clim[[i]][[layername]], terra::vect(tmp)) %>%
                  terra::as.points()
                
                data[data$SiteCode == j & data$survey_month == k, i] <- terra::values(clim_crop[terra::nearest(terra::vect(tmp), clim_crop)$to_id])
                
              }
          
            }
            
          }
          
        }
        
      }
    
    if(retain == FALSE) {
      
      message(paste0("WorldClim extraction complete. Removing files."))
      
      file.remove(list.files(ifelse(is.null(dl_path), "./worldclim", paste0(dl_path, "/worldclim")), full.names = T))
      
    }
  
  return(data)
  
  }
}

######################## EXAMPLE WORLDCLIM WORKFLOW ############################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020))

# Format and buffer data for extraction

worldclim_data <- data_fmt(data)

worldclim_data <- data_buff(worldclim_data, buffer = FALSE)

worldclim_rasts <- worldclim_download(worldclim_data,
                                      covariates = "worldclim_prec",
                                      countries = c("Canada"))

worldclim_data <- worldclim_extract(worldclim_data,
                                    worldclim_data = worldclim_rasts,
                                    covariates = "worldclim_prec")

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[2] <- 84

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- worldclim_extract(outofrange_data,
                                     worldclim_data = worldclim_rasts,
                                     covariates = "worldclim_prec")

# Tidy up

rm(data, worldclim_rasts, worldclim_data, outofrange_data)

############################# SCANFI FUNCTIONS #################################

scanfi_extract <- function(data,
                           covariates = NULL,
                           site_name = NULL,
                           dl_path = NULL,
                           retain = TRUE) {
  
  if(length(grep("scanfi_", covariates)) > 0) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "stringr", "sf", "terra", "landscapemetrics", "exactextractr"))
    
    if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
      
      stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_transform("ESRI:102001") %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      } else {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      }
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::project("ESRI:102001") %>%
          terra::buffer(20000)
        
      } else {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::buffer(20000)
        
      }
      
      data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
      
    }

    if(is.null(dl_path) & !dir.exists("./scanfi")) {
      
      dir.create("./scanfi", recursive = T)
      
    }
    
    if(!is.null(dl_path) & !dir.exists(paste0(dl_path, "/scanfi"))) {
      
      dir.create(paste0(dl_path, "/scanfi"), recursive = T)
      
    }
    
    scanfi_vars <- gsub(pattern = "scanfi_", replacement = "", grep("scanfi_", covariates, value = T))
    
    filename <- data.frame(variable = scanfi_vars) %>%
      dplyr::mutate(filename = case_when(variable == "biomass" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_biomass_SW_2020_v1.2.tif",
                                         variable == "closure" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_closure_SW_2020_v1.2.tif",
                                         variable == "height" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_height_SW_2020_v1.2.tif",
                                         variable == "nfilc" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif",
                                         variable == "balsamfir" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_balsamFir_SW_2020_v1.2.tif",
                                         variable == "blackspruce" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_blackSpruce_SW_2020_v1.2.tif",
                                         variable == "douglasfir" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_douglasFir_SW_2020_v1.2.tif",
                                         variable == "jackpine" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_jackPine_SW_2020_v1.2.tif",
                                         variable == "lodgepolepine" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_lodgepolePine_SW_2020_v1.2.tif",
                                         variable == "ponderosapine" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_ponderosaPine_SW_2020_v1.2.tif",
                                         variable == "tamarack" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_tamarack_SW_2020_v1.2.tif",
                                         variable == "whiteredpine" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_whiteRedPine_SW_2020_v1.2.tif",
                                         variable == "broadleaf" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_prcB_SW_2020_v1.2.tif",
                                         variable == "otherconifer" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_prcC_other_SW_2020_v1.2.tif"))
    
    ### SOMETHING TO NOTE: THE SPECIES LEVEL COVERS ARE COVER OF TOTAL CANOPY COVER, SO COVER OF A SP IN A CELL IS SPECIES LEVEL COVER * CANOPY COVER. MESSAGE ABOUT THIS OR BUILD IN?
    
    for(i in scanfi_vars) {
      
      ### WILL NEED TO CHECK IF DATA IS IN ARCTIC RANGE AND WARN.
      ### OUT OF COVERAGE WARNING?
      
      if(!file.exists(ifelse(is.null(dl_path), paste0("./scanfi/", dplyr::last(unlist(stringr::str_split(filename$filename[filename$variable == i], "/")))), paste0(dl_path, "/scanfi/", dplyr::last(unlist(stringr::str_split(filename$filename[filename$variable == i], "/"))))))) {
        
        message("SCANFI files are large, and require a fair bit of download and processing time.")
        
        ### USING METHODS OTHER THAN CURL SEEMS TO CAUSE ISSUES WITH DOWNLOADED FILE - NEED TO CONSIDER CURL COMPATIBILITY WITH OTHER OS'S.
        
        download.file(url = filename$filename[filename$variable == i], destfile = ifelse(is.null(dl_path), paste0("./scanfi/", dplyr::last(unlist(stringr::str_split(filename$filename[filename$variable == i], "/")))), paste0(dl_path, "/scanfi/", dplyr::last(unlist(stringr::str_split(filename$filename[filename$variable == i], "/"))))), method = "curl")
        
        scanfi <- terra::rast(ifelse(is.null(dl_path), paste0("./scanfi/", dplyr::last(unlist(stringr::str_split(filename$filename[filename$variable == i], "/")))), paste0(dl_path, "/scanfi/", dplyr::last(unlist(stringr::str_split(filename$filename[filename$variable == i], "/"))))))
        
        scanfi <- terra::crop(scanfi, terra::project(study_area, terra::crs(scanfi)))
        
      } else {
        
        scanfi <- terra::rast(ifelse(is.null(dl_path), paste0("./scanfi/", dplyr::last(unlist(stringr::str_split(filename$filename[filename$variable == i], "/")))), paste0(dl_path, "/scanfi/", dplyr::last(unlist(stringr::str_split(filename$filename[filename$variable == i], "/"))))))
        
        scanfi <- terra::crop(scanfi, terra::project(study_area, terra::crs(scanfi)))
        
      }
      
      if(i == "nfilc") {
        
        nfilc.classes <- data.frame(class = c(1:8), name = c("bryoid", "herbs", "rock", "shrub", "treed_broadleaf", "treed_conifer", "treed_mixed", "water"))
        
        for(j in unique(data$SiteCode)) {
          
          if(buffered == TRUE) {
            
            tmp <- data %>%
              dplyr::filter(SiteCode == j) %>%
              dplyr::select(SiteCode, geometry) %>%
              dplyr::distinct() %>%
              sf::st_transform(terra::crs(scanfi)) %>%
              terra::vect()
            
            scanfi_clip <- terra::crop(scanfi, tmp)
            
            scanfi_pland <- landscapemetrics::calculate_lsm(scanfi_clip, metric = "pland")
            
            for(k in scanfi_pland$class) {
              
              data[data$SiteCode == j, paste0("nfilc_", nfilc.classes$name[nfilc.classes$class == k])] <- scanfi_pland$value[scanfi_pland$class == k]
              
            }
            
            for(k in paste0("nfilc_", nfilc.classes$name[paste0("nfilc_", nfilc.classes$name) %in% names(data)])) {
              
              data[is.na(data[,k] %>% sf::st_drop_geometry()), k] <- 0
              
            }
            
          } else {
            
            tmp <- data %>%
              dplyr::filter(SiteCode == j) %>%
              dplyr::select(SiteCode, geometry) %>%
              dplyr::distinct() %>%
              sf::st_transform(terra::crs(scanfi))
            
            extr_table <- terra::extract(scanfi, tmp, fun = unique)[,"SCANFI_att_nfiLandCover_SW_2020_v1.2"]
            
            if(class(extr_table) == "integer") {
              
              extr_table <- extr_table %>%
                as.data.frame()
              
              names(extr_table) <- "class"
              
              extr_table <- dplyr::left_join(extr_table, nfilc.classes, by = "class")
              
            } else {
              
              extr_table <- extr_table %>%
                as.data.frame() %>%
                dplyr::select(SCANFI_att_nfiLandCover_SW_2020_v1.2)
              
              names(extr_table) <- "class"
              
              extr_table <- dplyr::left_join(extr_table, nfilc.classes, by = "class")
              
            }
            
            tryCatch(data[data$SiteCode == j, "nfilc_class"] <- nfilc.classes$name[nfilc.classes$class == terra::extract(scanfi, tmp, fun = unique)[,"SCANFI_att_nfiLandCover_SW_2020_v1.2"]],
                     warning = function(w) {
                       
                       if(conditionMessage(w) == "longer object length is not a multiple of shorter object length") {
                         
                         warning(paste0("SCANFI [NFI Land Cover]: Site ", j, " touches multiple cells. nc_covariates returned `", suppressWarnings(nfilc.classes$name[nfilc.classes$class == terra::extract(scanfi, tmp, fun = unique)[,"SCANFI_att_nfiLandCover_SW_2020_v1.2"]]), "` but possible values were `", stringr::str_flatten(extr_table$name, collapse = "`, `"), "`. Please examine to choose desired output and replace if necessary.",
                                        call. = FALSE))
                         
                       } else {
                         
                         warning(conditionMessage(w),
                                 call. = FALSE)
                         
                       }
                       
                     })
          }
        }
        
      } else {
        
        for(j in unique(data$SiteCode)) {
          
          tmp <- data %>%
            dplyr::filter(SiteCode == j) %>%
            dplyr::select(SiteCode, geometry) %>%
            dplyr::distinct() %>%
            sf::st_transform(sf::st_crs(scanfi))
          
          if(buffered == TRUE) {
            
            data[data$SiteCode == j, paste0("scanfi_", i)] <- exactextractr::exact_extract(x = scanfi, 
                                                                                           y = tmp, 
                                                                                           fun = "mean")
            
          } else {
            
            data[data$SiteCode == j, paste0("scanfi_", i)] <- terra::extract(x = scanfi, 
                                                                             y = tmp, 
                                                                             fun = "mean", na.rm = TRUE)[,2]
            
          }
          
          
        }
        
      }        
      
    }
    
    if(retain == FALSE) {
      
      message(paste0("SCANFI extraction complete. Removing files."))
      
      file.remove(list.files(ifelse(is.null(dl_path), "./scanfi", paste0(dl_path, "/scanfi")), full.names = T))
      
    }
    
  }
  
  return(data)
  
}

daymet_extract <- function(data,
                           covariates = NULL,
                           site_name = NULL,
                           date_year = NULL,
                           date_month = NULL,
                           date_day = NULL,
                           ed_username = NULL,
                           ed_password = NULL,
                           daymet_transfer = FALSE,
                           dl_path = NULL,
                           retain = TRUE) {
  
  if(length(grep("daymet_", covariates)) > 0) {
    
    # Check packages
    
    have_pkg_check(c("dplyr", "rlang", "lubridate", "stringr", "sf", "terra", "appeears", "readr", "exactextractr"))
    
    if(is.null(ed_username) | is.null(ed_password)) {
      
      stop("MODIS data requested but Earthdata system login information not supplied. NOTE: downloading DAYMET data requires your EarthData username, not email. Please register at https://urs.earthdata.nasa.gov/users/new and supply using `ed_username` and `ed_password` parameters.",
           call. = FALSE)
      
    }
    
    input_fmt <- covariate_fmt_check(data)
    
    if(input_fmt$type == "data.frame") {
      
      stop("Extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
           call. = FALSE)
      
    }
    
    # Check that all specified column names are present in the data
    
    specified_cols <- c(site_name, date_year, date_month, date_day)
    
    specified_cols <- specified_cols[!is.null(specified_cols)]
    
    data_cols <- names(data)
    
    if(!(all(specified_cols %in% data_cols))) {
      
      stop("Some specified columns missing from the data: ", stringr::str_flatten_comma(specified_cols[!(specified_cols %in% data_cols)]),". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
           call. = FALSE)
      
    }
    
    if(!is.null(site_name)) {
      
      data <- dplyr::mutate(data, SiteCode = !!rlang::sym(site_name))
      
    }
    
    data$SiteCode <- as.character(data$SiteCode)
    
    if(!is.null(date_year)) {
      
      data <- dplyr::mutate(data, survey_year = !!rlang::sym(date_year))
      
    }
    
    data$survey_year <- as.numeric(data$survey_year)
    
    if(!is.null(date_month)) {
      
      data <- dplyr::mutate(data, survey_month = !!rlang::sym(date_month))
      
    }
    
    data$survey_month <- as.numeric(data$survey_month)
    
    if(!is.null(date_day)) {
      
      data <- dplyr::mutate(data, survey_day = !!rlang::sym(date_day))
      
    }
    
    data$survey_day <- as.numeric(data$survey_day)
    
    if(input_fmt$type == "sf") {
      
      buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_transform("ESRI:102001") %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      } else {
        
        study_area <- sf::st_bbox(data) %>%
          sf::st_as_sfc() %>%
          sf::st_buffer(20000) %>% # might have to fiddle with this for extreme edge cases where someone buffers points by a huge distance
          terra::vect()
        
      }
      
    }
    
    if(input_fmt$type == "terra") {
      
      buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)
      
      orig_crs <- terra::crs(data)
      
      if(!(orig_crs == terra::crs("ESRI:102001"))) {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::project("ESRI:102001") %>%
          terra::buffer(20000)
        
      } else {
        
        study_area <- terra::ext(data) %>%
          terra::vect(crs = orig_crs) %>%
          terra::buffer(20000)
        
      }
      
      data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
      
    }
    
    if(is.null(dl_path) & !dir.exists("./daymet")) {
      
      dir.create("./daymet", recursive = T)
      
    }
    
    if(!is.null(dl_path) & !dir.exists(paste0(dl_path, "/daymet"))) {
      
      dir.create(paste0(dl_path, "/daymet"), recursive = T)
      
    }
    
    options(keyring_backend = "file")
    
    appeears::rs_set_key(user = ed_username, password = ed_password)
    
    token <- appeears::rs_login(user = ed_username)
    
    daymet.vars <- gsub(pattern = "daymet_", replacement = "", grep("daymet_", covariates, value = T))
    
    if(daymet_transfer == FALSE) {
      
      tasks <- list()
      
      for(i in sort(unique(data$survey_year))) {
        
        tasks[[as.character(i)]] <- data.frame(
          task = "polygon",
          subtask = "subtask",
          latitude = mean(sf::st_coordinates(data %>% sf::st_transform(4326))[,"Y"]),
          longitude = mean(sf::st_coordinates(data %>% sf::st_transform(4326))[,"X"]),
          start = paste0(i, "-", ifelse(nchar(min(data$survey_month[data$survey_year == i])) == 1, paste0(0, min(data$survey_month[data$survey_year == i])), min(data$survey_month[data$survey_year == i])), "-", ifelse(nchar(min(data$survey_day[data$survey_month == min(data$survey_month[data$survey_year == i]) & data$survey_year == i])) == 1, paste0(0, min(data$survey_day[data$survey_month == min(data$survey_month[data$survey_year == i]) & data$survey_year == i])), min(data$survey_day[data$survey_month == min(data$survey_month[data$survey_year == i]) & data$survey_year == i]))),
          end = paste0(i, "-", ifelse(nchar(max(data$survey_month[data$survey_year == i])) == 1, paste0(0, max(data$survey_month[data$survey_year == i])), max(data$survey_month[data$survey_year == i])), "-", ifelse(nchar(max(data$survey_day[data$survey_month == max(data$survey_month[data$survey_year == i]) & data$survey_year == i])) == 1, paste0(0, max(data$survey_day[data$survey_month == max(data$survey_month[data$survey_year == i]) & data$survey_year == i])), max(data$survey_day[data$survey_month == max(data$survey_month[data$survey_year == i]) & data$survey_year == i]))),
          product = "DAYMET.004",
          layer = daymet.vars
        )
        
      }
      
      for(i in sort(unique(data$survey_year))) {
        
        task <- appeears::rs_build_task(df = tasks[[as.character(i)]],
                                        roi = sf::st_as_sf(study_area),
                                        format = "geotiff")
        
        appeears::rs_request(
          request = task,
          user = ed_username,
          transfer = FALSE,
          verbose = TRUE
        )
        
      }
      
      task_ids <- list()
      
      tasklist <- appeears::rs_list_task(user = ed_username)
      
      for(i in sort(unique(data$survey_year), decreasing = TRUE)) {
        
        task_ids[[as.character(i)]] <- tasklist[which(sort(unique(data$survey_year), decreasing = TRUE) == i), "task_id"]
        
      }
      
      saveRDS(task_ids, file = ifelse(is.null(dl_path), "./daymet/daymet_reqs.RDS", paste0(dl_path, "/daymet/daymet_reqs.RDS")))
      
      assign("daymet_reqs", task_ids, envir = .GlobalEnv)
      
      appeears::rs_logout(token)
      
      message(cat("Requests have been placed with appeears for the data you've requested. Look to your email for confirmation that these have been completed. We have saved the request data in an external object at ", ifelse(is.null(dl_path), "./daymet/daymet_reqs.RDS", paste0(dl_path, "/daymet/daymet_reqs.RDS")), ". Please rerun your call to nc_covariates with parameter 'daymet_transfer' set to TRUE once you have received confirmation that these requests are approved at your EarthData email."))
      
      return(data)
      
    } else {
      
      if(file.exists(ifelse(is.null(dl_path), "./daymet/daymet_reqs.RDS", paste0(dl_path, "/daymet/daymet_reqs.RDS")))) {
        
        appeears <- readRDS(ifelse(is.null(dl_path), "./daymet/daymet_reqs.RDS", paste0(dl_path, "/daymet/daymet_reqs.RDS")))
        
      } else {
        
        stop(paste0("Cannot find file daymet_req.RDS at ", ifelse(is.null(dl_path), "./daymet/daymet_reqs.RDS", paste0(dl_path, "/daymet/daymet_reqs.RDS")), ". Have you submitted an initial request with appeears_transfer = FALSE? Have you moved the file?"))
        
      }
      
      
      for(i in sort(unique(data$survey_year))) {
        
        if(!dir.exists(ifelse(is.null(dl_path), paste0("./daymet/", appeears[[as.character(i)]]), paste0(dl_path, "/daymet/", appeears[[as.character(i)]])))) {
          
          dir.create(ifelse(is.null(dl_path), paste0("./daymet/", appeears[[as.character(i)]]), paste0(dl_path, "/daymet/", appeears[[as.character(i)]])))
          
          message(paste0("Downloading Daymet data for ", i))
          
          appeears::rs_transfer(task_id = appeears[[as.character(i)]], 
                                user = ed_username,
                                path = ifelse(is.null(dl_path), paste0("./daymet/", appeears[[as.character(i)]]), paste0(dl_path, "/daymet/", appeears[[as.character(i)]])))
          
          message(paste0("Daymet data for ", i, " downloaded."))
          
        }
      }
      
      daymet.stats <- list()
      
      all.dates <- c()
      
      for(i in sort(unique(data$survey_year))) {
        
        if(file.exists(ifelse(is.null(dl_path), paste0("./daymet/", appeears[[as.character(i)]], "/DAYMET-004-Statistics.csv"), paste0(dl_path, "/daymet/", appeears[[as.character(i)]], "/DAYMET-004-Statistics.csv")))) {
          
          daymet.stats[[as.character(i)]] <- readr::read_csv(ifelse(is.null(dl_path), paste0("./daymet/", appeears[[as.character(i)]], "/DAYMET-004-Statistics.csv"), paste0(dl_path, "/daymet/", appeears[[as.character(i)]], "/DAYMET-004-Statistics.csv")))
          
          all.dates <- c(all.dates, unique(daymet.stats[[as.character(i)]]$Date))
          
        }
        
      }
      
      all.dates <- as.Date(all.dates)
      
      data$date <- as.Date(paste0(data$survey_year, "-", data$survey_month, "-", data$survey_day))
      
      if(TRUE %in% unique(unique(data$date) > max(all.dates))) {
        
        latest.date <- max(data$date[data$date %in% all.dates])
        
        message(paste0("Daymet data is not available for some dates. The latest date in your data with available Daymet data is ", max(data$date[data$date %in% all.dates]), ". NA will be returned for dates beyond this point."))
        
      }
      
      dates <- sort(unique(data$date[data$date %in% all.dates]))
      
      for(i in daymet.vars) {
        
        for(j in dates) {
          
          pts_to_fill <- dplyr::filter(data, date == j)
          
          j.date <- as.Date(j)
          
          filename <- gsub(pattern = "DAYMET_", replacement = "DAYMET.", daymet.stats[[as.character(lubridate::year(j.date))]]$`File Name`[daymet.stats[[as.character(lubridate::year(j.date))]]$Date == j.date & daymet.stats[[as.character(lubridate::year(j.date))]]$Dataset == i])
          
          daymet <- terra::rast(ifelse(is.null(dl_path), paste0("./daymet/", appeears[[as.character(lubridate::year(j.date))]], "/", filename, ".tif"), paste0(dl_path, "/daymet/", appeears[[as.character(lubridate::year(j.date))]], "/", filename, ".tif")))
          
          for(k in unique(pts_to_fill$SiteCode)) {
            
            tmp <- pts_to_fill %>%
              dplyr::filter(SiteCode == k) %>%
              dplyr::select(SiteCode, geometry) %>%
              dplyr::distinct() %>%
              sf::st_transform(sf::st_crs(daymet))
            
            if(buffered == TRUE) {
              
              data[data$SiteCode == k & data$date == j.date, i] <- exactextractr::exact_extract(x = daymet, 
                                                                                                y = tmp, 
                                                                                                fun = "mean")
              
            } else {
              
              data[data$SiteCode == k & data$date == j.date, i] <- terra::extract(x = daymet, 
                                                                                  y = tmp, 
                                                                                  fun = "mean", na.rm = TRUE)[,2]
              
            }
            
          }
          
          message(paste0("Daymet [", i, "]: Date ", which(dates == j), " of ", length(dates)))  
          
        }
        
        if(TRUE %in% is.na(data[data$date <= latest.date, i])) {
          
          warning(paste0("Daymet [", i, "]: some points are close to shore, and so fall outside of raster coverage. For these cells, the nearest cell value will be used. Repairing now."))
          
          for(j in dates[date < latest.date]) {
            
            sites_to_fill <- unique(data$SiteCode[is.na(data[, i]) & data$date == j])
            
            if(nrow(sites_to_fill) > 0) {
              
              j.date <- as.Date(j)
              
              filename <- gsub(pattern = "DAYMET_", replacement = "DAYMET.", daymet.stats[[as.character(lubridate::year(j.date))]]$`File Name`[daymet.stats[[as.character(lubridate::year(j.date))]]$Date == j.date & daymet.stats[[as.character(lubridate::year(j.date))]]$Dataset == i])
              
              daymet <- terra::rast(ifelse(is.null(dl_path), paste0("./daymet/", appeears[[as.character(lubridate::year(j.date))]], "/", filename, ".tif"), paste0(dl_path, "/daymet/", appeears[[as.character(lubridate::year(j.date))]], "/", filename, ".tif")))
              
              for(k in sites_to_fill) {
                
                tmp <- data %>%
                  dplyr::filter(SiteCode == k) %>%
                  dplyr::select(SiteCode, geometry) %>%
                  dplyr::distinct() %>%
                  sf::st_buffer(2500) %>%
                  sf::st_transform(terra::crs(daymet))
                
                daymet_crop <- terra::crop(daymet, terra::vect(tmp)) %>%
                  terra::as.points()
                
                near.pt <- terra::nearest(terra::vect(tmp), daymet_crop)$to_id
                
                data[data$SiteCode == k & data$date == j, i] <- mean(terra::values(daymet_crop[near.pt])[,filename])
                
                
              }
            }
          }
        }
      }
      
    }
    
    data <- select(data, -date)
    
  }
  
  return(data)
  
}

nc_covariates <- function(data,
                          covariates = NULL,
                          buffer = FALSE,
                          buffer_radius = 500,
                          buffer_units = "m",
                          site_name = NULL,
                          coord_lat = NULL,
                          coord_lon = NULL,
                          date_year = NULL,
                          date_month = NULL,
                          date_day = NULL,
                          date_lubridate = NULL,
                          date_ordinal = NULL,
                          ed_email = NULL,
                          ed_username = NULL,
                          ed_password = NULL, # need to take this out of line
                          daymet_transfer = FALSE,
                          dl_path = NULL,
                          retain = TRUE) {
  
  if(daymet_transfer == FALSE) {
    
    data <- data_fmt(data, 
                     site_name = site_name,
                     coord_lon = coord_lon,
                     coord_lat = coord_lat,
                     date_year = date_year,
                     date_month = date_month,
                     date_day = date_day,
                     date_lubridate = date_lubridate,
                     date_ordinal = date_ordinal)
    
    data <- data_buff(data,
                      buffer = buffer,
                      buffer_radius = buffer_radius,
                      buffer_units = buffer_units)
    
    data <- landcover_extract(data,
                              covariates = covariates,
                              ed_email = ed_email,
                              ed_password = ed_password,
                              dl_path = dl_path,
                              retain = retain)
    
    data <- vegetation_extract(data,
                               covariates = covariates,
                               ed_email = ed_email,
                               ed_password,
                               dl_path = dl_path,
                               retain = retain)
    
    data <- elevation_extract(data,
                              covariates = covariates)
    
    data <- worldclim_extract(data,
                              covariates = covariates,
                              dl_path = dl_path,
                              retain = retain)
    
    data <- scanfi_extract(data,
                           covariates = covariates,
                           dl_path = dl_path,
                           retain = retain)
    
  }
  
  
  data <- daymet_extract(data,
                         covariates = covariates,
                         ed_username = ed_username,
                         ed_password = ed_password,
                         daymet_transfer = daymet_transfer,
                         dl_path = dl_path,
                         retain = retain)
  
  
}
