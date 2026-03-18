############################ SCRIPT INFORMATION ################################

# Script Title: Component functions to the nc_covariates function.

# Script Author: Rory Macklin (rmacklin@birdscanada.org)

########################## LOAD NECESSARY PACKAGES #############################

if (system.file(package = "librarian") == "") {
  install.packages("librarian")
}

if (system.file(package = "remotes") == "") {
  install.packages("remotes")
}

remotes::install_github("bluegreen-labs/appeears", build_vignettes = TRUE)

librarian::shelf(
  tidyverse,
  sf,
  "USEPA/elevatr",
  terra,
  exactextractr,
  geodata,
  "rspatial/luna",
  landscapemetrics,
  measurements,
  appeears
)


###################### PREEXISTING NATURECOUNTS FUNCTIONS ######################

# Function to check ordinal dates.

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

# Function to check if necessary packages are present.

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

# Function to check days of month.

dom_check <- function(d) {
  stp <- FALSE

  if (stringr::str_detect(d, "^[:digit:]+$")) {
    d <- as.numeric(d)
  }
  if (is.numeric(d)) {
    if (d < 1 | d > 31) {
      stp <- TRUE
    }
    if (round(d) != d) stp <- TRUE
  } else {
    d <- suppressWarnings(lubridate::ymd_hms(d, truncated = 4)) %>%
      lubridate::day()
    if (is.na(d)) stp <- TRUE
  }
  if (stp) {
    stop(
      "Day of month must be a number between 1 and 31. ",
      "If referring to an ordinal date (day of year), reformat with data_fmt()",
      " and the 'date_ordinal' argument.",
      call. = FALSE
    )
  }
  d
}

# Function to check months.

month_check <- function(m) {
  stp <- FALSE

  # Convert factor months to characters
  if (class(m) == "factor") {
    m <- as.character(m)
  }
  # Check if numeric values have been given as characters
  if (stringr::str_detect(m, "^[:digit:]+$")) {
    m <- as.numeric(m)
  }
  # Check numerics are between 1 and 12.
  if (is.numeric(m)) {
    if (m < 1 | m > 12) {
      stp <- TRUE
    }
    # Check numerics are whole numbers
    if (round(m) != m) stp <- TRUE
  } else {
    # If month name provided in either English or French, convert it to numeric.
    months <- data.frame(
      labels = c(
        "January",
        "Janvier",
        "Jan",
        "Janv",
        "Jan.",
        "Janv.",
        "February",
        "Février",
        "Fevrier",
        "Feb",
        "Févr",
        "Fevr",
        "Feb.",
        "Févr.",
        "Fevr.",
        "March",
        "Mars",
        "Mar",
        "Mar.",
        "April",
        "Avril",
        "Apr",
        "Avr",
        "Apr.",
        "Avr.",
        "May",
        "Mai",
        "May.",
        "Mai.",
        "June",
        "Juin",
        "Jun",
        "Jun.",
        "Juin.",
        "July",
        "Juillet",
        "Jul",
        "Juill",
        "Jul.",
        "Juill.",
        "August",
        "Août",
        "Aout",
        "Aug",
        "Aug.",
        "Août.",
        "Aout.",
        "September",
        "Septembre",
        "Sept",
        "Sept.",
        "October",
        "Octobre",
        "Oct",
        "Oct.",
        "November",
        "Novembre",
        "Nov",
        "Nov.",
        "December",
        "Décembre",
        "Decembre",
        "Dec",
        "Déc",
        "Dec.",
        "Déc."
      ),
      numerics = c(
        rep(1, times = 6),
        rep(2, times = 9),
        rep(3, times = 4),
        rep(4, times = 6),
        rep(5, times = 4),
        rep(6, times = 5),
        rep(7, times = 6),
        rep(8, times = 7),
        rep(c(9, 10, 11), each = 4),
        rep(12, times = 7)
      )
    )
    if (tolower(m) %in% tolower(months$labels)) {
      m <- months$numeric[tolower(months$labels) == tolower(m)]
    } else {
      # Stop if a non-month label character has been provided.
      stp <- TRUE
    }
    # Stop if missing values are present.
    if (is.na(m)) stp <- TRUE
  }
  if (stp) {
    stop(
      "Month must be either a number (1 = January, ..., 12 = December), ",
      "or a month name ('January'/'Jan'/'Jan.').",
      call. = FALSE
    )
  }
  return(m)
}

# New data table for sources of covariate data.

nc_covariate_table <- function() {
  cov.table <- data.frame(
    covariate_name = c(
      "modis_lctype1",
      "modis_lctype2",
      "modis_lctype3",
      "modis_lctype4",
      "modis_lctype5",
      "modis_snow",
      "modis_ndvi",
      "modis_evi",
      "elevation",
      "worldclim_tavg",
      "worldclim_tmax",
      "worldclim_tmin",
      "worldclim_prec",
      "worldclim_srad",
      "worldclim_wind",
      "worldclim_vapr",
      "scanfi_biomass",
      "scanfi_closure",
      "scanfi_height",
      "scanfi_nfilc",
      "scanfi_balsamfir",
      "scanfi_blackspruce",
      "scanfi_douglasfir",
      "scanfi_jackpine",
      "scanfi_lodgepolepine",
      "scanfi_ponderosapine",
      "scanfi_tamarack",
      "scanfi_whiteredpine",
      "scanfi_broadleaf",
      "scanfi_otherconifer",
      "daymet_dayl",
      "daymet_prcp",
      "dayment_srad",
      "daymet_swe",
      "daymet_tmax",
      "daymet_tmin",
      "daymet_vp"
    ),
    covariate_source = c(
      "MODIS Land Cover - IGBP global vegetation classification scheme",
      "MODIS Land Cover - University of Maryland (UMD) scheme",
      "MODIS Land Cover - MODIS-derived LAI/fPAR scheme",
      "MODIS Land Cover - MODIS-derived Net Primary Production scheme",
      "MODIS Land Cover - Plant Functional Type (PFT) scheme",
      "MODIS Snow Cover",
      "MODIS Vegetation Indices - Normalized Difference Vegetation Index",
      "MODIS Vegetation Indices - Enhanced Vegetation Index",
      "AWS Terrain Tiles Elevation (m)",
      "WorldClim - Monthly Average Temperature (degC), 1970-2000",
      "WorldClim - Monthly Maximum Temperature (degC), 1970-2000",
      "WorldClim - Monthly Minimum Temperature (degC), 1970-2000",
      "WorldClim - Monthly Precipitation (mm), 1970-2000",
      "WorldClim - Monthly Solar Radiation (kJ/m^2/day), 1970-2000",
      "WorldClim - Monthly Average Wind Speed (m/s), 1970-2000",
      "WorldClim - Monthly Average Water Vapor Pressure (kPa), 1970-2000",
      "SCANFI - Biomass (tons/ha)",
      "SCANFI - Crown closure (% covered by tree canopy)",
      "SCANFI - Height (m)",
      "SCANFI - NFI land cover class",
      "SCANFI - Balsam Fir cover proportion of total crown cover",
      "SCANFI - Black Spruce cover proportion of total crown cover",
      "SCANFI - Douglas Fir cover proportion of total crown cover",
      "SCANFI - Jack Pine cover proportion of total crown cover",
      "SCANFI - Lodgepole Pine cover proportion of total crown cover",
      "SCANFI - Ponderosa Pine cover proportion of total crown cover",
      "SCANFI - Tamarack cover proportion of total crown cover",
      "SCANFI - White and Red Pine cover proportion of total crown cover",
      "SCANFI - Broadleaf tree species cover proportion of total crown cover",
      "SCANFI - Other Conifer Species cover proportion of total crown cover",
      "Daymet - Daylength (s/day)",
      "Daymet - Precipitation (mm/day)",
      "Daymet - Shortwave radiation (W/m^2)",
      "Daymet - Snow water equivalent (kg/m^2)",
      "Daymet - Maximum air temperature (degrees C)",
      "Daymet - Minimum air temperature (degrees C)",
      "Daymet - Water vapor pressure (Pa)"
    ),
    covariate_source_specific = c(
      rep("MCD12Q1", times = 5),
      "MOD10A1",
      rep("MOD13A1", times = 2),
      NA,
      rep("WorldClim Ver. 2.1", times = 7),
      rep("SCANFI Ver. 1.2", times = 14),
      rep("DAYMET Ver. 004", times = 7)
    ),
    temporal_resolution = c(
      rep("Annual", times = 5),
      "Daily",
      rep("16-Day", times = 2),
      rep("Static", times = 22),
      rep("Daily", times = 7)
    ),
    spatial_resolution = c(
      rep("500 m", times = 8),
      "~600-800m",
      rep("~1 km^2", times = 7),
      rep("30 m", times = 14),
      rep("1 km", times = 7)
    ),
    via = c(
      rep("luna", times = 8),
      "elevatr",
      rep("geodata", times = 7),
      rep("Direct Download", times = 14),
      rep("appeears", times = 7)
    ),
    documentation = c(
      rep("https://doi.org/10.5067/MODIS/MCD12Q1.061", times = 5),
      "http://doi.org/10.5067/MODIS/MOD10A1.061",
      rep("https://doi.org/10.5067/MODIS/MOD13A1.061", times = 2),
      "https://github.com/USEPA/elevatr",
      rep("https://worldclim.org/data/worldclim21.html", times = 7),
      rep(
        "https://doi.org/10.23687/18e6a919-53fd-41ce-b4e2-44a9707c52dc",
        times = 14
      ),
      rep("https://doi.org/10.3334/ORNLDAAC/1840", times = 7)
    )
  )

  return(cov.table)
}

# Function to check data format and pull the information we need for extracting
# from external sources.

covariate_fmt_check <- function(data) {
  # Check packages
  have_pkg_check(c("sf", "terra"))

  # Check if input is a simple features object.
  if (inherits(data, "sf")) {
    # Store data type.
    data_type <- "sf"

    # Store data geometry.
    data_geometry <- as.character(sf::st_geometry_type(
      data,
      by_geometry = FALSE
    ))

    # Handle objects containing mixtures of multiple geometry types.
    if (data_geometry == "GEOMETRY") {
      stop(
        "[Data Formatting] mixed sf geometries detected. Please provide a set of",
        " only POINT geometries or only POLYGON geometries.",
        call. = FALSE
      )
    }

    # Reject objects that are not point or polygon objects.
    if (!(data_geometry %in% c("POINT", "POLYGON"))) {
      stop(
        "[Data Formatting] sf object provided, but not a set of POINT or",
        " POLYGON geometries.",
        call. = FALSE
      )
    }

    # Return stored information.
    return(list(type = data_type, geometry = data_geometry))

    # Check if input is a terra SpatVector.
  } else if (inherits(data, "SpatVector")) {
    # Store data type.
    data_type <- "terra"

    # Store data geometry.
    data_geometry <- terra::geomtype(data)

    # Reject objects that are not point or polygon objects.
    if (!(data_geometry %in% c("points", "polygons"))) {
      stop(
        "[Data Formatting] terra object provided, but not a set of points or",
        " polygons.",
        call. = FALSE
      )
    }

    # Return stored information.
    return(list(type = data_type, geometry = data_geometry))

    # Check if data is a dataframe.
  } else if (is.data.frame(data)) {
    # Store data type.
    data_type <- "data.frame"

    # Return stored information.
    return(list(type = data_type))

    # Reject all other data types.
  } else {
    stop(
      "[Data Formatting] invalid data format. Please provide data as either a",
      " dataframe, sf object with either `POINT` or `POLYGON` geometry, or",
      " terra SpatVector object with `points` or `polygons` geometry.",
      call. = FALSE
    )
  }
}

# Function to standardize formatting of data for extraction.

data_fmt <- function(
  data,
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'.
  coord_lon = NULL, # as in cosewic_ranges
  coord_lat = NULL, # as in cosewic_ranges
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'.
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'.
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'.
  date_lubridate = NULL, # optional argument to provide column name containing
  # 'lubridate' date objects.
  date_ordinal = NULL, # optional argument to provide column name containing
  # ordinal dates.
  crs = NULL # optional argument to provide a Coordinate Reference System for
  # provided data.
) {
  message("[Data Formatting] beginning formatting.")

  # Check packages

  have_pkg_check(c(
    "sf",
    "terra",
    "tidyterra"
  ))

  # Check data type - we need either a dataframe, sf points object, sf polygon,
  # or terra SpatVector.

  input_fmt <- covariate_fmt_check(data)

  # Deal with alternate CRS's

  # Check that 'crs' argument has been provided.
  if (!is.null(crs)) {
    # Check if input is an sf object.
    if (input_fmt$type == "sf") {
      # Check if provided sf object has a CRS. If missing, set to provided CRS.
      # Warn.
      if (is.na(sf::st_crs(data))) {
        warning(
          "[Data Formatting] the CRS of the provided sf object is missing, it",
          " will be set to the alternate CRS specified in the 'crs' argument.",
          call. = FALSE
        )

        suppressWarnings(sf::st_crs(data) <- crs)

        # If sf object still is missing CRS, suggests that provided CRS is
        # invalid. Return error.
        if (is.na(sf::st_crs(data))) {
          stop(
            "[Data Formatting] the provided CRS is invalid. CRS must be a",
            " valid proj4string character, a valid epsg integer value, or a list",
            " containing named elements proj4string (character) and/or epsg",
            " (integer).",
            call. = FALSE
          )
        }
      } else {
        # If sf object has a CRS and the 'crs' argument has been provided, use
        # the CRS included in the sf object. Warn.
        warning(
          "[Data Formatting] the sf object provided has a specified CRS and a",
          " CRS has been provided using the 'crs' argument. The CRS of the sf",
          " object will be used.",
          call. = FALSE
        )

        crs <- sf::st_crs(data)
      }
    }

    # Check if input is a terra SpatVector.
    if (input_fmt$type == "terra") {
      # Check if provided terra object has a CRS. If missing, set to provided
      # CRS. Warn.
      if (terra::crs(data) == "") {
        warning(
          "[Data Formatting] the CRS of the provided terra object is missing,",
          " it will be set to the alternate CRS specified in the 'crs'",
          " argument.",
          call. = FALSE
        )

        # Convert terra warnings associated with invalid CRS inputs into errors.
        tryCatch(
          terra::crs(data) <- crs,
          warning = function(w) {
            if (
              "[crs<-] Cannot set SRS to vector: empty srs" %in%
                conditionMessage(w) |
                paste0(
                  "PROJ: proj_create_from_database: crs not found:",
                  " EPSG:234634 (GDAL error 1)"
                ) %in%
                  conditionMessage(w)
            ) {
              stop(
                "[Data Formatting] the provided CRS is invalid. CRS",
                " must be a character string in WKT (e.g. 'EPSG:4326') or",
                " PROJ-string format (e.g. '+proj=utm +zone=12').",
                call. = FALSE
              )
            } else {
              warning(conditionMessage(w), call. = FALSE)
            }
          },
          error = function(e) {
            if (
              conditionMessage(e) ==
                paste0(
                  "[crs] I do not know what",
                  " to do with this argument",
                  " (expected a character",
                  " string)"
                )
            ) {
              stop(
                "[Data Formatting] the provided CRS is invalid. CRS",
                " must be a character string in WKT (e.g. 'EPSG:4326') or",
                " PROJ-string format (e.g. '+proj=utm +zone=12').",
                call. = FALSE
              )
            } else {
              stop(conditionMessage(e), call. = FALSE)
            }
          }
        )
      } else {
        # If terra object has a CRS and the 'crs' argument has been provided,
        # use the CRS included in the terra object. Warn.
        warning(
          "[Data Formatting] the terra object provided has a specified CRS and",
          " a CRS has been provided using the 'crs' argument. The CRS of the",
          " terra object will be used.",
          call. = FALSE
        )

        crs <- terra::crs(data)
      }
    }

    # If provided data is a data.frame, make sure we have the names of columns
    # pointing us to associated coordinate data. If not, return error.
    if (
      input_fmt$type == "data.frame" & (is.null(coord_lon) | is.null(coord_lat))
    ) {
      stop(
        "[Data Formatting] alternate CRS provided, but without specified",
        " column for one or more coordinate. Use the 'coord_lon' argument to",
        " give the name of column containing the X-coordinate, and the",
        " 'coord_lat' argument to give the name of the column containing the",
        " Y-coordinate.",
        call. = FALSE
      )
    }
  }

  # If no 'crs' argument is provided, and provided sf object lacks a CRS,
  # return error.
  if (is.null(crs) & input_fmt$type == "sf") {
    if (is.na(sf::st_crs(data))) {
      stop(
        "[Data Formatting] provided sf object lacks a CRS. Please specify",
        " using the 'crs' argument or provide an sf object with a CRS.",
        call. = FALSE
      )
    }
  }

  # If no 'crs' argument is provided, and provided terra object lacks a CRS,
  # return error.
  if (is.null(crs) & input_fmt$type == "terra") {
    if (terra::crs(data) == "") {
      stop(
        "[Data Formatting] provided terra object lacks a CRS. Please specify",
        " using the 'crs' argument or provide a terra object with a CRS.",
        call. = FALSE
      )
    }
  }

  # If no 'crs' argument is provided, and provided data is a dataframe, assume
  # it is the default NatureCounts format which uses lat/lon and use EPSG:4326.
  # Warn.
  if (is.null(crs) & input_fmt$type == "data.frame") {
    warning(
      "[Data Formatting] as the 'crs' argument is not specified, data CRS is",
      " assumed to be EPSG:4326.",
      call. = FALSE
    )

    crs <- 4326
  }

  # If spatial object is provided and the 'coord_lon'/'coord_lat' arguments
  # have been provided, use the coordinate data included in the spatial object.
  # Warn.
  if (
    input_fmt$type %in%
      c("sf", "terra") &
      (!is.null(coord_lon) | !is.null(coord_lat))
  ) {
    warning(
      "[Data Formatting] sf or terra object provided as well as a lat/lon",
      " column name. lat/lon will be derived from the spatial data within the",
      " sf/terra object and specified lat/lon column will be ignored.",
      call. = FALSE
    )

    coord_lon <- NULL
    coord_lat <- NULL
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(
    site_name,
    coord_lon,
    coord_lat,
    date_year,
    date_month,
    date_day,
    date_lubridate,
    date_ordinal
  )

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present.
  if (!(all(specified_cols %in% data_cols))) {
    stop(
      "[Data Formatting] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that",
      " diverges from NatureCounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }
    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (input_fmt$type == "data.frame") {
    if (!is.null(coord_lon)) {
      # Edge case: there is a col called longitude that isn't coord_lon.
      # Remove.
      if ("longitude" %in% names(data) & !(coord_lon == "longitude")) {
        data <- dplyr::select(data, -longitude)
      }

      if (input_fmt$type == "sf") {
        data <- sf::st_sf(data)
      }

      data <- dplyr::rename(data, "longitude" = !!coord_lon)
    }

    data$longitude <- as.numeric(data$longitude)

    if (!is.null(coord_lat)) {
      # Edge case: there is a col called latitude that isn't coord_lat. Remove.
      if ("latitude" %in% names(data) & !(coord_lat == "latitude")) {
        data <- dplyr::select(data, -latitude)
      }

      if (input_fmt$type == "sf") {
        data <- sf::st_sf(data)
      }

      data <- dplyr::rename(data, "latitude" = !!coord_lat)
    }

    data$latitude <- as.numeric(data$latitude)
  }

  if (!is.null(date_year)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  if (!is.null(date_month)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }

  # Use month_check() to validate month data. 'if' wrapper needed to handle
  # cases where no month column was provided, and a lubridate or ordinal date
  # column was provided instead.
  if ("survey_month" %in% names(data)) {
    month_corr <- c()

    for (i in 1:length(data$survey_month)) {
      month_corr[i] <- month_check(data$survey_month[i])
    }

    data$survey_month <- month_corr
  }

  if (!is.null(date_day)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_day" = !!date_day)
  }

  # Use dom_check() to validate day data. 'if' wrapper needed to handle cases
  # where no month column was provided, and a lubridate or ordinal date column
  # was provided instead.
  if ("survey_day" %in% names(data)) {
    for (i in data$survey_day) {
      dom_check(i)
    }
  }

  # If a date in lubridate or ordinal format is provided, make year, month and
  # day columns.
  if (!is.null(date_lubridate)) {
    # Standardize date column name
    data <- dplyr::rename(data, "date" = !!date_lubridate)

    # Check that provided lubridate data is a date object. If not, return error.
    if (!lubridate::is.Date(data$date)) {
      stop(
        "[Data Formatting] column ",
        date_lubridate,
        " expected to be in `Date` format, but is not.",
        call. = FALSE
      )
    }

    # Check that provided lubridate data is an instant rather than a duration
    # object. If not, return error.
    if (!lubridate::is.instant(data$date)) {
      stop(
        "[Data Formatting] column ",
        date_lubridate,
        " expected to be a single instant in time, but is not.",
        call. = FALSE
      )
    }

    # Check that all dates are either the current date or in the past. If not,
    # return error.
    if (!all(data$date <= as.Date(Sys.Date()))) {
      stop(
        "[Data Formatting] some dates are in the future! Covariate data only",
        " available for data in the past.",
        call. = FALSE
      )
    }

    # If lubridate column provided alongside other specified date column
    # options, use data from lubridate columns. Warn.
    if (
      !is.null(date_year) |
        !is.null(date_month) |
        !is.null(date_day) |
        !is.null(date_ordinal)
    ) {
      date_cols <- c(
        date_lubridate,
        date_year,
        date_month,
        date_day,
        date_ordinal
      )
      date_cols <- date_cols[!is.null(date_cols)]

      warning(
        paste0(
          "[Data Formatting] multiple date column options provided including ",
          stringr::str_flatten_comma(date_cols),
          ". The data in ",
          date_lubridate,
          " will be used."
        ),
        call. = FALSE
      )
    }

    # Extract year/month/day columns from lubridate date.
    data$survey_year <- lubridate::year(data$date)

    data$survey_month <- lubridate::month(data$date)

    data$survey_day <- lubridate::day(data$date)

    # In case ordinal data has also been provided, set to NULL so dates aren't
    # recalculated using ordinal data.
    date_year <- NULL

    date_ordinal <- NULL
  }

  # If a date in ordinal format is provided (and a date in lubridate format is
  # not provided, see above), make year, month and day columns.
  if (!is.null(date_ordinal)) {
    # Standardize ordinal date column name
    data <- dplyr::rename(data, "doy" = !!date_ordinal)

    # Check that year data has been provided alongside ordinal day data as this
    # is needed to convert to calendar date. If not, return error.
    if (!("survey_year" %in% names(data))) {
      stop(
        "[Data Formatting] if providing an ordinal date, year data must",
        " accompany it. Please provide a column with associated year data",
        " using the `date_year` argument.",
        call. = FALSE
      )
    }

    # Use doy_check() to validate ordinal date data.
    for (i in data$doy) {
      doy_check(i)
    }

    # If month or day data has also been provided, warn that ordinal date data
    # will supersede it.
    if (!is.null(date_month) | !is.null(date_day)) {
      warning(
        "[Data Formatting] dates derived from ordinal dates will supersede",
        " provided month and/or day data.",
        call. = FALSE
      )
    }

    # If ordinal date is numeric, add it to the first day of the associated
    # year to get the calendar date.
    if (is.numeric(data$doy)) {
      data$date <- as.Date(paste0(data$survey_year, "-01-01")) + data$doy - 1
    }

    # If ordinal date has been provided as a date object (likely due to
    # misunderstanding of the meaning of ordinal date) convert it to ordinal
    # date and add it to the first day of the associated calendar year.
    if (lubridate::is.Date(data$doy)) {
      data$date <- as.Date(paste0(data$survey_year, "-01-01")) +
        lubridate::yday(data$doy) -
        1
    }

    # Extract month and day data from ordinal-derived date column
    data$survey_month <- lubridate::month(data$date)

    data$survey_day <- lubridate::day(data$date)
  }

  # Ensure date columns are numeric.
  data$survey_year <- as.numeric(data$survey_year)

  data$survey_month <- as.numeric(data$survey_month)

  data$survey_day <- as.numeric(data$survey_day)

  # If data is a dataframe, ensure that there are no rows missing coordinate
  # data as this would prevent conversion into an sf object. Warn.
  if (input_fmt$type == "data.frame") {
    if (NA %in% unique(data$latitude) | NA %in% unique(data$longitude)) {
      warning(
        "[Data Formatting] some rows missing coordinate data will be dropped.",
        call. = FALSE
      )

      data <- dplyr::filter(data, !(is.na(latitude) | is.na(longitude)))
    }
  }

  # Handle missing SurveyAreaIdentifiers and ensure coordinates are present in
  # the data for later use in nc_covariates_merge().
  if (TRUE %in% is.na(data$SurveyAreaIdentifier)) {
    # For dataframe objects create an object containing all X/Y coordinates
    # that do not have an associated SurveyAreaIdentifier.
    if (input_fmt$type == "data.frame") {
      missing_sitecode <- data %>%
        dplyr::select(SurveyAreaIdentifier, latitude, longitude) %>%
        dplyr::filter(is.na(SurveyAreaIdentifier)) %>%
        dplyr::distinct()
    }

    # For sf objects, create an object containing all X/Y coordinates (derived
    # from geometries) that do not have an associated SurveyAreaIdentifier.
    # Also append coordinates to original data object for later joining.
    if (input_fmt$type == "sf") {
      missing_sitecode <- data %>%
        dplyr::select(SurveyAreaIdentifier, geometry)

      # For polygons, use the centroid as the X/Y coordinates.
      if (input_fmt$geometry == "POLYGON") {
        missing_sitecode <- suppressWarnings(sf::st_centroid(missing_sitecode))
      }

      # Extract coordinates and bind to data. Drop geometry and get all
      # unique coordinate combinations with missing SurveyAreaIdentifiers.
      missing_sitecode <- cbind(
        missing_sitecode,
        sf::st_coordinates(missing_sitecode)
      ) %>%
        dplyr::rename(longitude = X, latitude = Y) %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(is.na(SurveyAreaIdentifier)) %>%
        dplyr::distinct()

      # Edge case: there is a col called X. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("X" %in% names(data)) {
        data <- dplyr::select(data, -X)
      }

      # Edge case: there is a col called Y. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("Y" %in% names(data)) {
        data <- dplyr::select(data, -Y)
      }

      # Edge case: there is a col called longitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("longitude" %in% names(data)) {
        data <- dplyr::select(data, -longitude)
      }

      # Edge case: there is a col called latitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("latitude" %in% names(data)) {
        data <- dplyr::select(data, -latitude)
      }

      # Append coordinates (from centroids if polygons) to provided data object.
      if (input_fmt$geometry == "POLYGON") {
        data <- cbind(
          data,
          sf::st_coordinates(suppressWarnings(sf::st_centroid(data)))
        ) %>%
          dplyr::rename(longitude = X, latitude = Y)
      } else {
        data <- cbind(data, sf::st_coordinates(data)) %>%
          dplyr::rename(longitude = X, latitude = Y)
      }
    }

    # For terra objects, create an object containing all X/Y coordinates
    # (derived from geometries) that do not have an associated
    # SurveyAreaIdentifier. Also append coordinates to original data object for
    # later joining.
    if (input_fmt$type == "terra") {
      missing_sitecode <- data %>%
        tidyterra::select(SurveyAreaIdentifier)

      # For polygons, use the centroid as the X/Y coordinates.
      if (input_fmt$geometry == "polygons") {
        missing_sitecode <- terra::centroids(missing_sitecode)
      }

      # Extract coordinates and bind to data. Drop geometry and get all
      # unique coordinate combinations with missing SurveyAreaIdentifiers.
      missing_sitecode <- cbind(
        missing_sitecode,
        terra::crds(missing_sitecode)
      ) %>%
        tidyterra::rename(longitude = x, latitude = y) %>%
        terra::as.data.frame() %>%
        dplyr::filter(is.na(SurveyAreaIdentifier)) %>%
        dplyr::distinct()

      # Edge case: there is a col called x. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("x" %in% names(data)) {
        data <- tidyterra::select(data, -x)
      }

      # Edge case: there is a col called y. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("y" %in% names(data)) {
        data <- tidyterra::select(data, -y)
      }

      # Edge case: there is a col called longitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("longitude" %in% names(data)) {
        data <- dplyr::select(data, -longitude)
      }

      # Edge case: there is a col called latitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("latitude" %in% names(data)) {
        data <- dplyr::select(data, -latitude)
      }

      # Append coordinates (from centroids if polygons) to provided data object.
      if (input_fmt$geometry == "polygons") {
        data <- cbind(data, terra::crds(terra::centroids(data))) %>%
          dplyr::rename(longitude = x, latitude = y)
      } else {
        data <- cbind(data, terra::crds(data)) %>%
          dplyr::rename(longitude = x, latitude = y)
      }
    }

    # Create a dummy SurveyAreaIdentifier for all unique coordinate combinations
    # which are missing an associated SurveyAreaIdentifier.
    for (i in 1:nrow(missing_sitecode)) {
      missing_sitecode$SurveyAreaIdentifier[i] <- paste0("FilledSurveyArea", i)
    }

    # Use coordinates to join dummy SurveyAreaIdentifiers to original data.
    for (i in missing_sitecode$latitude) {
      for (j in missing_sitecode$longitude[missing_sitecode$latitude == i]) {
        data$SurveyAreaIdentifier[
          data$latitude == i & data$longitude == j
        ] <- missing_sitecode$SurveyAreaIdentifier[
          missing_sitecode$latitude == i & missing_sitecode$longitude == j
        ]
      }
    }
  } else {
    # In case all SurveyAreaIdentifiers are present, append coordinates to
    # spatial data objects for later use in nc_covariates_merge().
    if (input_fmt$type == "sf") {
      # Edge case: there is a col called X. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("X" %in% names(data)) {
        data <- dplyr::select(data, -X)
      }

      # Edge case: there is a col called Y. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("Y" %in% names(data)) {
        data <- dplyr::select(data, -Y)
      }

      # Edge case: there is a col called longitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("longitude" %in% names(data)) {
        data <- dplyr::select(data, -longitude)
      }

      # Edge case: there is a col called latitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("latitude" %in% names(data)) {
        data <- dplyr::select(data, -latitude)
      }

      # Append coordinates (from centroids if polygons) to provided data object.
      if (input_fmt$geometry == "POLYGON") {
        data <- cbind(
          data,
          sf::st_coordinates(suppressWarnings(sf::st_centroid(data)))
        ) %>%
          dplyr::rename(longitude = X, latitude = Y)
      } else {
        data <- cbind(data, sf::st_coordinates(data)) %>%
          dplyr::rename(longitude = X, latitude = Y)
      }
    }

    if (input_fmt$type == "terra") {
      # Edge case: there is a col called x. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("x" %in% names(data)) {
        data <- tidyterra::select(data, -x)
      }

      # Edge case: there is a col called y. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("y" %in% names(data)) {
        data <- tidyterra::select(data, -y)
      }

      # Edge case: there is a col called longitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("longitude" %in% names(data)) {
        data <- tidyterra::select(data, -longitude)
      }

      # Edge case: there is a col called latitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("latitude" %in% names(data)) {
        data <- tidyterra::select(data, -latitude)
      }

      # Append coordinates (from centroids if polygons) to provided data object.
      if (input_fmt$geometry == "polygons") {
        data <- cbind(data, terra::crds(terra::centroids(data))) %>%
          dplyr::rename(longitude = x, latitude = y)
      } else {
        data <- cbind(data, terra::crds(data)) %>%
          dplyr::rename(longitude = x, latitude = y)
      }
    }
  }

  # Create base list of columns to preserve.
  keep_cols <- c(
    "SurveyAreaIdentifier",
    "latitude",
    "longitude",
    "survey_year",
    "survey_month",
    "survey_day"
  )

  # If ordinal date provided, preserve it for later use in
  # nc_covariates_merge().
  if (!is.null(date_ordinal)) {
    keep_cols <- c(keep_cols[1:4], date_ordinal, keep_cols[5:6])
  }

  # If lubridate date provided, preserve it for later use in
  # nc_covariates_merge().
  if (!is.null(date_lubridate)) {
    keep_cols <- c(keep_cols[1:3], date_lubridate, keep_cols[4:6])
  }

  # For dataframe objects, convert to spatial features object.
  if (input_fmt$type == "data.frame") {
    # Get all distinct combinations of kept columns, convert to sf object.
    suppressWarnings(
      data <- dplyr::select(data, tidyselect::all_of(keep_cols)) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(
          coords = c("longitude", "latitude"),
          crs = crs,
          remove = FALSE
        )
    )

    # If created spatial object CRS is missing, provided CRS was invalid.
    # Return error.
    if (is.na(sf::st_crs(data))) {
      stop(
        "[Data Formatting] the provided CRS is invalid. CRS must be a valid",
        " proj4string character, a valid epsg integer value, or a list",
        " containing named elements proj4string (character) and/or epsg",
        " (integer).",
        call. = FALSE
      )
    }

    # Convert to CRS with metres as a base unit to facilitate buffering.
    data <- sf::st_transform(data, "ESRI:102001")
  }

  # For sf objects, keep all distinct combinations of kept columns.
  if (input_fmt$type == "sf") {
    # Ensure geometry column is retained.
    keep_cols <- c(keep_cols, "geometry")

    # Convert to CRS with metres as a base unit to facilitate buffering.
    data <- dplyr::select(data, tidyselect::all_of(keep_cols)) %>%
      dplyr::distinct() %>%
      sf::st_transform("ESRI:102001")
  }

  # For terra objects, keep all distinct combinations of kept columns and
  # convert to CRS with metres as a base unit to facilitate buffering.
  if (input_fmt$type == "terra") {
    data <- tidyterra::select(data, tidyselect::all_of(keep_cols)) %>%
      tidyterra::distinct() %>%
      terra::project("ESRI:102001")
  }

  # Store specified column names and crs as attributes so that they don't need
  # to be specified any time associated functions are called.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
    attr(data, "site_name") <- site_name
  }

  if (!is.null(coord_lon)) {
    names(data)[names(data) == "longitude"] <- coord_lon
    attr(data, "coord_lon") <- coord_lon
  }

  if (!is.null(coord_lat)) {
    names(data)[names(data) == "latitude"] <- coord_lat
    attr(data, "coord_lat") <- coord_lat
  }

  if (!is.null(date_year)) {
    names(data)[names(data) == "survey_year"] <- date_year
    attr(data, "date_year") <- date_year
  }

  if (!is.null(date_month)) {
    names(data)[names(data) == "survey_month"] <- date_month
    attr(data, "date_month") <- date_month
  }

  if (!is.null(date_day)) {
    names(data)[names(data) == "survey_day"] <- date_day
    attr(data, "date_day") <- date_day
  }

  if (!is.null(date_ordinal)) {
    attr(data, "date_ordinal") <- date_ordinal
  }

  if (!is.null(date_lubridate)) {
    attr(data, "date_lubridate") <- date_lubridate
  }

  if (!is.null(crs)) {
    attr(data, "crs") <- crs
  }

  # Return formatted data.
  return(data)
}

# Function to buffer data by a specified radius
data_buff <- function(
  data,
  buffer = FALSE, # Should the data be buffered?
  buffer_distance = 500, # Distance to buffer by.
  buffer_units = "m" # Units of provided distance.
) {
  # Unless buffering requested, do nothing.
  if (buffer == TRUE) {
    # Check packages
    have_pkg_check(c("terra", "sf", "measurements"))

    # Check data is in the desired format
    input_fmt <- covariate_fmt_check(data)

    # If not an sf or terra object, return error and point towards data_fmt().
    if (input_fmt$type == "data.frame") {
      stop(
        "[Data Buffering] buffering requires an sf or terra object as input in",
        " this workflow. Consider using `data_fmt` to conform data first.",
        call. = FALSE
      )
    }

    # Ensure radius is coercable to a numeric value.
    buffer_distance <- as.numeric(buffer_distance)

    # If unit provided is not compatible with measurements::conv_unit(), return
    # error.
    if (!(buffer_units %in% c("m", "km", "ft", "yd", "mi", "naut_mi"))) {
      stop(
        "[Data Buffering] buffer units not recognized: please set buffer_units to one of 'm' [metres], 'km' [kilometers], 'ft' [feet], 'yd' [yards], 'mi' [miles], or 'naut_mi' [nautical miles].",
        call. = FALSE
      )
    }

    message(
      "[Data Buffering] buffering sites by ",
      buffer_distance,
      buffer_units,
      " radius",
      ifelse(buffer_distance == 500 & buffer_units == "m", " (default)", ""),
      "."
    )

    # Buffer sf objects by requested amount.
    if (input_fmt$type == "sf") {
      # Store original CRS so data can be returned as provided.
      orig_crs <- terra::crs(data)

      # If not already in CRS used herein, transform.
      if (!(orig_crs == terra::crs("ESRI:102001"))) {
        data <- sf::st_transform(data, "ESRI:102001")
      }

      # If sf object contains polygon, warn that polygons will be buffered on
      # all sides. This might help users catch mistakes when pre-buffered data
      # is provided and they don't want it additionally buffered.
      if (input_fmt$geometry == "POLYGON") {
        warning(
          "[Data Buffering] sf POLYGON geometry provided. Existing polygons",
          " will be buffered by an additional ",
          buffer_distance,
          buffer_units,
          ".",
          call. = FALSE
        )
      }

      # Buffer. Use measurements::conv_unit() to handle units other than metres.
      data <- sf::st_buffer(
        data,
        measurements::conv_unit(
          x = buffer_distance,
          from = buffer_units,
          to = "m"
        )
      )

      # Back-transform to original CRS if it wasn't the CRS used herein.
      if (!(orig_crs == terra::crs("ESRI:102001"))) {
        data <- sf::st_transform(data, orig_crs)
      }
    }

    # Buffer terra objects by requested amount.
    if (input_fmt$type == "terra") {
      # Store original CRS so data can be returned as provided.
      orig_crs <- terra::crs(data)

      # If not already in CRS used herein, transform.
      if (!(orig_crs == terra::crs("ESRI:102001"))) {
        data <- terra::project(data, "ESRI:102001")
      }

      # If terra object contains polygon, warn that polygons will be buffered on
      # all sides. This might help users catch mistakes when pre-buffered data
      # is provided and they don't want it additionally buffered.
      if (input_fmt$geometry == "polygons") {
        warning(
          "[Data Buffering] terra polygons provided. Existing polygons will",
          " be buffered by an additional ",
          buffer_distance,
          buffer_units,
          ".",
          call. = FALSE
        )
      }

      # Buffer. Use measurements::conv_unit() to handle units other than metres.
      data <- terra::buffer(
        data,
        measurements::conv_unit(
          x = buffer_distance,
          from = buffer_units,
          to = "m"
        )
      )

      # Back-transform to original CRS if it wasn't the CRS used herein.
      if (!(orig_crs == terra::crs("ESRI:102001"))) {
        data <- terra::project(data, orig_crs)
      }
    }
  }

  # Return provided data if no buffering requested, or buffered data if
  # buffering requested.
  return(data)
}

############################ LANDCOVER FUNCTIONS ###############################

# Function for downloading MODIS MCD12Q1 data from NASA EarthData. Wrapper for
# luna::getNASA().
landcover_download <- function(
  data,
  ed_email, # users' EarthData account email address.
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  dl_path = NULL # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'modis/' in the working
  # directory.
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra",
    "luna"
  ))

  # Check that an EarthData account email has been provided. If not, return
  # error.
  if (missing(ed_email)) {
    stop(
      "[MODIS Landcover Download] MODIS Landcover data requested but Earthdata",
      " system login information not supplied. Please register at",
      " https://urs.earthdata.nasa.gov/users/new and supply using `ed_email`",
      " argument.",
      call. = FALSE
    )
  }

  # Check whether an EarthData password exists in the environment (is specified
  # earlier in the nc_covariates() workflow), and if not, request using
  # askpass::askpass().
  if (is.null(parent.frame()$ed_password)) {
    ed_password <- askpass::askpass(
      prompt = paste0(
        "Please enter password for ",
        "EarthData user '",
        ed_email,
        "'."
      )
    )
  } else {
    ed_password <- parent.frame()$ed_password
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[MODIS Landcover Download] downloading requires an sf or terra object",
      " as input in this workflow. Consider using `data_fmt` to conform data",
      " first.",
      call. = FALSE
    )
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(data, "date_year"))) {
    date_year <- attr(data, "date_year")
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name, date_year)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      (!("SurveyAreaIdentifier" %in% data_cols) |
        !("survey_year" %in% data_cols))
  ) {
    stop(
      "[MODIS Landcover Download] some specified columns missing from the",
      " data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that",
      " diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier") %in% data_cols) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_year) & !("survey_year") %in% data_cols) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  # Create area of interest polygon from provided sf object.
  if (input_fmt$type == "sf") {
    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_transform("ESRI:102001") %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    } else {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    }
  }

  # Create area of interest polygon from provided terra object.
  if (input_fmt$type == "terra") {
    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::project("ESRI:102001") %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    } else {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    }

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data) # Maybe down the line write full process out
    # in terra for terra data?
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./modis/MCD12Q1")) {
    dir.create("./modis/MCD12Q1", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/modis/MCD12Q1"))) {
    dir.create(paste0(dl_path, "/modis/MCD12Q1"), recursive = TRUE)
  }

  message("[MODIS Landcover Download] downloading data.")

  # Call to API using luna::getNASA()
  modis_files <- luna::getNASA(
    product = "MCD12Q1",
    start = paste0(min(data$survey_year), "-01-01"), # Starting year
    end = paste0(max(data$survey_year), "-12-31"), # End year
    aoi = terra::ext(terra::project(study_area, "epsg:4326")),
    download = TRUE,
    overwrite = FALSE,
    path = ifelse(
      is.null(dl_path),
      "./modis/MCD12Q1",
      paste0(dl_path, "/modis/MCD12Q1")
    ),
    username = ed_email,
    password = ed_password
  )

  # Return character vector of filepaths to downloaded files.
  return(modis_files)
}

# Function to extract land cover data from provided MODIS MCD12Q1 data files.
landcover_extract <- function(
  data,
  covariates = "modis_lctype1", # Other options listed in nc_covariate_table().
  landcover_files, # Character vector of filepaths to downloaded files.
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  retain = TRUE # Should data files be kept after extraction?
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "luna",
    "terra",
    "stats"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[MODIS Landcover Extraction] covariates either not listed or one or",
      " more are invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no landcover files are provided, return error.
  if (missing(landcover_files)) {
    stop(
      "[MODIS Landcover Extraction] no landcover files provided to extract from. Please provide a vector containing filepaths of all necessary MODIS files for your data. Data can be downloaded using landcover_download().",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[MODIS Landcover Extraction] extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Store attributes so they don't get lost.

  # List potential attributes.
  attr_names <- c(
    "site_name",
    "coord_lon",
    "coord_lat",
    "date_year",
    "date_month",
    "date_day",
    "date_ordinal",
    "date_lubridate",
    "crs"
  )

  # If any potential attribute names are present in the data attributes,
  # store.
  if (length(attr_names[attr_names %in% names(attributes(data))]) > 0) {
    attrs <- attributes(data)[attr_names[
      attr_names %in% names(attributes(data))
    ]]
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(data, "date_year"))) {
    date_year <- attr(data, "date_year")
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name, date_year)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      (!("SurveyAreaIdentifier" %in% data_cols) |
        !("survey_year" %in% data_cols))
  ) {
    stop(
      "[MODIS Landcover Extraction] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_year) & !("survey_year" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  # Check whether object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "sf") {
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
  }

  if (input_fmt$type == "terra") {
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("landscapemetrics")
  }

  # Parse dates stored in filenames of MODIS data files and append column to
  # filenames.
  modis_files <- luna::modisDate(landcover_files)
  modis_files <- cbind(
    modis_files,
    as.data.frame(luna::modisExtent(modis_files$filename))
  )

  modis_files$year <- as.numeric(modis_files$year)

  # Build object to use in matching sites to their respective MODIS data file.
  modis_match <- data %>%
    dplyr::select(SurveyAreaIdentifier, survey_year, geometry) %>%
    sf::st_transform(terra::crs(terra::rast(modis_files$filename[1])))

  # If buffered, extract coordinates from centroids. Append coordinates.
  if (buffered == TRUE) {
    suppressWarnings(
      modis_match <- cbind(
        modis_match,
        sf::st_coordinates(sf::st_centroid(modis_match))
      )
    )
  } else {
    modis_match <- cbind(modis_match, sf::st_coordinates(modis_match))
  }

  # Loop through years to check that all are represented in the MODIS data.
  # When requests are placed for data containing years not covered by MODIS,
  # nothing in the downloading process alerts the user to this. Warn here, and
  # use nearest year.
  for (i in sort(unique(modis_match$survey_year))) {
    if (!(i %in% modis_files$year)) {
      warning(
        paste0(
          "[MODIS Landcover Extraction]: MODIS data not available for ",
          i,
          " - using data from nearest year (",
          unique(modis_files$year)[which(
            abs(i - unique(modis_files$year)) ==
              min(abs(i - unique(modis_files$year)))
          )],
          ")."
        ),
        call. = FALSE
      )
    }
  }

  # Open vector to store names of out of range sites. NOTE: this might not be
  # that informative for datasets without dedicated site names.
  out_of_range <- c()

  # Loop through each site-year combination and match to appropriate file.
  for (i in unique(modis_match$SurveyAreaIdentifier)) {
    for (j in unique(modis_match$survey_year[
      modis_match$SurveyAreaIdentifier == i
    ])) {
      # Create temporary spatial object containing only the buffer for site i.
      tmp <- dplyr::filter(
        modis_match,
        SurveyAreaIdentifier == i,
        survey_year == j
      ) %>%
        dplyr::distinct()

      # Check if the coordinates of that site fall within the coverage of the
      # provided MODIS files. If not, warn and note site name. If not, proceed
      # with file-matching.
      if (
        all(tmp$X > modis_files$xmax) |
          all(tmp$X < modis_files$xmin) |
          all(tmp$Y > modis_files$ymax) |
          all(tmp$Y < modis_files$ymin)
      ) {
        warning(
          "[MODIS Landcover Extraction] site ",
          i,
          " falls outside of the spatial extent of the MODIS files provided.",
          " No value will be assigned.",
          call. = FALSE
        )

        out_of_range <- c(out_of_range, i)
      } else {
        # Match to appropriate file, using either the nearest year covered by
        # MODIS if the data's year is outside MODIS coverage, or the data's
        # year, and the site's coordinates.
        suppressWarnings(
          if (!(j %in% modis_files$year)) {
            modis_match[
              modis_match$SurveyAreaIdentifier == i &
                modis_match$survey_year == j,
              "filename"
            ] <- modis_files$filename[
              modis_files$year ==
                unique(modis_files$year)[which(
                  abs(j - unique(modis_files$year)) ==
                    abs(min(j - unique(modis_files$year)))
                )] &
                modis_files$xmin < tmp$X &
                modis_files$xmax > tmp$X &
                modis_files$ymin < tmp$Y &
                modis_files$ymax > tmp$Y
            ]
          } else {
            modis_match[
              modis_match$SurveyAreaIdentifier == i &
                modis_match$survey_year == j,
              "filename"
            ] <- modis_files$filename[
              modis_files$year == tmp$survey_year &
                modis_files$xmin < tmp$X &
                modis_files$xmax > tmp$X &
                modis_files$ymin < tmp$Y &
                modis_files$ymax > tmp$Y
            ]
          }
        )
      }
    }

    rm(tmp)
  }

  # Create object with parseable names for MODIS classes. Transcribed from
  # documentation at
  # https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf where
  # class definitions are also available. NOTE: might be worth transcribing
  # these into an object within NatureCounts.
  modis_classes <- list(
    modis_lctype1 = data.frame(
      class = c(1:17, 255),
      name = c(
        "evergreen_needleleaf_forests",
        "evergreen_broadleaf_forests",
        "decidious_needleleaf_forests",
        "deciduous_broadleaf_forests",
        "mixed_forests",
        "closed_shrublands",
        "open_shrublands",
        "woody_savannas",
        "savannas",
        "grasslands",
        "permanent_wetlands",
        "croplands",
        "urban_builtup_lands",
        "cropland_natural_vegetation_mosaic",
        "permanent_snow_ice",
        "barren",
        "water_bodies",
        "unclassified"
      )
    ),
    modis_lctype2 = data.frame(
      class = c(0:15, 255),
      name = c(
        "water_bodies",
        "evergreen_needleleaf_forests",
        "evergreen_broadleaf_forests",
        "deciduous_needleleaf_forests",
        "deciduous_broadleaf_forests",
        "mixed_forests",
        "closed_shrublands",
        "open_shrublands",
        "woody_savannas",
        "savannas",
        "grasslands",
        "permanent_wetlands",
        "croplands",
        "urban_builtup_lands",
        "cropland_natural_vegetation_mosaic",
        "nonvegetated_lands",
        "unclassified"
      )
    ),
    modis_lctype3 = data.frame(
      class = c(0:10, 255),
      name = c(
        "water_bodies",
        "grasslands",
        "shrublands",
        "broadleaf_croplands",
        "savannas",
        "evergreen_broadleaf_forests",
        "deciduous_broadleaf_forests",
        "evergreen_needleleaf_forests",
        "deciduous_needleleaf_forests",
        "nonvegetated_lands",
        "urban_builtup_lands",
        "unclassified"
      )
    ),
    modis_lctype4 = data.frame(
      class = c(0:8, 255),
      name = c(
        "water_bodies",
        "evergreen_needleleaf_vegetation",
        "evergreen_broadleaf_vegetation",
        "deciduous_needleleaf_vegetation",
        "deciduous_broadleaf_vegetation",
        "annual_broadleaf_vegetation",
        "annual_grass_vegetation",
        "nonvegetated_lands",
        "urban_builtup_lands",
        "unclassified"
      )
    ),
    modis_lctype5 = data.frame(
      class = c(0:11, 255),
      name = c(
        "water_bodies",
        "evergreen_needleleaf_trees",
        "evergreen_broadleaf_trees",
        "deciduous_needleleaf_trees",
        "deciduous_broadleaf_trees",
        "shrub",
        "grass",
        "cereal_croplands",
        "broadleaf_croplands",
        "urban_builtup_lands",
        "permanent_snow_ice",
        "barren",
        "unclassified"
      )
    )
  )

  # Open loop going through each requested land cover classification and
  # extracting.
  for (i in grep("modis_lc", covariates, value = TRUE)) {
    # Parse covariate name for layer name used by MODIS data files.
    index <- gsub("modis_lct", "LC_T", i)

    message(paste0(
      "[MODIS Landcover Extraction] calculating MODIS ",
      gsub("_", " ", index),
      "."
    ))

    # Loop through each matched MODIS data file.
    for (j in stats::na.omit(unique(modis_match$filename))) {
      # Create object with all sites that matched to file j.
      pts_to_fill <- data[
        data$SurveyAreaIdentifier %in%
          modis_match$SurveyAreaIdentifier[modis_match$filename == j],
      ]

      # Open the requested layer in file j.
      modis <- terra::rast(j)[index]

      # Loop through each site matched to file j and extract.
      for (k in unique(pts_to_fill$SurveyAreaIdentifier)) {
        # If buffered, extract using landscapemetrics::calculate_lsm(). If not,
        # extract using terra::extract().
        if (buffered == TRUE) {
          # Create temporary object containing only the buffer for site k.
          tmp <- data %>%
            dplyr::filter(SurveyAreaIdentifier == k) %>%
            dplyr::select(SurveyAreaIdentifier, geometry) %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(modis)) %>%
            terra::vect()

          # Crop MODIS data file to site k's buffer.
          modis_clip <- terra::crop(modis, tmp)

          # Use landscapemetrics::calculate_lsm() to calculate the proportion
          # of each land cover type present in the cropped raster ("pland").
          modis_pland <- landscapemetrics::calculate_lsm(
            modis_clip,
            metric = "pland"
          )

          # Loop through each land cover type present in the cropped raster
          # and append proportion at site k in the appropriate year to input
          # data. Create parseable column names using names for each
          # class listed above.
          for (l in modis_pland$class) {
            data[
              data$SurveyAreaIdentifier == k &
                data$survey_year %in%
                  modis_match$survey_year[modis_match$filename == j],
              paste0(
                index,
                "_",
                modis_classes[[i]]$name[modis_classes[[i]]$class == l]
              )
            ] <- modis_pland$value[modis_pland$class == l]
          }

          # Check whether any land cover classes were never in the cropped
          # raster. These are true zeros, but would be left out otherwise.
          # Add these columns in with 0 values.
          missing_cols <- paste0(index, "_", modis_classes[[i]]$name)[
            !(paste0(index, "_", modis_classes[[i]]$name) %in% names(data))
          ]

          for (l in missing_cols) {
            data[, l] <- 0
          }

          # Replace NAs present in columns for land cover classes that were
          # found at some sites but not others with the true zeros they
          # represent.
          for (l in paste0(index, "_", modis_classes[[i]]$name)) {
            data[
              is.na(data[, l] %>% sf::st_drop_geometry()) &
                !(data$SurveyAreaIdentifier %in% out_of_range),
              l
            ] <- 0
          }

          # Reorder columns to match class order provided in MODIS
          # documentation.
          data <- data[, c(
            grep(index, names(data), value = TRUE, invert = TRUE),
            paste0(index, "_", modis_classes[[i]]$name)
          )]
        } else {
          # Create temporary object containing only the point for site k.
          tmp <- data %>%
            filter(SurveyAreaIdentifier == k) %>%
            select(SurveyAreaIdentifier, geometry) %>%
            distinct() %>%
            sf::st_transform(terra::crs(modis))

          # Extract point value from MODIS raster. It appears to be possible
          # that a point falls such that it extracts from two raster tiles,
          # so handle that possibility below.
          extr_table <- terra::extract(modis, tmp, fun = unique)[, index]

          # Whether only a single value was extracted (class == "integer") or
          # multiple values (else) prepare to pass to input data.
          if (class(extr_table) == "integer") {
            extr_table <- extr_table %>%
              as.data.frame()

            names(extr_table) <- "class"

            extr_table <- dplyr::left_join(
              extr_table,
              modis_classes[[i]],
              by = "class"
            )
          } else {
            extr_table <- extr_table %>%
              as.data.frame() %>%
              dplyr::select(dplyr::all_of(index))

            names(extr_table) <- "class"

            extr_table <- dplyr::left_join(
              extr_table,
              modis_classes[[i]],
              by = "class"
            )
          }

          # Join extracted value to input data. If multiple values were
          # extracted, join the first value in extr_table and warn the user
          # about potential values so they can adjust manually.
          tryCatch(
            data[
              data$SurveyAreaIdentifier == k &
                data$survey_year %in%
                  modis_match$survey_year[modis_match$filename == j],
              paste0(index, "_Class")
            ] <- modis_classes[[i]]$name[
              modis_classes[[i]]$class ==
                terra::extract(modis, tmp, fun = unique)[, index]
            ],
            warning = function(w) {
              if (
                conditionMessage(w) ==
                  paste0(
                    "longer object length is not a multiple of shorter",
                    "object length"
                  )
              ) {
                warning(
                  paste0(
                    "[MODIS Landcover Extraction] MODIS ",
                    index,
                    ": Site ",
                    k,
                    " in year(s) ",
                    stringr::str_flatten_comma(sort(unique(modis_match$survey_year[
                      modis_match$filename == j
                    ]))),
                    " touches multiple cells. Extraction returned `",
                    suppressWarnings(modis_classes[[i]]$name[
                      modis_classes[[i]]$class ==
                        terra::extract(modis, tmp, fun = unique)[, index]
                    ]),
                    "` but possible values were `",
                    stringr::str_flatten(extr_table$name, collapse = "`, `"),
                    "`. Please examine to choose desired output and replace if",
                    " necessary."
                  ),
                  call. = FALSE
                )
              } else {
                warning(conditionMessage(w))
              }
            }
          )
        }
      }
    }
  }

  # Check if attributes were found and stored from input data. If they were
  # found reattach.
  if (exists("attrs")) {
    # Reattach attributes
    attributes(data)[names(attrs)] <- attrs
  }

  # Reinstate user's specified column names.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
  }

  if (!is.null(date_year)) {
    names(data)[names(data) == "survey_year"] <- date_year
  }

  # If requested, remove MODIS data files.
  if (retain == FALSE) {
    message(paste0(
      "[MODIS Landcover Extraction] extraction complete. Removing files."
    ))

    file.remove(modis_files$filename)
  }

  # Return input data with appended land cover columns.
  return(data)
}

########################### VEGETATION FUNCTIONS ###############################

# Function for downloading MODIS MOD13A1 data from NASA EarthData. Wrapper for
# luna::getNASA().
vegetation_download <- function(
  data,
  ed_email, # users' EarthData account email address.
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'.
  dl_path = NULL # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'modis/' in the working
  # directory.
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra",
    "luna"
  ))

  # Check that an EarthData account email has been provided. If not, return
  # error.
  if (missing(ed_email)) {
    stop(
      "[MODIS NDVI/EVI Download] MODIS data requested but Earthdata system",
      " login information not supplied. Please register at",
      " https://urs.earthdata.nasa.gov/users/new and supply using `ed_email`",
      " argument.",
      call. = FALSE
    )
  }

  # Check whether an EarthData password exists in the environment (is specified
  # earlier in the nc_covariates() workflow), and if not, request using
  # askpass::askpass().
  if (is.null(parent.frame()$ed_password)) {
    ed_password <- askpass::askpass(
      prompt = paste0(
        "Please enter password for ",
        "EarthData user '",
        ed_email,
        "'."
      )
    )
  } else {
    ed_password <- parent.frame()$ed_password
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[MODIS NDVI/EVI Download] downloading requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(data, "date_year"))) {
    date_year <- attr(data, "date_year")
  }

  if (is.null(date_month) & !is.null(attr(data, "date_month"))) {
    date_month <- attr(data, "date_month")
  }

  if (is.null(date_day) & !is.null(attr(data, "date_day"))) {
    date_day <- attr(data, "date_day")
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name, date_year, date_month, date_day)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      (!("SurveyAreaIdentifier" %in% data_cols) |
        !("survey_year" %in% data_cols) |
        !("survey_month" %in% data_cols) |
        !("survey_day" %in% data_cols))
  ) {
    stop(
      "[MODIS NDVI/EVI Download] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that",
      " diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_year) & !("survey_year" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  if (!is.null(date_month)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }

  # Use month_check() to validate month data.
  month_corr <- c()

  for (i in 1:length(data$survey_month)) {
    month_corr[i] <- month_check(data$survey_month[i])
  }

  data$survey_month <- month_corr

  if (!is.null(date_day)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_day" = !!date_day)
  }

  # Use dom_check() to validate day data.
  for (i in data$survey_day) {
    dom_check(i)
  }

  # Create area of interest polygon from provided sf object.
  if (input_fmt$type == "sf") {
    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_transform("ESRI:102001") %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    } else {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_buffer(20000) %>% # # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    }
  }

  # Create area of interest polygon from provided terra object.
  if (input_fmt$type == "terra") {
    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::project("ESRI:102001") %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    } else {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    }

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data) # Maybe down the line write full process out in
    # terra for terra data.
  }

  # Remove any observations missing year, month, or day data. Warn.
  if (
    TRUE %in%
      is.na(data$survey_year) |
      TRUE %in% is.na(data$survey_month) |
      TRUE %in% is.na(data$survey_day)
  ) {
    warning(
      "[MODIS NDVI/EVI Download] missing date data detected. Complete year,",
      " month, and day data is needed for data download. Observations missing",
      " date data will be dropped.",
      call. = FALSE
    )

    data <- data %>%
      dplyr::filter(
        !is.na(survey_year),
        !is.na(survey_month),
        !is.na(survey_day)
      )
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./modis/MOD13A1")) {
    dir.create("./modis/MOD13A1", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/modis/MOD13A1"))) {
    dir.create(paste0(dl_path, "/modis/MOD13A1"), recursive = TRUE)
  }

  # In first iteration of loop, fetch number of files to download to warn
  # user as the 16-day resolution of this data can result in large file batches
  # by setting download = FALSE in luna::getNASA().
  for (i in c(FALSE, TRUE)) {
    # Open list to store filenames.
    modis_files <- list()

    # Make API call using luna::getNASA() fetching all data between the
    # first day of the first month surveyed in the first survey year to the
    # last day of the last month surveyed in the first survey year. If data for
    # that year is not available, skip and warn.
    tryCatch(
      modis_files[[as.character(min(data$survey_year))]] <- luna::getNASA(
        product = "MOD13A1",
        start = paste0(
          min(data$survey_year),
          "-",
          min(data$survey_month[data$survey_year == min(data$survey_year)]),
          "-01"
        ),
        end = paste0(
          min(data$survey_year),
          "-",
          max(data$survey_month[data$survey_year == min(data$survey_year)]),
          ifelse(
            max(data$survey_month[
              data$survey_year == min(data$survey_year)
            ]) %in%
              c(1, 3, 5, 7, 8, 10, 12),
            "-31",
            ifelse(
              max(data$survey_month[
                data$survey_year == min(data$survey_year)
              ]) %in%
                c(4, 6, 9, 11),
              "-30",
              ifelse(
                lubridate::leap_year(min(data$survey_year)),
                "-29",
                "-28"
              )
            )
          )
        ),
        aoi = terra::project(study_area, "epsg:4326"),
        download = i,
        overwrite = FALSE,
        path = ifelse(
          is.null(dl_path),
          "./modis/MOD13A1",
          paste0(dl_path, "/modis/MCD12Q1")
        ),
        username = ed_email,
        password = ed_password
      ),
      warning = function(w) {
        if (conditionMessage(w) == "No results found") {
          if (!i) {
            warning(
              paste0(
                "[MODIS NDVI/EVI Download] no data found for year ",
                min(data$survey_year),
                ". Is it outside of the temporal coverage of this dataset?"
              ),
              call. = FALSE
            )
          }
        } else {
          warning(conditionMessage(w))
        }
      }
    )

    # Loop through remaining survey years and repeat process.
    for (j in sort(unique(data$survey_year))[
      2:length(unique(data$survey_year))
    ]) {
      tryCatch(
        modis_files[[as.character(j)]] <- luna::getNASA(
          product = "MOD13A1",
          start = paste0(
            j,
            "-",
            min(data$survey_month[data$survey_year == j]),
            "-01"
          ),
          end = paste0(
            j,
            "-",
            max(data$survey_month[data$survey_year == j]),
            ifelse(
              max(data$survey_month[data$survey_year == j]) %in%
                c(1, 3, 5, 7, 8, 10, 12),
              "-31",
              ifelse(
                max(data$survey_month[data$survey_year == j]) %in%
                  c(4, 6, 9, 11),
                "-30",
                ifelse(
                  lubridate::leap_year(j) &
                    max(data$survey_month[data$survey_year == j]) == 2,
                  "-29",
                  "-28"
                )
              )
            )
          ),
          aoi = terra::project(study_area, "epsg:4326"),
          download = i,
          overwrite = FALSE,
          path = ifelse(
            is.null(dl_path),
            "./modis/MOD13A1",
            paste0(dl_path, "/modis/MCD12Q1")
          ),
          username = ed_email,
          password = ed_password
        ),
        warning = function(w) {
          if (conditionMessage(w) == "No results found") {
            if (!i) {
              warning(
                paste0(
                  "[MODIS NDVI/EVI Download] no data found for year ",
                  j,
                  ". Is it outside of the temporal coverage of this dataset?"
                ),
                call. = FALSE
              )
            }
          } else {
            warning(conditionMessage(w))
          }
        }
      )
    }

    # Convert list to a flat vector.
    modis_files <- unlist(modis_files, use.names = FALSE)

    # On first iteration send message about expected number of files to
    # download.
    if (i == FALSE) {
      message(paste0(
        "[MODIS NDVI/EVI Download] data products are at a 16 day resolution, resulting in ",
        length(modis_files),
        " files to download for your data. This may take some time."
      ))
    }
  }

  # Return character vector of filepaths to downloaded files.
  return(modis_files)
}


# Function to extract vegetation data from provided MODIS MOD13A1 data files.
vegetation_extract <- function(
  data,
  covariates = "modis_ndvi", # Other options listed in nc_covariate_table().
  vegetation_files, # Character vector of filepaths to downloaded files.
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  retain = TRUE # Should data files be kept after extraction?
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "luna",
    "terra"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[MODIS NDVI/EVI Extraction] covariates either not listed or one or more are invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no vegetation files are provided, return error.
  if (missing(vegetation_files)) {
    stop(
      "[MODIS NDVI/EVI Extraction] no vegetation files provided to extract from. Please provide a vector containing filepaths of all necessary MODIS files for your data. Data can be downloaded using landcover_download.",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[MODIS NDVI/EVI Extraction] extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Store attributes so they don't get lost.

  # List potential attributes.
  attr_names <- c(
    "site_name",
    "coord_lon",
    "coord_lat",
    "date_year",
    "date_month",
    "date_day",
    "date_ordinal",
    "date_lubridate",
    "crs"
  )

  # If any potential attribute names are present in the data attributes,
  # store.
  if (length(attr_names[attr_names %in% names(attributes(data))]) > 0) {
    attrs <- attributes(data)[attr_names[
      attr_names %in% names(attributes(data))
    ]]
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(data, "date_year"))) {
    date_year <- attr(data, "date_year")
  }

  if (is.null(date_month) & !is.null(attr(data, "date_month"))) {
    date_month <- attr(data, "date_month")
  }

  if (is.null(date_day) & !is.null(attr(data, "date_day"))) {
    date_day <- attr(data, "date_day")
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name, date_year, date_month, date_day)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      (!("SurveyAreaIdentifier" %in% data_cols) |
        !("survey_year" %in% data_cols) |
        !("survey_month" %in% data_cols) |
        !("survey_day" %in% data_cols))
  ) {
    stop(
      "[MODIS NDVI/EVI Extraction] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_year) & !("survey_year" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  if (!is.null(date_month) & !("survey_month" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }

  month_corr <- c()

  for (i in 1:length(data$survey_month)) {
    month_corr[i] <- month_check(data$survey_month[i])
  }

  # Use month_check() to validate month data.
  data$survey_month <- month_corr

  data$survey_month <- as.numeric(data$survey_month)

  if (!is.null(date_day) & !("survey_day" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_day" = !!date_day)
  }

  # Use dom_check() to validate day data.
  for (i in data$survey_day) {
    dom_check(i)
  }

  data$survey_day <- as.numeric(data$survey_day)

  # Check whether sf object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "sf") {
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
  }

  # Check whether terra object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "terra") {
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data) # Maybe down the line write full process out in
    # terra for terra data.
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("exactextractr")
  }

  # Remove any observations missing year, month, or day data.
  if (
    TRUE %in%
      is.na(data$survey_year) |
      TRUE %in% is.na(data$survey_month) |
      TRUE %in% is.na(data$survey_day)
  ) {
    warning(
      "[MODIS NDVI/EVI Extraction] Missing date data detected. Complete year,",
      " month, and day data is needed for extraction. Observations missing",
      " date data will be dropped.",
      call. = FALSE
    )

    data <- data %>%
      dplyr::filter(
        !is.na(survey_year),
        !is.na(survey_month),
        !is.na(survey_day)
      )
  }

  # Parse dates stored in filenames of MODIS data files and append column to
  # filenames.
  modis_files <- luna::modisDate(vegetation_files)

  # As each files contains data covering a 16 day period, create an end date of
  # each files coverage.
  modis_files$enddate <- modis_files$date + 16

  modis_files$year <- as.numeric(modis_files$year)
  modis_files$month <- as.numeric(modis_files$month)
  modis_files$day <- as.numeric(modis_files$day)

  modis_files$endyear <- lubridate::year(modis_files$enddate)
  modis_files$endmonth <- lubridate::month(modis_files$enddate)
  modis_files$endday <- lubridate::day(modis_files$enddate)

  modis_files$yday <- lubridate::yday(modis_files$date)
  modis_files$endyday <- lubridate::yday(modis_files$enddate)

  # Function for quick conversion of ordinal dates.
  yearyearday <- function(yr, yd) {
    base <- as.Date(paste0(yr, "-01-01")) # take Jan 1 of year
    day <- base + yd - 1
  }

  # Some date windows have multiple files produced at different times. Extract
  # and store production dates so we can select between these files later.
  modis_files$productiondate <- yearyearday(
    as.numeric(substr(modis_files$filename, 61 - 16, 61 - 13)),
    as.numeric(substr(modis_files$filename, 61 - 12, 61 - 10))
  ) +
    lubridate::hms(paste0(
      substr(modis_files$filename, 61 - 9, 61 - 8),
      ":",
      substr(modis_files$filename, 61 - 7, 61 - 6),
      ":",
      substr(modis_files$filename, 61 - 5, 61 - 4)
    )) # So long as date format in files stays consistent, this should work
  # fine.

  # Extract and bind spatial extent of each data file.
  modis_files <- cbind(
    modis_files,
    as.data.frame(luna::modisExtent(modis_files$filename))
  )

  # Build object to use in matching sites to their respective MODIS data file.
  modis_match <- data %>%
    dplyr::mutate(
      date = as.Date(paste0(survey_year, "-", survey_month, "-", survey_day))
    ) %>%
    dplyr::mutate(yday = lubridate::yday(date)) %>%
    dplyr::select(SurveyAreaIdentifier, survey_year, yday, geometry) %>%
    sf::st_transform(terra::crs(terra::rast(modis_files$filename[1])))

  # If buffered, extract coordinates from centroids. Append coordinates.
  if (buffered == TRUE) {
    suppressWarnings(
      modis_match <- cbind(
        modis_match,
        sf::st_coordinates(sf::st_centroid(modis_match))
      )
    )
  } else {
    modis_match <- cbind(modis_match, sf::st_coordinates(modis_match))
  }

  # Open vectors to store site/date information for sites/dates that are unable
  # to be matched to a data file.
  warning_sites <- c()
  warning_years <- c()
  warning_dates <- c()

  # Loop through each site-date combination and match to a data file.
  for (i in unique(modis_match$SurveyAreaIdentifier)) {
    for (j in unique(modis_match$survey_year[
      modis_match$SurveyAreaIdentifier == i
    ])) {
      for (k in unique(modis_match$yday[
        modis_match$SurveyAreaIdentifier == i & modis_match$survey_year == j
      ])) {
        # Create temporary object containing only data for site i on day k
        # of year j.
        tmp <- dplyr::filter(
          modis_match,
          SurveyAreaIdentifier == i,
          survey_year == j,
          yday == k
        )

        # Check to see whether the site-date combination can be matched to a
        # data file.
        if (
          nrow(modis_files[
            modis_files$year == tmp$survey_year &
              modis_files$xmin < tmp$X &
              modis_files$xmax > tmp$X &
              modis_files$ymin < tmp$Y &
              modis_files$ymax > tmp$Y &
              modis_files$yday <= tmp$yday &
              modis_files$endyday > tmp$yday,
          ]) ==
            0
        ) {
          # Do the coordinates fall within the area covered by any of
          # the data files?
          spatial_check <- ifelse(
            nrow(modis_files[
              modis_files$xmin < tmp$X &
                modis_files$xmax > tmp$X &
                modis_files$ymin < tmp$Y &
                modis_files$ymax > tmp$Y,
            ]) >
              0,
            TRUE,
            FALSE
          )

          # Does data exist for the data's year?
          year_check <- ifelse(
            nrow(modis_files[modis_files$year == tmp$survey_year, ]) > 0,
            TRUE,
            FALSE
          )

          # Does the date fall within the date windows covered by any of the
          # data files?
          yday_check <- ifelse(
            nrow(modis_files[
              modis_files$yday <= tmp$yday & modis_files$endyday > tmp$yday,
            ]) >
              0,
            TRUE,
            FALSE
          )

          # If any checks not passed, store for later warning message.
          if (!(spatial_check)) {
            warning_sites <- c(warning_sites, i)
          }

          if (!(year_check)) {
            warning_years <- c(warning_years, j)
          }

          # Only warn about date if the data is within the spatial extent of
          # the provided MODIS data and is in a year covered by the data.
          if (spatial_check & year_check & !(yday_check)) {
            warning_dates <- c(warning_dates, yearyearday(j, k))
          }
        } else {
          # If no issues with coverage, match site-date combinations to
          # respective files.

          # List all files that match the location and date.
          suppressWarnings(
            {
              poss_files <- modis_files[
                modis_files$year == tmp$survey_year &
                  modis_files$xmin < tmp$X &
                  modis_files$xmax > tmp$X &
                  modis_files$ymin < tmp$Y &
                  modis_files$ymax > tmp$Y &
                  modis_files$yday <= tmp$yday &
                  modis_files$endyday > tmp$yday,
              ]

              # Pick the most recently produced file.
              modis_match[
                modis_match$SurveyAreaIdentifier == i &
                  modis_match$survey_year == j &
                  modis_match$yday == k,
                "filename"
              ] <- poss_files$filename[
                poss_files$productiondate == max(poss_files$productiondate)
              ]
            }
          )
        }
      }
    }
  }

  # Order sites and dates for warning message.
  warning_sites <- sort(unique(warning_sites))
  warning_years <- sort(unique(warning_years))
  warning_dates <- sort(unique(warning_dates))

  # Warn about sites that fall outside of the spatial extent of the provided
  # MODIS data.
  if (length(warning_sites) > 0) {
    if (length(warning_sites) == 1) {
      warning(
        "[MODIS NDVI/EVI Extraction] site ",
        stringr::str_flatten_comma(unique(warning_sites)),
        " falls outside of the spatial extent of the files provided. No value",
        " will be returned.",
        call. = FALSE
      )
    } else {
      warning(
        "[MODIS NDVI/EVI Extraction] sites ",
        stringr::str_flatten_comma(unique(warning_sites)),
        " fall outside of the spatial extent of the files provided. No value",
        " will be returned.",
        call. = FALSE
      )
    }
  }

  # Warn about observations in years that fall outside of the temporal coverage
  # of the provided MODIS data.
  if (length(warning_years) > 0) {
    if (length(warning_years) == 1) {
      warning(
        "[MODIS NDVI/EVI Extraction] observations from year ",
        stringr::str_flatten_comma(unique(warning_years)),
        " fall outside of the temporal extent of the files provided. Is it in",
        " a year where data is unavailable from this dataset? No value will",
        " be returned.",
        call. = FALSE
      )
    } else {
      warning(
        "[MODIS NDVI/EVI Extraction] observations from years ",
        stringr::str_flatten_comma(unique(warning_years)),
        " fall outside of the temporal extent of the files provided. Is it in",
        " a year where data is unavailable from this dataset? No value will be",
        " returned.",
        call. = FALSE
      )
    }
  }

  # Warn about observations on dates that fall outside of the temporal coverage
  # of the provided MODIS data.
  if (length(warning_dates) > 0) {
    warning(
      "[MODIS NDVI/EVI Extraction] observations on ",
      stringr::str_flatten_comma(unique(warning_dates)),
      " fall outside of the temporal extent of the files provided. You have",
      " provided data for this year but not this 16-day window. No value will",
      " be returned.",
      call. = FALSE
    )
  }

  # Remove observations without matches.
  modis_match <- dplyr::filter(modis_match, !is.na(filename))

  # Create an ordinal date column in original data for later joining.
  data$yday <- paste0(
    data$survey_year,
    "-",
    data$survey_month,
    "-",
    data$survey_day
  ) %>%
    as.Date() %>%
    lubridate::yday()

  # Loop through each requested vegetation metric, extract, and join to original
  # data.
  for (i in `if`(
    "modis_ndvi" %in% covariates,
    `if`(
      "modis_evi" %in% covariates,
      c("modis_ndvi", "modis_evi"),
      "modis_ndvi"
    ),
    "modis_evi"
  )) {
    message(paste0(
      "[MODIS NDVI/EVI Extraction] calculating MODIS ",
      ifelse(i == "modis_ndvi", "NDVI", "EVI"),
      "."
    ))

    # Create index to access appropriate data layer from MODIS rasters.
    index <- ifelse(
      i == "modis_ndvi",
      "\"500m 16 days NDVI\"",
      "\"500m 16 days EVI\""
    )

    # Loop through each matched MODIS data file.
    for (j in unique(modis_match$filename)) {
      # Create object with all site-date combinations that matched to file j.
      pts_to_fill <- data[
        data$SurveyAreaIdentifier %in%
          modis_match$SurveyAreaIdentifier[modis_match$filename == j] &
          data$survey_year %in%
            modis_match$survey_year[modis_match$filename == j] &
          data$yday %in% modis_match$yday[modis_match$filename == j],
      ]

      # Open the requested layer in file j.
      modis <- terra::rast(j)[index]

      # Loop through each site matched to file j and extract.
      for (k in unique(pts_to_fill$SurveyAreaIdentifier)) {
        # If buffered, extract using exactextractr::exact_extract(). If not,
        # extract using terra::extract().
        if (buffered == TRUE) {
          # Create temporary object containing only the buffer for site k.
          tmp <- data %>%
            dplyr::filter(
              SurveyAreaIdentifier == k,
              survey_year %in%
                modis_match$survey_year[modis_match$filename == j]
            ) %>%
            dplyr::select(SurveyAreaIdentifier, geometry) %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(modis))

          # Crop MODIS data file to site k's buffer.
          modis_clip <- terra::crop(modis, tmp)

          # Extract using exactextractr::exact_extract().
          data[
            data$SurveyAreaIdentifier == k &
              data$survey_year ==
                modis_match$survey_year[
                  modis_match$filename == j &
                    modis_match$SurveyAreaIdentifier == k
                ] &
              data$yday %in%
                modis_match$yday[
                  modis_match$filename == j &
                    modis_match$SurveyAreaIdentifier == k
                ],
            ifelse(i == "modis_ndvi", "ndvi", "evi")
          ] <- exactextractr::exact_extract(modis_clip, tmp, fun = "mean")
        } else {
          # Create temporary object containing only the point for site k.
          tmp <- data %>%
            dplyr::filter(
              SurveyAreaIdentifier == k,
              survey_year %in%
                modis_match$survey_year[modis_match$filename == j]
            ) %>%
            dplyr::select(SurveyAreaIdentifier, geometry) %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(modis)) %>%
            terra::vect()

          # Extract using terra::extract().
          data[
            data$SurveyAreaIdentifier == k &
              data$survey_year ==
                modis_match$survey_year[
                  modis_match$filename == j &
                    modis_match$SurveyAreaIdentifier == k
                ] &
              data$yday %in%
                modis_match$yday[
                  modis_match$filename == j &
                    modis_match$SurveyAreaIdentifier == k
                ],
            ifelse(i == "modis_ndvi", "ndvi", "evi")
          ] <- terra::extract(modis, tmp, fun = "mean")[, index]
        }
      }
    }
  }

  # Remove ordinal date column from original data.
  data <- dplyr::select(data, -yday)

  # Check if attributes were found and stored from input data. If they were
  # found reattach.
  if (exists("attrs")) {
    # Reattach attributes

    attributes(data)[names(attrs)] <- attrs
  }

  # Reinstate user's specified column names.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
  }

  if (!is.null(date_year)) {
    names(data)[names(data) == "survey_year"] <- date_year
  }

  if (!is.null(date_month)) {
    names(data)[names(data) == "survey_month"] <- date_month
  }

  if (!is.null(date_day)) {
    names(data)[names(data) == "survey_day"] <- date_day
  }

  # If requested, remove MODIS data files.
  if (retain == FALSE) {
    message(paste0(
      "[MODIS NDVI/EVI Extraction] task complete. Removing files."
    ))

    file.remove(modis_files$filename)
  }

  # Return input data with appended vegetation columns.
  return(data)
}


############################ ELEVATION FUNCTIONS ###############################

# Function to download elevation data from Terrain Tiles. Wrapper for
# elevatr::get_elev_raster().
elevation_download <- function(
  data,
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  z = 7, # determines zoom for downloaded elevation data. For more information
  # see https://github.com/tilezen/joerd/blob/master/docs/data-sources.md.
  src = "aws" # Source for elevation data. "aws" is for Terrain Tiles,
  # but users may be interested in other available sources from OpenTopography.
  # See ??get_elev_raster().
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "elevatr",
    "terra"
  ))

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[Elevation Download] downloading requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  # Check that all specified column names are present in the data.
  specified_cols <- c(site_name)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      !("SurveyAreaIdentifier" %in% data_cols)
  ) {
    stop(
      "[Elevation Download] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  # Check whether sf object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "sf") {
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
  }

  # Check whether terra object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "terra") {
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data)
  }

  message("[Elevation Download] downloading data.")

  # Call to API using elevatr::get_elev_raster() and store in SpatRaster.
  elev <- elevatr::get_elev_raster(
    locations = data,
    z = z,
    prj = sf::st_crs(data),
    src = src,
    neg_to_na = TRUE, # Turn ocean tiles with negative elevation to NAs.
    expand = 20000, # Arbitrarily high number selected (20km).
    # Maybe unnecessary, could reduce download size.
    verbose = FALSE
  ) %>%
    terra::rast()

  # Return SpatRaster of downloaded elevation data.
  return(elev)
}

# Function to extract elevation data from provided elevation SpatRaster.
elevation_extract <- function(
  data,
  elevation_data, # SpatRaster derived from elevatr::get_elev_raster(),
  # downloadable via elevation_download().
  covariates = "elevation", # Only option is elevation.
  site_name = NULL # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[Elevation Extraction] covariates either not listed or one or more are invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no elevation raster is provided, return error.
  if (missing(elevation_data)) {
    stop(
      "[Elevation Extraction] no elevation data provided to extract from. Please provide a terra SpatRaster containing the necessary elevation data. Elevation data can be downloaded using elevation_download.",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[Elevation Extraction] extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Store attributes so they don't get lost.

  # List potential attributes.
  attr_names <- c(
    "site_name",
    "coord_lon",
    "coord_lat",
    "date_year",
    "date_month",
    "date_day",
    "date_ordinal",
    "date_lubridate",
    "crs"
  )

  # If any potential attribute names are present in the data attributes,
  # store.
  attrs <- attributes(data)[attr_names[attr_names %in% names(attributes(data))]]

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  # Check that all specified column names are present in the data.
  specified_cols <- c(site_name)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      !("SurveyAreaIdentifier" %in% data_cols)
  ) {
    stop(
      "[Elevation Extraction] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  # Check whether sf object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "sf") {
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
  }

  # Check whether terra object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "terra") {
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data)
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("exactextractr")
  }

  elev <- elevation_data

  message("[Elevation Extraction] extracting elevation data.")

  # Loop through each site and extract.
  for (i in unique(data$SurveyAreaIdentifier)) {
    # Create temporary object with only point/buffer for site i.
    tmp <- data %>%
      dplyr::filter(SurveyAreaIdentifier == i) %>%
      dplyr::select(SurveyAreaIdentifier, geometry) %>%
      dplyr::distinct()

    # Check if site i falls within the spatial extent of the provided elevation
    # raster. If not, warn. If only partially, warn.
    if (!terra::is.related(elev, terra::vect(tmp), relation = "intersects")) {
      warning(
        "[Elevation Extraction] site ",
        i,
        " falls outside of the spatial extent of the elevation rasters",
        " provided. No value will be returned.",
        call. = FALSE
      )
    } else if (
      terra::is.related(elev, terra::vect(tmp), relation = "intersects") &
        !terra::is.related(elev, terra::vect(tmp), relation = "contains")
    ) {
      warning(
        "[Elevation Extraction] site ",
        i,
        "'s buffered area is only partially contained by the spatial extent of",
        " the elevation rasters provided. Returned mean elevation value will",
        " be derived from the available values.",
        call. = FALSE
      )
    } else {
      # If no issues with coverage, proceed to extract. If buffered, extract
      # using exactextractr::exact_extract(). If not, extract using
      # terra::extract().
      if (buffered == FALSE) {
        data[data$SurveyAreaIdentifier == i, "elevation"] <- terra::extract(
          x = elev,
          y = tmp,
          fun = "mean"
        )[, names(elev)]
      } else {
        data[
          data$SurveyAreaIdentifier == i,
          "elevation"
        ] <- exactextractr::exact_extract(
          x = elev,
          y = tmp,
          fun = "mean",
          progress = FALSE
        )
      }
    }
  }

  # Code to grab nearest raster value for sites outside of raster coverage.
  # Not sure whether to keep this since we are warning users about these sites
  # and saying nothing will be returned. Maybe keep as an option
  # (nearest = TRUE)?
  # if (TRUE %in% is.na(data$elevation)) {
  #   warning(
  #     "[Elevation Extraction] some points are close to shore, and so fall into cells with negative elevation (below sea level). For these cells, the nearest positive elevation has been used.",
  #     call. = FALSE
  #   )
  #
  #   for (i in unique(data$SurveyAreaIdentifier[is.na(data$elevation)])) {
  #     tmp <- data %>%
  #       dplyr::filter(SurveyAreaIdentifier == i) %>%
  #       dplyr::select(SurveyAreaIdentifier, geometry) %>%
  #       dplyr::distinct() %>%
  #       sf::st_buffer(2500)
  #
  #     if (terra::is.related(elev, terra::vect(tmp), relation = "intersects")) {
  #       elev_crop <- terra::crop(elev, vect(tmp)) %>%
  #         terra::as.points()
  #
  #       data$elevation[
  #         data$SurveyAreaIdentifier == i
  #       ] <- terra::values(elev_crop[
  #         terra::nearest(terra::vect(tmp), elev_crop)$to_id
  #       ])
  #     }
  #   }
  # }

  # Check if attributes were found and stored from input data. If they were
  # found reattach.
  if (exists("attrs")) {
    # Reattach attributes

    attributes(data)[names(attrs)] <- attrs
  }

  # Reinstate user's specified column names.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
  }

  # Return input data with appended elevation columns.
  return(data)
}


########################### WORLDCLIM FUNCTIONS ################################

# Function for downloading WorldClim data. Wrapper for
# geodata::worldclim_country().
worldclim_download <- function(
  data,
  covariates = "worldclim_tavg", # Other options listed in nc_covariate_table().
  countries = NULL, # Character vector of country names or ISO3 codes. If left
  # NULL, country will be auto-detected.
  res = 0.5, # resolution of WorldClim data to be downloaded. Options are 10,
  # 5, 2.5, and 0.5 minutes of a degree.
  dl_path = NULL # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'worldclim/' in the working
  # directory.
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "geodata",
    "terra"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[WorldClim Download] covariates either not listed or one or more are",
      " invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./worldclim")) {
    dir.create("./worldclim", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/worldclim"))) {
    dir.create(paste0(dl_path, "/worldclim"), recursive = TRUE)
  }

  # Create index for climate variables from covariate request.
  clim_vars <- gsub(
    pattern = "worldclim_",
    replacement = "",
    grep("worldclim_", covariates, value = TRUE)
  )

  # Unless user specified, attempt to automatically detect the countries for
  # which data must be downloaded.
  if (is.null(countries)) {
    # Check for additional package necessary in this workflow.
    have_pkg_check("spData")

    # Check data is in the desired format.
    input_fmt <- covariate_fmt_check(data)

    # If not an sf or terra object, return error and point towards data_fmt().
    if (input_fmt$type == "data.frame") {
      stop(
        "[WorldClim Download] downloading requires an sf or terra object as",
        " input in this workflow. Consider using `data_fmt` to conform",
        " data first.",
        call. = FALSE
      )
    }

    # For sf input, compare to country data from spData package.
    if (input_fmt$type == "sf") {
      world <- sf::st_read(
        system.file("shapes/world.gpkg", package = "spData"),
        quiet = TRUE
      )

      data <- sf::st_transform(data, sf::st_crs(world))

      world <- sf::st_intersection(world, data)

      countries <- unique(world$name_long)
    }

    # For terra input, convert to sf and  compare to country data from spData
    # package.
    if (input_fmt$type == "terra") {
      data <- sf::st_as_sf(data)

      world <- sf::st_read(
        system.file("shapes/world.gpkg", package = "spData"),
        quiet = TRUE
      )

      data <- sf::st_transform(data, sf::st_crs(world))

      world <- sf::st_intersection(world, data)

      countries <- unique(world$name_long)
    }
  }

  # Open list to store downloaded rasters.
  clim <- list()

  # Loop through each requested WorldClim variable, download.
  for (i in clim_vars) {
    # Loop through each country requested or detected.
    for (j in countries) {
      # Pull country codes table to handle ISO3 codes.
      country_code <- geodata::country_codes()

      # Check if provided country is an ISO3 code, if so, convert.
      if (!(j %in% country_code$ISO3)) {
        country_code <- country_code$ISO3[country_code$NAME == j]
      } else {
        country_code <- j
      }

      # If file doesn't already exist, call geodata::worldclim_country() to
      # download data.
      if (
        !file.exists(ifelse(
          is.null(dl_path),
          paste0(
            "./worldclim/climate/wc2.1_country/",
            country_code,
            "_wc2.1_30s_",
            i,
            ".tif"
          ),
          paste0(
            dl_path,
            "/worldclim/climate/wc2.1_country/",
            country_code,
            "_wc2.1_30s_",
            i,
            ".tif"
          )
        ))
      ) {
        message(
          "[Worldclim Download] downloading WorldClim '",
          i,
          "' data for ",
          j,
          "."
        )

        clim[[i]][[j]] <- geodata::worldclim_country(
          var = i,
          country = j,
          res = res,
          path = ifelse(
            is.null(dl_path),
            "./worldclim",
            paste0(dl_path, "/worldclim")
          )
        )
      } else {
        clim[[i]][[j]] <- terra::rast(ifelse(
          is.null(dl_path),
          paste0(
            "./worldclim/climate/wc2.1_country/",
            country_code,
            "_wc2.1_30s_",
            i,
            ".tif"
          ),
          paste0(
            dl_path,
            "/worldclim/climate/wc2.1_country/",
            country_code,
            "_wc2.1_30s_",
            i,
            ".tif"
          )
        ))
      }
    }

    # Convert each variables different country rasters to a SpatRasterCollection
    # then merge into a single layer for each variable.
    clim[[i]] <- terra::sprc(clim[[i]])

    clim[[i]] <- terra::merge(clim[[i]])
  }

  # Return WorldClim SpatRasters
  return(clim)
}

# Function to extract WorldClim data from provided WorldClim SpatRaster(s).
worldclim_extract <- function(
  data,
  worldclim_data, # named list containing SpatRaster containing
  # WorldClim data, downloadable via WorldClim_download(). Names derived from
  # WorldClim variable names ("tmin", "tmax", "tavg", "prec", "wind", "vapr",
  # "bio").
  covariates = "worldclim_tavg", # Other options listed in nc_covariate_table().
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  dl_path = NULL, # Path to downloaded files. Only needed if retain = TRUE and
  # custom dl_path is used.
  retain = TRUE # Should data files be kept after extraction?
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[WorldClim Extraction] covariates either not listed or one or more are",
      " invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no WorldClim rasters are provided, return error.
  if (missing(worldclim_data)) {
    stop(
      "[WorldClim Extraction] no WorldClim rasters provided to extract from.",
      " Please provide a list of the necessary rasters. Data can be downloaded",
      " using worldclim_download().",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[WorldClim Extraction] downloading requires an sf or terra object as",
      " input in this workflow. Consider using `data_fmt` to conform data",
      " first.",
      call. = FALSE
    )
  }

  # Store attributes so they don't get lost.

  # List potential attributes.
  attr_names <- c(
    "site_name",
    "coord_lon",
    "coord_lat",
    "date_year",
    "date_month",
    "date_day",
    "date_ordinal",
    "date_lubridate",
    "crs"
  )

  # If any potential attribute names are present in the data attributes,
  # store.
  if (length(attr_names[attr_names %in% names(attributes(data))]) > 0) {
    attrs <- attributes(data)[attr_names[
      attr_names %in% names(attributes(data))
    ]]
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  if (is.null(date_month) & !is.null(attr(data, "date_month"))) {
    date_month <- attr(data, "date_month")
  }

  # Check that all specified column names are present in the data.
  specified_cols <- c(site_name, date_month)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      (!("SurveyAreaIdentifier" %in% data_cols) |
        !("survey_month" %in% data_cols))
  ) {
    stop(
      "[WorldClim Extraction] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_month) & !("survey_month" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }

  # Use month_check() to validate month data.
  month_corr <- c()

  for (i in 1:length(data$survey_month)) {
    month_corr[i] <- month_check(data$survey_month[i])
  }

  data$survey_month <- month_corr

  data$survey_month <- as.numeric(data$survey_month)

  # For sf objects, create area of interest to crop WorldClim rasters to to
  # reduce memory load.
  if (input_fmt$type == "sf") {
    # Check whether sf object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)

    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_transform("ESRI:102001") %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    } else {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    }
  }

  # For terra objects, create area of interest to crop WorldClim rasters to to
  # reduce memory load. Convert to sf.
  if (input_fmt$type == "terra") {
    # Check whether terra object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::project("ESRI:102001") %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    } else {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    }

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data)
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("exactextractr")
  }

  clim <- worldclim_data

  # Loop through each requested WorldClim variable.
  for (i in names(worldclim_data)) {
    message("[WorldClim Extraction] extracting WorldClim ", i, ".")

    # Loop through each site and extract.
    for (j in unique(data$SurveyAreaIdentifier)) {
      # Create temporary object with only point/buffer for site i.
      tmp <- data %>%
        dplyr::filter(SurveyAreaIdentifier == j) %>%
        dplyr::select(SurveyAreaIdentifier, survey_month, geometry) %>%
        dplyr::distinct() %>%
        sf::st_transform(terra::crs(clim[[i]]))

      # Loop through each month site i was visited, extract.
      for (k in unique(data$survey_month[data$SurveyAreaIdentifier == j])) {
        # Use variable name and month to pull correct layer from WorldClim
        # raster.
        layername <- paste0(
          substr(
            names(clim[[i]])[1],
            start = 1,
            stop = nchar(names(clim[[i]])[1]) - 1
          ),
          k
        )

        # In the first iteration of the loop, check that the site falls within
        # or is only partially covered by the spatial extent of the provided
        # WorldClim rasters. If not, warn.
        if (
          which(
            unique(data$survey_month[data$SurveyAreaIdentifier == j]) == k
          ) ==
            1
        ) {
          if (
            !terra::is.related(
              clim[[i]],
              terra::vect(tmp),
              relation = "intersects"
            )
          ) {
            warning(
              "[WorldClim (",
              i,
              ") Extraction] site ",
              j,
              " falls outside of the spatial extent of the WorldClim rasters",
              " provided. No value will be returned.",
              call. = FALSE
            )
          } else if (
            terra::is.related(
              clim[[i]],
              terra::vect(tmp),
              relation = "intersects"
            ) &
              !terra::is.related(
                clim[[i]],
                terra::vect(tmp),
                relation = "contains"
              )
          ) {
            warning(
              "[WorldClim (",
              i,
              ") Extraction] site ",
              j,
              "'s buffered area is only partially contained by the spatial",
              " extent of the WorldClim rasters provided. Returned mean ",
              i,
              " value will be derived from the available values.",
              call. = FALSE
            )
          } else {
            # If no issues with coverage, proceed to extract. If buffered,
            # extract using exactextractr::exact_extract(). If not, extract
            # using terra::extract().
            if (buffered == TRUE) {
              data[
                data$SurveyAreaIdentifier == j & data$survey_month == k,
                i
              ] <- exactextractr::exact_extract(
                x = clim[[i]][[layername]],
                y = tmp %>% dplyr::filter(survey_month == k),
                fun = "mean"
              )
            } else {
              data[
                data$SurveyAreaIdentifier == j & data$survey_month == k,
                i
              ] <- terra::extract(
                x = clim[[i]][[layername]],
                y = tmp %>% dplyr::filter(survey_month == k),
                fun = "mean",
                na.rm = TRUE
              )[, layername]
            }
          }
        } else {
          # For all iterations after the first, extract if covered by the
          # WorldClim rasters. Issue no further warnings if not.
          if (
            terra::is.related(
              clim[[i]],
              terra::vect(tmp),
              relation = "intersects"
            )
          ) {
            if (buffered == TRUE) {
              data[
                data$SurveyAreaIdentifier == j & data$survey_month == k,
                i
              ] <- exactextractr::exact_extract(
                x = clim[[i]][[layername]],
                y = tmp %>% dplyr::filter(survey_month == k),
                fun = "mean"
              )
            } else {
              data[
                data$SurveyAreaIdentifier == j & data$survey_month == k,
                i
              ] <- terra::extract(
                x = clim[[i]][[layername]],
                y = tmp %>% dplyr::filter(survey_month == k),
                fun = "mean",
                na.rm = TRUE
              )[, layername]
            }
          }
        }
      }
    }

    # Code to grab nearest raster value for sites outside of raster coverage.
    # Not sure whether to keep this since we are warning users about these sites
    # and saying nothing will be returned. Maybe keep as an option
    # (nearest = TRUE)?
    #   if (TRUE %in% is.na(data[, i])) {
    #     for (j in unique(data$SurveyAreaIdentifier[is.na(data[, i])])) {
    #       for (k in unique(data$survey_month[data$SurveyAreaIdentifier == j])) {
    #         layername <- paste0(
    #           substr(
    #             names(clim[[i]])[1],
    #             start = 1,
    #             stop = nchar(names(clim[[i]])[1]) - 1
    #           ),
    #           k
    #         )
    #
    #         tmp <- data %>%
    #           dplyr::filter(SurveyAreaIdentifier == j) %>%
    #           dplyr::select(SurveyAreaIdentifier, survey_month, geometry) %>%
    #           dplyr::distinct() %>%
    #           sf::st_transform(terra::crs(clim[[i]]))
    #
    #         if (
    #           terra::is.related(
    #             clim[[i]],
    #             terra::vect(tmp),
    #             relation = "intersects"
    #           )
    #         ) {
    #           if (
    #             which(
    #               unique(data$SurveyAreaIdentifier[is.na(
    #                 data$SurveyAreaIdentifier
    #               )]) ==
    #                 j
    #             ) ==
    #               1
    #           ) {
    #             warning(
    #               paste0(
    #                 "[WorldClim (",
    #                 i,
    #                 ") Extraction] some points are close to shore, and so fall outside of raster coverage. For these cells, the nearest cell value has been used."
    #               ),
    #               call. = FALSE
    #             )
    #           }
    #
    #           tmp <- data %>%
    #             dplyr::filter(SurveyAreaIdentifier == j, survey_month == k) %>%
    #             dplyr::select(SurveyAreaIdentifier, survey_month, geometry) %>%
    #             dplyr::distinct() %>%
    #             sf::st_buffer(2500) %>%
    #             sf::st_transform(terra::crs(clim[[i]]))
    #
    #           clim_crop <- terra::crop(
    #             clim[[i]][[layername]],
    #             terra::vect(tmp)
    #           ) %>%
    #             terra::as.points()
    #
    #           data[
    #             data$SurveyAreaIdentifier == j & data$survey_month == k,
    #             i
    #           ] <- terra::values(clim_crop[
    #             terra::nearest(terra::vect(tmp), clim_crop)$to_id
    #           ])
    #         }
    #       }
    #     }
    #   }
  }

  # Check if attributes were found and stored from input data. If they were
  # found reattach.
  if (exists("attrs")) {
    # Reattach attributes

    attributes(data)[names(attrs)] <- attrs
  }

  # Reinstate user's specified column names.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
  }

  if (!is.null(date_month)) {
    names(data)[names(data) == "survey_month"] <- date_month
  }

  # Remove WorldClim files if requested.
  if (retain == FALSE) {
    # Check that if default directory doesn't exist an alterate has been
    # specified.
    if (is.null(dl_path) & !dir.exists("./worldclim")) {
      warning(
        "[WorldClim Extraction] unable to find default WorldClim",
        " directory and no alternate specified using dl_path argument",
        ". No files will be removed.",
        call. = FALSE
      )
    } else {
      message(paste0("[WorldClim Extraction] task complete. Removing files."))

      unlink(
        ifelse(
          is.null(dl_path),
          "./worldclim/climate",
          paste0(dl_path, "/worldclim/climate")
        ),
        recursive = TRUE
      )
    }
  }

  # Return input data with appended WorldClim columns.
  return(data)
}

############################# SCANFI FUNCTIONS #################################

# Function to download data from the Spatialized Canadian National Forest
# Inventory using download.file().
scanfi_download <- function(
  covariates = "scanfi_height", # Other options
  # listed in nc_covariate_table().
  dl_path = NULL # optional argument to provide path
  # to download data to. By default, data is
  # downloaded to a subfolder 'scanfi/' in the
  # working directory.
) {
  # Check packages
  have_pkg_check("terra")

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[SCANFI Download] covariates either not listed or one or more are",
      " invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./scanfi")) {
    dir.create("./scanfi", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/scanfi"))) {
    dir.create(paste0(dl_path, "/scanfi"), recursive = TRUE)
  }

  # Create index for SCANFI variables from requested covariates.
  scanfi_vars <- gsub(
    pattern = "scanfi_",
    replacement = "",
    grep("scanfi_", covariates, value = TRUE)
  )

  # Create table of download links for each SCANFI variable.
  filename <- data.frame(variable = scanfi_vars) %>%
    dplyr::mutate(
      filename = case_when(
        variable ==
          "biomass" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_biomass_SW_2020_v1.2.tif",
        variable ==
          "closure" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_closure_SW_2020_v1.2.tif",
        variable ==
          "height" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_height_SW_2020_v1.2.tif",
        variable ==
          "nfilc" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif",
        variable ==
          "balsamfir" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_balsamFir_SW_2020_v1.2.tif",
        variable ==
          "blackspruce" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_blackSpruce_SW_2020_v1.2.tif",
        variable ==
          "douglasfir" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_douglasFir_SW_2020_v1.2.tif",
        variable ==
          "jackpine" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_jackPine_SW_2020_v1.2.tif",
        variable ==
          "lodgepolepine" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_lodgepolePine_SW_2020_v1.2.tif",
        variable ==
          "ponderosapine" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_ponderosaPine_SW_2020_v1.2.tif",
        variable ==
          "tamarack" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_tamarack_SW_2020_v1.2.tif",
        variable ==
          "whiteredpine" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_whiteRedPine_SW_2020_v1.2.tif",
        variable ==
          "broadleaf" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_prcB_SW_2020_v1.2.tif",
        variable ==
          "otherconifer" ~ "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/SCANFI_sps_prcC_other_SW_2020_v1.2.tif"
      )
    )

  ### SOMETHING TO NOTE: THE SPECIES LEVEL COVERS ARE COVER OF TOTAL CANOPY COVER, SO COVER OF A SP IN A CELL IS SPECIES LEVEL COVER * CANOPY COVER. MESSAGE ABOUT THIS OR BUILD IN?

  # Open list to store SCANFI rasters.
  scanfi <- list()

  # Loop through each requested SCANFI variable and download.
  for (i in scanfi_vars) {
    ### WILL NEED TO CHECK IF DATA IS IN ARCTIC RANGE AND WARN ABOUT NFI LAND
    ### COVER MODELING PROCESS.
    # If file doesn't already exist, download requested variable.
    if (
      !file.exists(ifelse(
        is.null(dl_path),
        paste0(
          "./scanfi/",
          dplyr::last(unlist(stringr::str_split(
            filename$filename[filename$variable == i],
            "/"
          )))
        ),
        paste0(
          dl_path,
          "/scanfi/",
          dplyr::last(unlist(stringr::str_split(
            filename$filename[filename$variable == i],
            "/"
          )))
        )
      ))
    ) {
      message(
        "[SCANFI Download] downloading SCANFI ",
        i,
        ". Files are large and may require a fair bit of download and processing time."
      )

      ### USING METHODS OTHER THAN CURL SEEMS TO CAUSE ISSUES WITH DOWNLOADED FILE - NEED TO CONSIDER CURL COMPATIBILITY WITH OTHER OS'S.

      # tryCatch needed to handle curl issues and redirect users to downloading
      # manually and reading using scanfi_read().
      tryCatch(
        suppressMessages(download.file(
          url = filename$filename[filename$variable == i],
          destfile = ifelse(
            is.null(dl_path),
            paste0(
              "./scanfi/",
              dplyr::last(unlist(stringr::str_split(
                filename$filename[filename$variable == i],
                "/"
              )))
            ),
            paste0(
              dl_path,
              "/scanfi/",
              dplyr::last(unlist(stringr::str_split(
                filename$filename[filename$variable == i],
                "/"
              )))
            )
          ),
          method = "curl"
        )),
        error = function(e) {
          if (conditionMessage(e) == "'curl' call had nonzero exit status") {
            stop(
              "[SCANFI Download] 'curl' call had nonzero exist status. Please download files directly from https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/ and read in using scanfi_read().",
              call. = FALSE
            )
          } else {
            stop(conditionMessage(e), call. = FALSE)
          }
        }
      )

      # Read in downloaded variable and store in list.
      scanfi[[i]] <- terra::rast(ifelse(
        is.null(dl_path),
        paste0(
          "./scanfi/",
          dplyr::last(unlist(stringr::str_split(
            filename$filename[filename$variable == i],
            "/"
          )))
        ),
        paste0(
          dl_path,
          "/scanfi/",
          dplyr::last(unlist(stringr::str_split(
            filename$filename[filename$variable == i],
            "/"
          )))
        )
      ))
    } else {
      scanfi[[i]] <- terra::rast(ifelse(
        is.null(dl_path),
        paste0(
          "./scanfi/",
          dplyr::last(unlist(stringr::str_split(
            filename$filename[filename$variable == i],
            "/"
          )))
        ),
        paste0(
          dl_path,
          "/scanfi/",
          dplyr::last(unlist(stringr::str_split(
            filename$filename[filename$variable == i],
            "/"
          )))
        )
      ))
    }
  }

  # Return list of scanfi rasters.
  return(scanfi)
}

scanfi_read <- function(
  covariates = NULL, # vector of requested SCANFI
  # variables formatted as in nc_covariate_table().
  file = NULL # file path to respective SCANFI data file.
  # Should be in the order variables are listed in the
  # covariates argument.
) {
  # Check packages
  have_pkg_check("terra")

  # Create index for SCANFI variables from requested covariates.
  scanfi_vars <- gsub(
    pattern = "scanfi_",
    replacement = "",
    grep("scanfi_", covariates, value = TRUE)
  )

  # Open list to store SCANFI rasters.
  scanfi <- list()

  # Read using corresponding file path.
  for (i in scanfi_vars) {
    scanfi[[i]] <- terra::rast(file[which(scanfi_vars == i)])
  }

  # Return list of scanfi rasters.
  return(scanfi)
}

scanfi_extract <- function(
  data,
  scanfi_data, # named list containing SpatRaster containing
  # SCANFI data, downloadable via WorldClim_download(). Names derived from
  # SCANFI variables ("height", "biomass", etc.)
  covariates = "scanfi_height", # Other options listed in nc_covariate_table().
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  dl_path = NULL, # Path to downloaded files. Only needed if retain = TRUE and
  # custom dl_path is used.
  retain = TRUE # Should data files be kept after extraction?
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[SCANFI Extraction] covariates either not listed or one or more are",
      " invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no SCANFI rasters are provided, return error.
  if (missing(scanfi_data)) {
    stop(
      "[SCANFI Extraction] no SCANFI rasters provided to extract from. Please",
      " provide a list containing one raster for each listed SCANFI covariate.",
      " Data can be downloaded using scanfi_download.",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[SCANFI Extraction] extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Store attributes so they don't get lost.

  # List potential attributes.
  attr_names <- c(
    "site_name",
    "coord_lon",
    "coord_lat",
    "date_year",
    "date_month",
    "date_day",
    "date_ordinal",
    "date_lubridate",
    "crs"
  )

  # If any potential attribute names are present in the data attributes,
  # store.
  if (length(attr_names[attr_names %in% names(attributes(data))]) > 0) {
    attrs <- attributes(data)[attr_names[
      attr_names %in% names(attributes(data))
    ]]
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      !("SurveyAreaIdentifier" %in% data_cols)
  ) {
    stop(
      "[SCANFI Extraction] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  # For sf objects, create area of interest to crop SCANFI rasters to to
  # reduce memory load.
  if (input_fmt$type == "sf") {
    # Check whether sf object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)

    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_transform("ESRI:102001") %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    } else {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    }
  }

  # For terra objects, create area of interest to crop SCANFI rasters to to
  # reduce memory load.
  if (input_fmt$type == "terra") {
    # Check whether terra object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::project("ESRI:102001") %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    } else {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    }

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data)
  }

  # Fetch index from names of list.
  scanfi_vars <- names(scanfi_data)

  # Loop through each requested SCANFI variable.
  for (i in scanfi_vars) {
    message("[SCANFI Extraction] extracting SCANFI ", i, ".")

    # If buffered, check for packages necessary in buffered workflow.
    if (buffered == TRUE & i == "nfilc") {
      have_pkg_check("landscapemetrics")
    }

    if (buffered == TRUE & !(i == "nfilc")) {
      have_pkg_check("exactextractr")
    }

    # Crop SCANFI data to study area.
    scanfi_data[[i]] <- terra::crop(
      scanfi_data[[i]],
      terra::project(study_area, terra::crs(scanfi_data[[i]]))
    )

    # Loop through each site and extract.
    for (j in unique(data$SurveyAreaIdentifier)) {
      # Create temporary object with only point/buffer for site i.
      tmp <- data %>%
        dplyr::filter(SurveyAreaIdentifier == j) %>%
        dplyr::select(SurveyAreaIdentifier, geometry) %>%
        dplyr::distinct() %>%
        sf::st_transform(terra::crs(scanfi_data[[i]]))

      # Check if the site out of or only partially covered by the spatial
      # extent of the provided SCANFI data. Warn if so.
      if (
        !terra::is.related(
          scanfi_data[[i]],
          terra::vect(tmp),
          relation = "intersects"
        )
      ) {
        warning(
          "[SCANFI (",
          i,
          ") Extraction] site ",
          j,
          " falls outside of the spatial extent of the SCANFI rasters provided.",
          " No value will be returned.",
          call. = FALSE
        )
      } else if (
        terra::is.related(
          scanfi_data[[i]],
          terra::vect(tmp),
          relation = "intersects"
        ) &
          !terra::is.related(
            scanfi_data[[i]],
            terra::vect(tmp),
            relation = "contains"
          )
      ) {
        warning(
          "[SCANFI (",
          i,
          ") Extraction] site ",
          j,
          "'s buffered area is only partially contained by the spatial extent",
          " of the SCANFI rasters provided. Returned ",
          i,
          " value will be derived from the available values.",
          call. = FALSE
        )
      } else {
        # If no issues with coverage, proceed to extract. For NFI Land Cover,
        # extract with landscapemetrics::calculate_lsm() if buffered and with
        # terra::extract() if not. Otherwise, extract with
        # exactextractr::exact_extract() if buffered, and terra::extract()
        # if not.
        if (i == "nfilc") {
          # Create object containing parseable names for NFI Land Cover classes.
          nfilc_classes <- data.frame(
            class = c(1:8),
            name = c(
              "bryoid",
              "herbs",
              "rock",
              "shrub",
              "treed_broadleaf",
              "treed_conifer",
              "treed_mixed",
              "water"
            )
          )

          # If buffered, extract with landscapemetrics::calculate_lsm().
          if (buffered == TRUE) {
            # Convert temporary object to SpatVector to use with terra:crop()
            tmp <- tmp %>%
              terra::vect()

            # Crop SCANFI data to site buffer.
            scanfi_clip <- terra::crop(scanfi_data[[i]], tmp)

            # Use landscapemetrics::calculate_lsm() to calculate the proportion
            # of each land cover type present in the cropped raster ("pland").
            scanfi_pland <- landscapemetrics::calculate_lsm(
              scanfi_clip,
              metric = "pland"
            )

            # Loop through each land cover type present in the cropped raster
            # and append proportion at site k  to input data. Create parseable
            # column names using names for each class listed above.
            for (k in scanfi_pland$class) {
              data[
                data$SurveyAreaIdentifier == j,
                paste0("nfilc_", nfilc_classes$name[nfilc_classes$class == k])
              ] <- scanfi_pland$value[scanfi_pland$class == k]
            }

            # Check whether any land cover classes were never in the cropped
            # raster. These are true zeros, but would be left out otherwise.
            # Add these columns in with 0 values.
            missing_cols <- paste0("nfilc_", nfilc_classes$name)[
              !(paste0("nfilc_", nfilc_classes$name) %in% names(data))
            ]

            for (l in missing_cols) {
              data[, l] <- 0
            }

            # Replace NAs present in columns for land cover classes that were
            # found at some sites but not others with the true zeros they
            # represent.
            for (k in paste0(
              "nfilc_",
              nfilc_classes$name[
                paste0("nfilc_", nfilc_classes$name) %in% names(data)
              ]
            )) {
              data[is.na(data[, k] %>% sf::st_drop_geometry()), k] <- 0
            }

            # Reorder columns to match class order provided in NFILC
            # documentation.
            data <- data[, c(
              grep("nfilc_", names(data), value = TRUE, invert = TRUE),
              paste0("nfilc_", nfilc_classes$name)
            )]
          } else {
            # Extract point value from SCANFI raster. It appears to be possible
            # that a point falls such that it extracts from two raster tiles,
            # so handle that possibility below.
            extr_table <- terra::extract(
              scanfi_data[[i]],
              tmp,
              fun = unique
            )[, "SCANFI_att_nfiLandCover_SW_2020_v1.2"]

            # Whether only a single value was extracted (class == "integer") or
            # multiple values (else) prepare to pass to input data.
            if (class(extr_table) == "integer") {
              extr_table <- extr_table %>%
                as.data.frame()

              names(extr_table) <- "class"

              extr_table <- dplyr::left_join(
                extr_table,
                nfilc_classes,
                by = "class"
              )
            } else {
              extr_table <- extr_table %>%
                as.data.frame() %>%
                dplyr::select(SCANFI_att_nfiLandCover_SW_2020_v1.2)

              names(extr_table) <- "class"

              extr_table <- dplyr::left_join(
                extr_table,
                nfilc_classes,
                by = "class"
              )
            }

            # Join extracted value to input data. If multiple values were
            # extracted, join the first value in extr_table and warn the user
            # about potential values so they can adjust manually.
            tryCatch(
              data[
                data$SurveyAreaIdentifier == j,
                "nfilc_class"
              ] <- nfilc_classes$name[
                nfilc_classes$class ==
                  terra::extract(scanfi_data[[i]], tmp, fun = unique)[,
                    "SCANFI_att_nfiLandCover_SW_2020_v1.2"
                  ]
              ],
              warning = function(w) {
                if (
                  conditionMessage(w) ==
                    paste0(
                      "longer object length is not a multiple of shorter",
                      " object length"
                    )
                ) {
                  warning(paste0(
                    "[SCANFI (",
                    i,
                    ") Extraction] site ",
                    j,
                    " touches multiple cells. Extraction returned `",
                    suppressWarnings(nfilc_classes$name[
                      nfilc_classes$class ==
                        terra::extract(scanfi_data[[i]], tmp, fun = unique)[,
                          "SCANFI_att_nfiLandCover_SW_2020_v1.2"
                        ]
                    ]),
                    "` but possible values were `",
                    stringr::str_flatten(extr_table$name, collapse = "`, `"),
                    "`. Please examine to choose desired output and replace if",
                    " necessary.",
                    call. = FALSE
                  ))
                } else {
                  warning(conditionMessage(w), call. = FALSE)
                }
              }
            )
          }
        } else {
          # For other SCANFI variables, if buffered, extract using
          # exactextractr::exact_extract(). If not, extract using
          # terra::extract().
          if (buffered == TRUE) {
            data[
              data$SurveyAreaIdentifier == j,
              paste0("scanfi_", i)
            ] <- exactextractr::exact_extract(
              x = scanfi_data[[i]],
              y = tmp,
              fun = "mean"
            )
          } else {
            data[
              data$SurveyAreaIdentifier == j,
              paste0("scanfi_", i)
            ] <- terra::extract(
              x = scanfi_data[[i]],
              y = tmp,
              fun = "mean",
              na.rm = TRUE
            )[, 2]
          }
        }
      }
    }
  }

  # Check if attributes were found and stored from input data. If they were
  # found reattach.
  if (exists("attrs")) {
    # Reattach attributes

    attributes(data)[names(attrs)] <- attrs
  }

  # Reinstate user's specified column names.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
  }

  # Remove SCANFI files if requested.
  if (retain == FALSE) {
    # Check that if default directory doesn't exist an alterate has been
    # specified.
    if (is.null(dl_path) & !dir.exists("./scanfi")) {
      warning(
        "[SCANFI Extraction] unable to find default SCANFI",
        " directory and no alternate specified using dl_path argument",
        ". No files will be removed.",
        call. = FALSE
      )
    } else {
      message(paste0("[SCANFI Extraction] task complete. Removing files."))

      file.remove(list.files(
        ifelse(is.null(dl_path), "./scanfi", paste0(dl_path, "/scanfi")),
        full.names = TRUE
      ))
    }
  }

  # Return input data with appended SCANFI columns.
  return(data)
}


############################# DAYMET FUNCTIONS #################################

# Function to download data from Daymet. Wrapper for appeears::request_rs().
daymet_download <- function(
  data,
  covariates = "daymet_prcp", # Other options listed in nc_covariate_table().
  ed_username, # users' EarthData account username NOT EMAIL.
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  daymet_transfer = FALSE, # Should data be transferred from AppEEARS? Only
  # change to true once requests have been submitted and you have been notified
  # via email that they are complete or if you have your own completed request
  # IDs.
  dl_path = NULL # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'worldclim/' in the working
  # directory.
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra",
    "appeears"
  ))

  # Check that an EarthData account username has been provided. If not, return
  # error.
  if (missing(ed_username)) {
    stop(
      "[Daymet Download] MODIS data requested but Earthdata system login",
      " information not supplied. NOTE: downloading DAYMET data requires your",
      " EarthData username, not email. Please register at",
      " https://urs.earthdata.nasa.gov/users/new and supply using",
      " the `ed_username` parameter.",
      call. = FALSE
    )
  }

  # Check whether an EarthData password exists in the environment (is specified
  # earlier in the nc_covariates() workflow), and if not, request using
  # askpass::askpass().
  if (is.null(parent.frame()$ed_password)) {
    ed_password <- askpass::askpass(
      prompt = paste0(
        "Please enter password for ",
        "EarthData user '",
        ed_username,
        "'."
      )
    )
  } else {
    ed_password <- parent.frame()$ed_password
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[Daymet Download] downloading requires an sf or terra object as input",
      " in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(data, "date_year"))) {
    date_year <- attr(data, "date_year")
  }

  if (is.null(date_month) & !is.null(attr(data, "date_month"))) {
    date_month <- attr(data, "date_month")
  }

  if (is.null(date_day) & !is.null(attr(data, "date_day"))) {
    date_day <- attr(data, "date_day")
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name, date_year, date_month, date_day)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      (!("SurveyAreaIdentifier" %in% data_cols) |
        !("survey_year" %in% data_cols) |
        !("survey_month" %in% data_cols) |
        !("survey_day" %in% data_cols))
  ) {
    stop(
      "[Daymet Download] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_year) & !("survey_year" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  if (!is.null(date_month) & !("survey_month" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }

  # Use month_check() to validate month data.
  month_corr <- c()

  for (i in 1:length(data$survey_month)) {
    month_corr[i] <- month_check(data$survey_month[i])
  }

  data$survey_month <- month_corr

  data$survey_month <- as.numeric(data$survey_month)

  if (!is.null(date_day) & !("survey_day" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_day" = !!date_day)
  }

  # Use dom_check() to validate day data.
  for (i in data$survey_day) {
    dom_check(i)
  }

  data$survey_day <- as.numeric(data$survey_day)

  # Create area of interest polygon from provided sf object.
  if (input_fmt$type == "sf") {
    # Check whether sf object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)

    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_transform("ESRI:102001") %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    } else {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    }
  }

  # Create area of interest polygon from provided terra object.
  if (input_fmt$type == "terra") {
    # Check whether terra object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::project("ESRI:102001") %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    } else {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    }

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data)
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./daymet")) {
    dir.create("./daymet", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/daymet"))) {
    dir.create(paste0(dl_path, "/daymet"), recursive = TRUE)
  }

  # Set EarthData username and password in user Keyring.
  options(keyring_backend = "file")

  appeears::rs_set_key(user = ed_username, password = ed_password)

  # Autheniticate with AppEEARS.
  token <- appeears::rs_login(user = ed_username)

  # Create index from each requested Daymet covariate.
  daymet_vars <- gsub(
    pattern = "daymet_",
    replacement = "",
    grep("daymet_", covariates, value = TRUE)
  )

  # By default, take user through request submission process.
  if (daymet_transfer == FALSE) {
    # Build a request for each surveyed year to be submitted to AppEEARS. This
    # request will download data for every day between the first and last
    # observation date in each year.
    tasks <- list()

    for (i in sort(unique(data$survey_year))) {
      tasks[[as.character(i)]] <- data.frame(
        task = "polygon",
        subtask = "subtask",
        latitude = mean(sf::st_coordinates(data %>% sf::st_transform(4326))[,
          "Y"
        ]),
        longitude = mean(sf::st_coordinates(data %>% sf::st_transform(4326))[,
          "X"
        ]),
        start = paste0(
          i,
          "-",
          ifelse(
            nchar(min(data$survey_month[data$survey_year == i])) == 1,
            paste0(0, min(data$survey_month[data$survey_year == i])),
            min(data$survey_month[data$survey_year == i])
          ),
          "-",
          ifelse(
            nchar(min(data$survey_day[
              data$survey_month ==
                min(data$survey_month[data$survey_year == i]) &
                data$survey_year == i
            ])) ==
              1,
            paste0(
              0,
              min(data$survey_day[
                data$survey_month ==
                  min(data$survey_month[data$survey_year == i]) &
                  data$survey_year == i
              ])
            ),
            min(data$survey_day[
              data$survey_month ==
                min(data$survey_month[data$survey_year == i]) &
                data$survey_year == i
            ])
          )
        ),
        end = paste0(
          i,
          "-",
          ifelse(
            nchar(max(data$survey_month[data$survey_year == i])) == 1,
            paste0(0, max(data$survey_month[data$survey_year == i])),
            max(data$survey_month[data$survey_year == i])
          ),
          "-",
          ifelse(
            nchar(max(data$survey_day[
              data$survey_month ==
                max(data$survey_month[data$survey_year == i]) &
                data$survey_year == i
            ])) ==
              1,
            paste0(
              0,
              max(data$survey_day[
                data$survey_month ==
                  max(data$survey_month[data$survey_year == i]) &
                  data$survey_year == i
              ])
            ),
            max(data$survey_day[
              data$survey_month ==
                max(data$survey_month[data$survey_year == i]) &
                data$survey_year == i
            ])
          )
        ),
        product = "DAYMET.004",
        layer = daymet_vars
      )
    }

    # Final build and submission.
    for (i in sort(unique(data$survey_year))) {
      task <- appeears::rs_build_task(
        df = tasks[[as.character(i)]],
        roi = sf::st_as_sf(study_area),
        format = "geotiff"
      )

      appeears::rs_request(
        request = task,
        user = ed_username,
        transfer = FALSE,
        verbose = TRUE
      )
    }

    # Open list to store request IDs.
    task_ids <- list()

    # Grab request IDs.
    tasklist <- appeears::rs_list_task(user = ed_username)

    # Rows 1:length(unique(data$survey_year)) should contain the request
    # ID's for the requests submitted above.
    for (i in sort(unique(data$survey_year), decreasing = TRUE)) {
      task_ids[[as.character(i)]] <- tasklist[
        which(sort(unique(data$survey_year), decreasing = TRUE) == i),
        "task_id"
      ]
    }

    # Save externally in case user ends R session.
    saveRDS(
      task_ids,
      file = ifelse(
        is.null(dl_path),
        "./daymet/daymet_reqs.RDS",
        paste0(dl_path, "/daymet/daymet_reqs.RDS")
      )
    )

    # End AppEEARS session.
    appeears::rs_logout(token)

    # Send detailed message instructing user on next steps.
    message(cat(
      "[Daymet Download] requests have been placed with appeears for the data",
      " you've requested. Look to your email for confirmation that these have",
      " been completed. We have saved the request data in an external object",
      " at ",
      ifelse(
        is.null(dl_path),
        "./daymet/daymet_reqs.RDS",
        paste0(dl_path, "/daymet/daymet_reqs.RDS")
      ),
      " and as the output of this daymet_download/nc_covariates call. Please",
      " rerun your call to daymet_download/nc_covariates with parameter",
      " 'daymet_transfer' set to TRUE once you have received confirmation that",
      " these requests are approved at your EarthData email."
    ))

    return(daymet_reqs)
  } else {
    # If transfer requested, search for request IDs saved externally. If not
    # present, return error.

    if (
      file.exists(ifelse(
        is.null(dl_path),
        "./daymet/daymet_reqs.RDS",
        paste0(dl_path, "/daymet/daymet_reqs.RDS")
      ))
    ) {
      appeears <- readRDS(ifelse(
        is.null(dl_path),
        "./daymet/daymet_reqs.RDS",
        paste0(dl_path, "/daymet/daymet_reqs.RDS")
      ))
    } else {
      stop(paste0(
        "[Daymet Download] cannot find file daymet_req.RDS at ",
        ifelse(
          is.null(dl_path),
          "./daymet/daymet_reqs.RDS",
          paste0(dl_path, "/daymet/daymet_reqs.RDS")
        ),
        ". Have you submitted an initial request with",
        " appeears_transfer = FALSE? Have you moved the file?"
      ))
    }

    # Loop through each year and download respective request if not already
    # downloaded.
    for (i in sort(unique(data$survey_year))) {
      if (
        !dir.exists(ifelse(
          is.null(dl_path),
          paste0("./daymet/", appeears[[as.character(i)]]),
          paste0(dl_path, "/daymet/", appeears[[as.character(i)]])
        ))
      ) {
        dir.create(ifelse(
          is.null(dl_path),
          paste0("./daymet/", appeears[[as.character(i)]]),
          paste0(dl_path, "/daymet/", appeears[[as.character(i)]])
        ))

        message(paste0("[Daymet Download] downloading Daymet data for ", i))

        appeears::rs_transfer(
          task_id = appeears[[as.character(i)]],
          user = ed_username,
          path = ifelse(
            is.null(dl_path),
            paste0("./daymet/", appeears[[as.character(i)]]),
            paste0(dl_path, "/daymet/", appeears[[as.character(i)]])
          )
        )

        message(paste0(
          "[Daymet Download] Daymet data for ",
          i,
          " downloaded."
        ))
      }
    }

    # Store request ID object to return to user for use in daymet_extract().
    if (daymet_transfer == TRUE) {
      appeears <- readRDS(ifelse(
        is.null(dl_path),
        "./daymet/daymet_reqs.RDS",
        paste0(dl_path, "/daymet/daymet_reqs.RDS")
      ))

      # Return request ID object.
      return(appeears)
    }
  }
}

daymet_extract <- function(
  data,
  daymet_reqs, # Named list. Each list element should be named after
  # a year for which data was requested, and should contain the corresponding
  # request ID.
  covariates = "daymet_prcp", # Options listed in nc_covariate_table().
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  dl_path = NULL, # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'daymet/' in the working
  # directory.
  retain = TRUE
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "readr",
    "terra"
  ))

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[Daymet Extraction] extraction requires an sf or terra object as input",
      " in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Check that DAYMET request information is supplied.
  if (missing(daymet_reqs)) {
    stop(
      "[Daymet Extraction] no Daymet request details are provided to extract from.",
      " Please provide a named list with a element containing the request ID for",
      " each year of data downloaded, named with the corresponding year. Data ",
      " can be downloaded using daymet_download().",
      call. = FALSE
    )
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(data, "date_year"))) {
    date_year <- attr(data, "date_year")
  }

  if (is.null(date_month) & !is.null(attr(data, "date_month"))) {
    date_month <- attr(data, "date_month")
  }

  if (is.null(date_day) & !is.null(attr(data, "date_day"))) {
    date_day <- attr(data, "date_day")
  }

  # Store attributes so they don't get lost.

  # List potential attributes.
  attr_names <- c(
    "site_name",
    "coord_lon",
    "coord_lat",
    "date_year",
    "date_month",
    "date_day",
    "date_ordinal",
    "date_lubridate",
    "crs"
  )

  # If any potential attribute names are present in the data attributes,
  # store.
  if (length(attr_names[attr_names %in% names(attributes(data))]) > 0) {
    attrs <- attributes(data)[attr_names[
      attr_names %in% names(attributes(data))
    ]]
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name, date_year, date_month, date_day)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      (!("SurveyAreaIdentifier" %in% data_cols) |
        !("survey_year" %in% data_cols) |
        !("survey_month" %in% data_cols) |
        !("survey_day" %in% data_cols))
  ) {
    stop(
      "[Daymet Extraction] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that",
      " diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_year) & !("survey_year" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  if (!is.null(date_month) & !("survey_month" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }

  # Validate month data using month_check()
  month_corr <- c()

  for (i in 1:length(data$survey_month)) {
    month_corr[i] <- month_check(data$survey_month[i])
  }

  data$survey_month <- month_corr

  data$survey_month <- as.numeric(data$survey_month)

  if (!is.null(date_day) & !("survey_day" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_day" = !!date_day)
  }

  # Validate day data using dom_check()
  for (i in data$survey_day) {
    dom_check(i)
  }

  data$survey_day <- as.numeric(data$survey_day)

  # Check whether sf object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "sf") {
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
  }

  # Check whether terra object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "terra") {
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data)
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("exactextractr")
  }

  # Create index using requested covariates.
  daymet_vars <- gsub(
    pattern = "daymet_",
    replacement = "",
    grep("daymet_", covariates, value = TRUE)
  )

  appeears <- daymet_reqs

  # Open list to store information file that comes with downloaded Daymet data.
  daymet_stats <- list()

  # Open vector to store date data.
  all_dates <- c()

  # Loop through each year and check dates data is available for. This
  # information is sources from the DAYMET-004-Statistics.csv file that comes
  # with downloads. If this file can't be found, return error.
  for (i in sort(unique(data$survey_year))) {
    if (
      file.exists(ifelse(
        is.null(dl_path),
        paste0(
          "./daymet/",
          appeears[[as.character(i)]],
          "/DAYMET-004-Statistics.csv"
        ),
        paste0(
          dl_path,
          "/daymet/",
          appeears[[as.character(i)]],
          "/DAYMET-004-Statistics.csv"
        )
      ))
    ) {
      daymet_stats[[as.character(i)]] <- readr::read_csv(ifelse(
        is.null(dl_path),
        paste0(
          "./daymet/",
          appeears[[as.character(i)]],
          "/DAYMET-004-Statistics.csv"
        ),
        paste0(
          dl_path,
          "/daymet/",
          appeears[[as.character(i)]],
          "/DAYMET-004-Statistics.csv"
        )
      ))

      all_dates <- c(all_dates, unique(daymet_stats[[as.character(i)]]$Date))
    } else {
      stop(
        "[Daymet Extraction] cannot find ",
        ifelse(
          is.null(dl_path),
          paste0(
            "./daymet/",
            appeears[[as.character(i)]],
            "/DAYMET-004-Statistics.csv"
          ),
          paste0(
            dl_path,
            "/daymet/",
            appeears[[as.character(i)]],
            "/DAYMET-004-Statistics.csv"
          )
        ),
        ". Please provide this file along with all downloaded rasters in",
        " folders for each year under a folder named 'daymet' in your working",
        " directory (default) or under the path specified using the dl_path",
        " argument.",
        call. = FALSE
      )
    }
  }

  # Convert to date objects
  all_dates <- as.Date(all_dates)

  # Create comparable date objects in original data.
  data$date <- as.Date(paste0(
    data$survey_year,
    "-",
    data$survey_month,
    "-",
    data$survey_day
  ))

  # Note any dates that do not have available Daymet data. Warn.
  missing_dates <- sort(data$date[!(data$date %in% all_dates)])

  if (length(missing_dates) > 0) {
    warning(
      "[Daymet Extraction] data has not been provided for some dates. These",
      " are: ",
      stringr::str_flatten_comma(as.character(missing_dates)),
      ". No value will be returned for these dates. Keep in mind that Daymet",
      " data for the current year may not be available yet.",
      call. = FALSE
    )
  }

  # Fetch all dates with available data.
  dates <- sort(unique(data$date[data$date %in% all_dates]))

  # Open vector to store site names that are outside of spatial extent of
  # provided Daymet files.
  bad_sites <- c()

  # Loop through each requested Daymet variable and extract.
  for (i in daymet_vars) {
    # Loop through each date with data.
    for (j in dates) {
      # Grab all observations needing data from date j.
      pts_to_fill <- dplyr::filter(data, date == j)

      j_date <- as.Date(j)

      # Access corresponding file name from data in information file.
      filename <- gsub(
        pattern = "DAYMET_",
        replacement = "DAYMET.",
        daymet_stats[[as.character(lubridate::year(j_date))]]$`File Name`[
          daymet_stats[[as.character(lubridate::year(j_date))]]$Date == j_date &
            daymet_stats[[as.character(lubridate::year(j_date))]]$Dataset == i
        ]
      )

      # Read in data for date j.
      daymet <- terra::rast(ifelse(
        is.null(dl_path),
        paste0(
          "./daymet/",
          appeears[[as.character(lubridate::year(j_date))]],
          "/",
          filename,
          ".tif"
        ),
        paste0(
          dl_path,
          "/daymet/",
          appeears[[as.character(lubridate::year(j_date))]],
          "/",
          filename,
          ".tif"
        )
      ))

      # Loop through each site and extract.
      for (k in unique(pts_to_fill$SurveyAreaIdentifier)) {
        tmp <- pts_to_fill %>%
          dplyr::filter(SurveyAreaIdentifier == k) %>%
          dplyr::select(SurveyAreaIdentifier, geometry) %>%
          dplyr::distinct() %>%
          sf::st_transform(sf::st_crs(daymet))

        # Check if the site falls outside of or is only partially covered by
        # the spatial extent of the provided Daymet rasters. If so, warn and
        # store site name to avoid extracting data for it later.
        if (
          !terra::is.related(
            daymet,
            terra::vect(tmp),
            relation = "intersects"
          )
        ) {
          warning(
            "[Daymet (",
            i,
            ") Extraction]  site ",
            k,
            " falls outside of the spatial extent of the DAYMET rasters",
            " provided. No value will be returned.",
            call. = FALSE
          )

          bad_sites <- c(bad_sites, k)
        } else if (
          terra::is.related(
            daymet,
            terra::vect(tmp),
            relation = "intersects"
          ) &
            !terra::is.related(
              daymet,
              terra::vect(tmp),
              relation = "contains"
            )
        ) {
          warning(
            "[Daymet (",
            i,
            ") Extraction] site ",
            k,
            "'s buffered area is only partially contained by the spatial",
            " extent of the DAYMET rasters provided. Returned ",
            i,
            " value will be derived from the available values.",
            call. = FALSE
          )
        } else {
          # If no issues with coverage, proceed to extraction. If buffered,
          # extract using exactextractr::exact_extract(). If not, extract
          # using terra::extract().
          if (buffered == TRUE) {
            data[
              data$SurveyAreaIdentifier == k & data$date == j_date,
              i
            ] <- exactextractr::exact_extract(
              x = daymet,
              y = tmp,
              fun = "mean"
            )
          } else {
            data[
              data$SurveyAreaIdentifier == k & data$date == j_date,
              i
            ] <- terra::extract(
              x = daymet,
              y = tmp,
              fun = "mean",
              na.rm = TRUE
            )[, 2]
          }
        }
      }

      # Progress bar.
      message(paste0(
        "[Daymet Extraction] Date ",
        which(dates == j),
        " of ",
        length(dates),
        " complete."
      ))
    }

    # Code to grab nearest raster value for sites outside of raster coverage.
    # Not sure whether to keep this since we are warning users about these sites
    # and saying nothing will be returned. Maybe keep as an option
    # (nearest = TRUE)?
    #
    # if (
    #   TRUE %in%
    #     is.na(data[
    #       data$date %in% dates & !(data$SurveyAreaIdentifier %in% bad_sites),
    #       i
    #     ])
    # ) {
    #   warning(paste0(
    #     "[Daymet (",
    #     i,
    #     ") Extraction] some points are close to shore, and so fall outside of",
    #     " raster coverage. For these cells, the nearest cell value will be",
    #     " used. Repairing now."
    #   ))
    #
    #   for (j in dates) {
    #     sites_to_fill <- unique(data$SurveyAreaIdentifier[
    #       is.na(data[, i]) & data$date == j
    #     ])
    #
    #     if (nrow(sites_to_fill) > 0) {
    #       j_date <- as.Date(j)
    #
    #       filename <- gsub(
    #         pattern = "DAYMET_",
    #         replacement = "DAYMET.",
    #         daymet_stats[[as.character(lubridate::year(j_date))]]$`File Name`[
    #           daymet_stats[[as.character(lubridate::year(j_date))]]$Date ==
    #             j_date &
    #             daymet_stats[[as.character(lubridate::year(
    #               j_date
    #             ))]]$Dataset ==
    #               i
    #         ]
    #       )
    #
    #       daymet <- terra::rast(ifelse(
    #         is.null(dl_path),
    #         paste0(
    #           "./daymet/",
    #           appeears[[as.character(lubridate::year(j_date))]],
    #           "/",
    #           filename,
    #           ".tif"
    #         ),
    #         paste0(
    #           dl_path,
    #           "/daymet/",
    #           appeears[[as.character(lubridate::year(j_date))]],
    #           "/",
    #           filename,
    #           ".tif"
    #         )
    #       ))
    #
    #       for (k in sites_to_fill) {
    #         tmp <- data %>%
    #           dplyr::filter(SurveyAreaIdentifier == k) %>%
    #           dplyr::select(SurveyAreaIdentifier, geometry) %>%
    #           dplyr::distinct() %>%
    #           sf::st_buffer(2500) %>%
    #           sf::st_transform(terra::crs(daymet))
    #
    #         daymet_crop <- terra::crop(daymet, terra::vect(tmp)) %>%
    #           terra::as.points()
    #
    #         near.pt <- terra::nearest(terra::vect(tmp), daymet_crop)$to_id
    #
    #         data[
    #           data$SurveyAreaIdentifier == k & data$date == j,
    #           i
    #         ] <- mean(terra::values(daymet_crop[near.pt])[, filename])
    #       }
    #     }
    #   }
    # }
  }

  # Remove temporary date column from original data.
  data <- select(data, -date)

  # Check if attributes were found and stored from input data. If they were
  # found reattach.
  if (exists("attrs")) {
    # Reattach attributes

    attributes(data)[names(attrs)] <- attrs
  }

  # Reinstate user's specified column names.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
  }

  if (!is.null(date_year)) {
    names(data)[names(data) == "survey_year"] <- date_year
  }

  if (!is.null(date_month)) {
    names(data)[names(data) == "survey_month"] <- date_month
  }

  if (!is.null(date_day)) {
    names(data)[names(data) == "survey_day"] <- date_day
  }

  # Remove Daymet files if requested.
  if (retain == FALSE) {
    message(paste0("[Daymet Extraction] task complete. Removing files."))

    file.remove(list.files(
      ifelse(is.null(dl_path), "./daymet", paste0(dl_path, "/daymet")),
      full.names = TRUE
    ))
  }

  # Return input data with appended Daymet columns.
  return(data)
}

############################## MERGING FUNCTION ################################

# Function to merge outputs of extraction functions to original data.
nc_covariates_merge <- function(
  original_data, # Data input to data_fmt() or an extraction function.
  covariate_data, # Output of an extraction function.
  coord_lon = NULL, # as in cosewic_ranges
  coord_lat = NULL, # as in cosewic_ranges
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_lubridate = NULL, # optional argument to provide column name containing
  # 'lubridate' date objects.
  date_ordinal = NULL # optional argument to provide column name containing
  # ordinal dates.
) {
  # Check packages.
  have_pkg_check(c(
    "sf",
    "terra",
    "tidyterra"
  ))

  # Fetch format of original data.
  input_fmt <- covariate_fmt_check(original_data)

  # Store relevant information depending on original data format.
  if (input_fmt$type == "data.frame") {
    original_fmt <- "data.frame"
  }

  if (input_fmt$type == "sf") {
    original_fmt <- "sf"

    original_cols <- names(original_data)

    original_geom <- input_fmt$geometry

    original_crs <- sf::st_crs(original_data)
  }

  if (input_fmt$type == "terra") {
    original_fmt <- "terra"

    original_cols <- names(original_data)

    original_geom <- input_fmt$geometry

    original_crs <- terra::crs(original_data)
  }

  # Fetch format of covariate data.
  output_fmt <- covariate_fmt_check(covariate_data)

  # Covariate data should be the output of an extraction function, which is
  # expected to be an sf object. If not, return error.
  if (!(output_fmt$type == "sf")) {
    stop(
      "[Data Merging] Provided covariate data not in expected format.",
      " 'sf' object expected as output by NatureCounts covariate extraction",
      " functions.",
      call. = FALSE
    )
  }

  # Check whether sf object is buffered or not to determine joining
  # procedure down the line.
  if (output_fmt$type == "sf") {
    buffer <- ifelse(output_fmt$geometry == "POLYGON", TRUE, FALSE)
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(coord_lon) & !is.null(attr(covariate_data, "coord_lon"))) {
    coord_lon <- attr(covariate_data, "coord_lon")
  }

  if (is.null(coord_lat) & !is.null(attr(covariate_data, "coord_lat"))) {
    coord_lat <- attr(covariate_data, "coord_lat")
  }

  if (is.null(site_name) & !is.null(attr(covariate_data, "site_name"))) {
    site_name <- attr(covariate_data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(covariate_data, "date_year"))) {
    date_year <- attr(covariate_data, "date_year")
  }

  if (is.null(date_month) & !is.null(attr(covariate_data, "date_month"))) {
    date_month <- attr(covariate_data, "date_month")
  }

  if (is.null(date_day) & !is.null(attr(covariate_data, "date_day"))) {
    date_day <- attr(covariate_data, "date_day")
  }

  if (is.null(date_ordinal) & !is.null(attr(covariate_data, "date_ordinal"))) {
    date_ordinal <- attr(covariate_data, "date_ordinal")
  }

  if (
    is.null(date_lubridate) & !is.null(attr(covariate_data, "date_lubridate"))
  ) {
    date_lubridate <- attr(covariate_data, "date_lubridate")
  }

  # Joining procedure for original data in data frame objects.
  if (original_fmt == "data.frame") {
    # Remove SurveyAreaIdentifier column as this is a less reliable joiner
    # than coordinate data.
    covariate_data[, ifelse(
      is.null(site_name),
      "SurveyAreaIdentifier",
      site_name
    )] <- NULL

    # Convert covariate data to data frame.
    covariate_data <- sf::st_drop_geometry(covariate_data)

    # If lubridate or ordinal date data is present, use to join. If not, use
    # individual date columns. If both lubridate and ordinal date data is
    # present, use lubridate data to join.
    if (is.null(date_ordinal) & is.null(date_lubridate)) {
      matched_data <- dplyr::left_join(
        original_data,
        covariate_data,
        by = c(
          ifelse(is.null(coord_lon), "longitude", coord_lon),
          ifelse(is.null(coord_lat), "latitude", coord_lat),
          ifelse(is.null(date_year), "survey_year", date_year),
          ifelse(is.null(date_month), "survey_month", date_month),
          ifelse(is.null(date_day), "survey_day", date_day)
        )
      )
    } else {
      if (!is.null(date_ordinal)) {
        if (!is.null(date_lubridate)) {
          covariate_data <- dplyr::select(
            covariate_data,
            -survey_year,
            -survey_month,
            -survey_day
          )

          matched_data <- dplyr::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              date_lubridate
            )
          )
        } else {
          covariate_data <- dplyr::select(
            covariate_data,
            -survey_month,
            -survey_day
          )

          matched_data <- dplyr::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              ifelse(is.null(date_year), "survey_year", date_year),
              date_ordinal
            )
          )
        }
      }

      if (!is.null(date_lubridate)) {
        covariate_data <- dplyr::select(
          covariate_data,
          -survey_year,
          -survey_month,
          -survey_day
        )

        matched_data <- dplyr::left_join(
          original_data,
          covariate_data,
          by = c(
            ifelse(is.null(coord_lon), "longitude", coord_lon),
            ifelse(is.null(coord_lat), "latitude", coord_lat),
            date_lubridate
          )
        )
      }
    }
  }

  # Joining procedure for original data in sf objects.
  if (original_fmt == "sf") {
    # Remove SurveyAreaIdentifier column as this is a less reliable joiner
    # than coordinate data.
    covariate_data[, ifelse(
      is.null(site_name),
      "SurveyAreaIdentifier",
      site_name
    )] <- NULL

    # Convert covariate data to data frame.
    covariate_data <- sf::st_drop_geometry(covariate_data)

    # Edge case: there is a column in the original data called X that needs
    # to be preserved.
    if ("X" %in% names(original_data)) {
      x_storage <- original_data$X

      original_data$X <- NULL
    }

    # Edge case: there is a column in the original data called Y that needs
    # to be preserved.
    if ("Y" %in% names(original_data)) {
      y_storage <- original_data$Y

      original_data$Y <- NULL
    }

    # Edge case: there is a column in the original data called longitude that
    # needs to be preserved.
    if ("longitude" %in% names(original_data)) {
      lon_storage <- original_data$longitude

      original_data$longitude <- NULL
    }

    # Edge case: there is a column in the original data called latitude that
    # needs to be preserved.
    if ("latitude" %in% names(original_data)) {
      lat_storage <- original_data$latitude

      original_data$latitude <- NULL
    }

    # Create coordinate columns to join with. For polygon original data, use
    # centroids.
    if (original_geom == "POLYGON") {
      original_data <- cbind(
        original_data,
        sf::st_coordinates(suppressWarnings(sf::st_centroid(original_data)))
      ) %>%
        dplyr::rename(longitude = X, latitude = Y)
    } else {
      original_data <- cbind(
        original_data,
        sf::st_coordinates(original_data)
      ) %>%
        dplyr::rename(longitude = X, latitude = Y)
    }

    # Restore X and Y columns if they needed to be preserved.
    if (exists("x_storage")) {
      original_data$X <- x_storage

      rm(x_storage)
    }

    if (exists("y_storage")) {
      original_data$Y <- y_storage

      rm(y_storage)
    }

    if (!is.null(coord_lon)) {
      names(original_data)[names(original_data) == "longitude"] <- coord_lon
    }

    if (!is.null(coord_lat)) {
      names(original_data)[names(original_data) == "latitude"] <- coord_lat
    }

    # If lubridate or ordinal date data is present, use to join. If not, use
    # individual date columns. If both lubridate and ordinal date data is
    # present, use lubridate data to join.
    if (is.null(date_ordinal) & is.null(date_lubridate)) {
      matched_data <- dplyr::left_join(
        original_data,
        covariate_data,
        by = c(
          ifelse(is.null(coord_lon), "longitude", coord_lon),
          ifelse(is.null(coord_lat), "latitude", coord_lat),
          ifelse(is.null(date_year), "survey_year", date_year),
          ifelse(is.null(date_month), "survey_month", date_month),
          ifelse(is.null(date_day), "survey_day", date_day)
        )
      )
    } else {
      if (!is.null(date_ordinal)) {
        if (!is.null(date_lubridate)) {
          covariate_data <- dplyr::select(
            covariate_data,
            -survey_year,
            -survey_month,
            -survey_day
          )

          matched_data <- dplyr::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              date_lubridate
            )
          )
        } else {
          covariate_data <- dplyr::select(
            covariate_data,
            -survey_month,
            -survey_day
          )

          matched_data <- dplyr::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              ifelse(is.null(date_year), "survey_year", date_year),
              date_ordinal
            )
          )
        }
      }

      if (!is.null(date_lubridate)) {
        covariate_data <- dplyr::select(
          covariate_data,
          -survey_year,
          -survey_month,
          -survey_day
        )

        matched_data <- dplyr::left_join(
          original_data,
          covariate_data,
          by = c(
            ifelse(is.null(coord_lon), "longitude", coord_lon),
            ifelse(is.null(coord_lat), "latitude", coord_lat),
            date_lubridate
          )
        )
      }
    }

    # Remove coordinate columns used for joining.
    matched_data[, ifelse(is.null(coord_lon), "longitude", coord_lon)] <- NULL
    matched_data[, ifelse(is.null(coord_lat), "latitude", coord_lat)] <- NULL

    # Restore latitude/longitude columns if they needed to be preserved.
    if (exists("lon_storage")) {
      matched_data$longitude <- lon_storage

      rm(lon_storage)
    }

    if (exists("lat_storage")) {
      matched_data$latitude <- lat_storage

      rm(lat_storage)
    }

    # Reorder columns to match original data.
    matched_data <- matched_data[, c(
      original_cols,
      setdiff(original_cols, names(matched_data))
    )]
  }

  # Joining procedure for original data in data frame objects.
  if (original_fmt == "terra") {
    # Remove SurveyAreaIdentifier column as this is a less reliable joiner
    # than coordinate data.
    covariate_data[, ifelse(
      is.null(site_name),
      "SurveyAreaIdentifier",
      site_name
    )] <- NULL

    # Convert covariate data to data frame.
    covariate_data <- sf::st_drop_geometry(covariate_data)

    # Edge case: there is a column in the original data called x that needs
    # to be preserved.
    if ("x" %in% names(original_data)) {
      x_storage <- original_data$x

      original_data$x <- NULL
    }

    # Edge case: there is a column in the original data called y that needs
    # to be preserved.
    if ("y" %in% names(original_data)) {
      y_storage <- original_data$y

      original_data$y <- NULL
    }

    # Edge case: there is a column in the original data called longitude that
    # needs to be preserved.
    if ("longitude" %in% names(original_data)) {
      lon_storage <- original_data$longitude

      original_data$longitude <- NULL
    }

    # Edge case: there is a column in the original data called latitude that
    # needs to be preserved.
    if ("latitude" %in% names(original_data)) {
      lat_storage <- original_data$latitude

      original_data$latitude <- NULL
    }

    # Create coordinate columns to join with. For polygon original data, use
    # centroids.
    if (input_fmt$geometry == "polygons") {
      original_data <- cbind(
        original_data,
        terra::crds(terra::centroids(original_data))
      ) %>%
        tidyterra::rename(longitude = x, latitude = y)
    } else {
      original_data <- cbind(original_data, terra::crds(original_data)) %>%
        tidyterra::rename(longitude = x, latitude = y)
    }

    # Restore X and Y columns if they needed to be preserved.
    if (exists("x_storage")) {
      original_data$x <- x_storage

      rm(x_storage)
    }

    if (exists("y_storage")) {
      original_data$y <- y_storage

      rm(y_storage)
    }

    if (!is.null(coord_lon)) {
      names(original_data)[names(data) == "longitude"] <- coord_lon
    }

    if (!is.null(coord_lat)) {
      names(original_data)[names(data) == "latitude"] <- coord_lat
    }

    # If lubridate or ordinal date data is present, use to join. If not, use
    # individual date columns. If both lubridate and ordinal date data is
    # present, use lubridate data to join.
    if (is.null(date_ordinal) & is.null(date_lubridate)) {
      matched_data <- tidyterra::left_join(
        original_data,
        covariate_data,
        by = c(
          ifelse(is.null(coord_lon), "longitude", coord_lon),
          ifelse(is.null(coord_lat), "latitude", coord_lat),
          ifelse(is.null(date_year), "survey_year", date_year),
          ifelse(is.null(date_month), "survey_month", date_month),
          ifelse(is.null(date_day), "survey_day", date_day)
        )
      )
    } else {
      if (!is.null(date_ordinal)) {
        if (!is.null(date_lubridate)) {
          covariate_data <- dplyr::select(
            covariate_data,
            -survey_year,
            -survey_month,
            -survey_day
          )

          matched_data <- tidyterra::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              date_lubridate
            )
          )
        } else {
          covariate_data <- dplyr::select(
            covariate_data,
            -survey_month,
            -survey_day
          )

          matched_data <- tidyterra::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              ifelse(is.null(date_year), "survey_year", date_year),
              date_ordinal
            )
          )
        }
      }

      if (!is.null(date_lubridate)) {
        covariate_data <- dplyr::select(
          covariate_data,
          -survey_year,
          -survey_month,
          -survey_day
        )

        matched_data <- tidyterra::left_join(
          original_data,
          covariate_data,
          by = c(
            ifelse(is.null(coord_lon), "longitude", coord_lon),
            ifelse(is.null(coord_lat), "latitude", coord_lat),
            date_lubridate
          )
        )
      }
    }

    # Remove coordinate columns used for joining.
    matched_data[, ifelse(is.null(coord_lon), "longitude", coord_lon)] <- NULL
    matched_data[, ifelse(is.null(coord_lat), "latitude", coord_lat)] <- NULL

    # Restore latitude/longitude columns if they needed to be preserved.
    if (exists("lon_storage")) {
      matched_data$longitude <- lon_storage

      rm(lon_storage)
    }

    if (exists("lat_storage")) {
      matched_data$latitude <- lat_storage

      rm(lat_storage)
    }

    # Reorder columns to match original data.
    matched_data <- matched_data[, c(
      original_cols,
      setdiff(original_cols, names(matched_data))
    )]
  }

  # Remove lingering attribute.
  if (!is.null(attr(matched_data, "site_name"))) {
    attr(matched_data, "site_name") <- NULL
  }

  # Return original data with appended covariate columns.
  return(matched_data)
}


########################## FULL COVARIATES FUNCTION ############################

# Full covariate download, extraction, and merging function!
nc_covariates <- function(
  data,
  covariates = NULL, # Options listed in nc_covariate_table().
  buffer = FALSE, # Should the data be buffered?
  buffer_radius = 500, # Radial distance to buffer by.
  buffer_units = "m", # Units of provided distance.
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'.
  coord_lon = NULL, # as in cosewic_ranges
  coord_lat = NULL, # as in cosewic_ranges
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'.
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'.
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'.
  date_lubridate = NULL, # optional argument to provide column name containing
  # 'lubridate' date objects.
  date_ordinal = NULL, # optional argument to provide column name containing
  # ordinal dates.
  crs = NULL, # optional argument to provide a Coordinate Reference System for
  # provided data.
  ed_email = NULL, # users' EarthData account email address. Only required if
  # land cover or vegetation data requested.
  ed_username = NULL, # users' EarthData account username. Only required if
  # Daymet data requested.
  elevation_z = 7, # determines zoom for downloaded elevation data. For more information
  # see https://github.com/tilezen/joerd/blob/master/docs/data-sources.md.
  elevation_src = "aws", # Source for elevation data. "aws" is for Terrain Tiles,
  # but users may be interested in other available sources from OpenTopography.
  # See ??get_elev_raster().
  worldclim_countries = NULL, # Character vector of country names or ISO3 codes. If left
  # NULL, country will be auto-detected.
  worldclim_res = 0.5, # resolution of WorldClim data to be downloaded. Options are 10,
  # 5, 2.5, and 0.5 minutes of a degree.
  scanfi_read = FALSE, # Should scanfi_read() be used? Assists users if direct
  # download from SCANFI server through scanfi_download() failed.
  scanfi_file = NULL, # file path to respective SCANFI data file.
  # Should be in the order variables are listed in the
  # covariates argument.
  daymet_transfer = FALSE, # Should data be transferred from AppEEARS? Only
  # change to true once requests have been submitted and you have been notified
  # via email that they are complete or if you have your own completed request
  # IDs.
  dl_path = NULL, # optional argument to provide path to download data to. By
  # default, data is downloaded to automatically generated subfolders in the
  # working directory.
  retain = TRUE, # Should data files be kept after extraction?
  merge = TRUE # Should data be merged to original data supplied in the 'data'
  # argument with nc_covariates_merge()?
) {
  # Check packages
  have_pkg_check("askpass")

  # If merging, copy original data to merge back to later.
  if (merge == TRUE) {
    original_data <- data
  }

  # If data requiring EarthData login information is requested, prompt for
  # password using askpass::askpass().
  if (
    "modis_lctype1" %in%
      covariates |
      "modis_lctype2" %in% covariates |
      "modis_lctype3" %in% covariates |
      "modis_lctype4" %in% covariates |
      "modis_lctype5" %in% covariates |
      "modis_ndvi" %in% covariates |
      "modis_evi" %in% covariates |
      length(grep("daymet_", covariates)) > 0
  ) {
    if (length(grep("daymet_", covariates)) > 0) {
      if (
        "modis_lctype1" %in%
          covariates |
          "modis_lctype2" %in% covariates |
          "modis_lctype3" %in% covariates |
          "modis_lctype4" %in% covariates |
          "modis_lctype5" %in% covariates |
          "modis_ndvi" %in% covariates |
          "modis_evi" %in% covariates
      ) {
        ed_password <- askpass::askpass(
          prompt = paste0(
            "Please enter password for ",
            "EarthData user '",
            ed_username,
            "' [, ",
            ed_email,
            "]."
          )
        )
      } else {
        ed_password <- askpass::askpass(
          prompt = paste0(
            "Please enter password for ",
            "EarthData user '",
            ed_username,
            "'."
          )
        )
      }
    } else {
      ed_password <- askpass::askpass(
        prompt = paste0(
          "Please enter password for ",
          "EarthData user '",
          ed_email,
          "'."
        )
      )
    }
  }

  # When daymet_transfer is TRUE, don't perform other operations. We assume
  # that a user first ran nc_covariates with daymet_transfer = FALSE, got other
  # requested covariate data, and then runs it again to only get Daymet data.
  if (daymet_transfer == FALSE) {
    # Format data.
    data <- data_fmt(
      data,
      site_name = site_name,
      coord_lon = coord_lon,
      coord_lat = coord_lat,
      date_year = date_year,
      date_month = date_month,
      date_day = date_day,
      date_lubridate = date_lubridate,
      date_ordinal = date_ordinal,
      crs = crs
    )

    # Buffer, if requested.
    data <- data_buff(
      data,
      buffer = buffer,
      buffer_radius = buffer_radius,
      buffer_units = buffer_units
    )

    # If requested, download and extract land cover data.
    if (
      "modis_lctype1" %in%
        covariates |
        "modis_lctype2" %in% covariates |
        "modis_lctype3" %in% covariates |
        "modis_lctype4" %in% covariates |
        "modis_lctype5" %in% covariates
    ) {
      landcover_data <- landcover_download(
        data,
        ed_email = ed_email,
        site_name = site_name,
        date_year = date_year,
        dl_path = dl_path
      )

      data <- landcover_extract(
        data,
        covariates = covariates,
        landcover_files = landcover_data,
        site_name = site_name,
        date_year = date_year,
        retain = retain
      )
    }

    # If requested, download and extract vegetation data.
    if ("modis_ndvi" %in% covariates | "modis_evi" %in% covariates) {
      vegetation_data <- vegetation_download(
        data,
        ed_email = ed_email,
        site_name = site_name,
        date_year = date_year,
        date_month = date_month,
        date_day = date_day,
        dl_path = dl_path
      )

      data <- vegetation_extract(
        data,
        covariates = covariates,
        vegetation_files = vegetation_data,
        site_name = site_name,
        date_year = date_year,
        date_month = date_month,
        date_day = date_day,
        retain = retain
      )
    }

    # If requested, download and extract elevation data.
    if ("elevation" %in% covariates) {
      elevation_data <- elevation_download(
        data,
        site_name = site_name,
        z = elevation_z,
        src = elevation_src
      )

      data <- elevation_extract(
        data,
        covariates = covariates,
        site_name = site_name,
        elevation_data = elevation_data
      )
    }

    # If requested, download and extract WorldClim data.
    if (length(grep("worldclim_", covariates)) > 0) {
      worldclim_data <- worldclim_download(
        data,
        covariates = covariates,
        countries = worldclim_countries,
        res = worldclim_res,
        dl_path = dl_path
      )

      data <- worldclim_extract(
        data,
        covariates = covariates,
        worldclim_data = worldclim_data,
        site_name = site_name,
        date_month = date_month,
        retain = retain
      )
    }

    # If requested, download (or read) and extract SCANFI data.
    if (length(grep("scanfi_", covariates)) > 0) {
      if (!scanfi_read) {
        scanfi_data <- scanfi_download(
          covariates = covariates,
          dl_path = dl_path
        )
      } else {
        scanfi_data <- scanfi_read(covariates = covariates, file = scanfi_file)
      }

      data <- scanfi_extract(
        data,
        covariates = covariates,
        scanfi_data = scanfi_data,
        site_name = site_name,
        retain = retain
      )
    }
  }

  # If requested, either submit requests for Daymet data
  # (daymet_transfer = FALSE) or download and extract Daymet data
  # (daymet_transfer = TRUE).
  if (length(grep("daymet_", covariates)) > 0) {
    daymet_reqs <- daymet_download(
      data,
      covariates = covariates,
      site_name = site_name,
      date_year = date_year,
      date_month = date_month,
      date_day = date_day,
      ed_username = ed_username,
      daymet_transfer = daymet_transfer,
      dl_path = dl_path
    )

    if (daymet_transfer == TRUE) {
      data <- daymet_extract(
        data,
        covariates = covariates,
        site_name = site_name,
        date_year = date_year,
        date_month = date_month,
        date_day = date_day,
        daymet_reqs = daymet_reqs,
        retain = retain
      )
    } else {
      return(daymet_reqs)
    }
  }

  # If requested, merge.
  if (merge == TRUE) {
    data <- nc_covariates_merge(
      original_data = original_data,
      covariate_data = data,
      coord_lon = coord_lon,
      coord_lat = coord_lat,
      site_name = site_name,
      date_year = date_year,
      date_month = date_month,
      date_day = date_day,
      date_lubridate = date_lubridate,
      date_ordinal = date_ordinal
    )
  }

  # Return outputted data.
  return(data)
}
