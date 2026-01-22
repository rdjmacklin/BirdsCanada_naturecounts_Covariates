source("./functions.R")

######################### EXAMPLE LANDCOVER WORKFLOW ###########################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020)) %>%
  rename(
    sites = SurveyAreaIdentifier,
    yr = survey_year,
    mth = survey_month,
    dy = survey_day
  )

# Format and buffer data for extraction

landcover_data <- data_fmt(
  data,
  site_name = "sites",
  date_year = "yr",
  date_month = "mth",
  date_day = "dy"
)

landcover_data <- data_buff(landcover_data, buffer = TRUE)

# Enter EarthData Login info and download data

ed_email <- readline(prompt = "Enter EarthData email: ")

modis_files <- landcover_download(
  landcover_data,
  ed_email = ed_email
)

landcover_data <- landcover_extract(
  landcover_data,
  landcover_files = modis_files,
  covariates = "modis_lctype1"
)

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[1] <- 60

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- landcover_extract(
  outofrange_data,
  modis_files = modis_files,
  covariates = c("modis_lctype1", "modis_lctype2")
)

# Tidy up

rm(ed_email, ed_pw, modis_files, landcover_data, outofrange_data)

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

vegetation_files <- vegetation_download(
  vegetation_data,
  ed_email = ed_email
)

vegetation_data <- vegetation_extract(
  vegetation_data,
  vegetation_files = vegetation_files,
  covariates = "modis_evi"
)

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[2] <- 60

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- vegetation_extract(
  outofrange_data,
  vegetation_files = vegetation_files,
  covariates = "modis_evi"
)

# Tidy up

rm(ed_email, ed_pw, vegetation_files, vegetation_data, outofrange_data)

######################## EXAMPLE ELEVATION WORKFLOW ############################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020))

# Format and buffer data for extraction

elevation_data <- data_fmt(data)

elevation_data <- data_buff(elevation_data, buffer = FALSE)

elevation_rasts <- elevation_download(elevation_data)

elevation_data <- elevation_extract(
  elevation_data,
  elevation_data = elevation_rasts
)

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[2] <- 60

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- elevation_extract(
  outofrange_data,
  elevation_data = elevation_rasts
)

# Tidy up

rm(data, elevation_rasts, elevation_data, outofrange_data)

######################## EXAMPLE WORLDCLIM WORKFLOW ############################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020))

# Format and buffer data for extraction

worldclim_data <- data_fmt(data)

worldclim_data <- data_buff(worldclim_data, buffer = FALSE)

worldclim_rasts <- worldclim_download(
  covariates = "worldclim_prec"
)

worldclim_data <- worldclim_extract(
  worldclim_data,
  worldclim_data = worldclim_rasts,
  covariates = "worldclim_prec"
)

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[2] <- 84

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- worldclim_extract(
  outofrange_data,
  worldclim_data = worldclim_rasts,
  covariates = "worldclim_prec"
)

# Tidy up

rm(data, worldclim_rasts, worldclim_data, outofrange_data)

########################## EXAMPLE SCANFI WORKFLOW #############################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020))

# Format and buffer data for extraction

scanfi_data <- data_fmt(data)

scanfi_data <- data_buff(scanfi_data, buffer = TRUE)

scanfi_rasts <- scanfi_download(covariates = "scanfi_height")

scanfi_rasts <- scanfi_read(
  covariates = "scanfi_height",
  file = "./scanfi/SCANFI_att_height_SW_2020_v1.2.tif"
)

scanfi_data <- scanfi_extract(
  scanfi_data,
  scanfi_data = scanfi_rasts,
  covariates = "scanfi_height"
)

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$latitude[2] <- 84

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- scanfi_extract(
  outofrange_data,
  scanfi_data = scanfi_rasts,
  covariates = "scanfi_height"
)

# Tidy up

rm(data, scanfi_rasts, scanfi_data, outofrange_data)

########################## EXAMPLE DAYMET WORKFLOW #############################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020))

# Format and buffer data for extraction

daymet_data <- data_fmt(data)

daymet_data <- data_buff(daymet_data, buffer = TRUE)

# Enter EarthData Login info and download data

ed_username <- readline(prompt = "Enter EarthData username: ")

daymet_download(
  daymet_data,
  covariates = "daymet_prcp",
  ed_username = ed_username,
)

daymet_reqs <- daymet_download(
  daymet_data,
  covariates = "daymet_prcp",
  ed_username = ed_username,
  daymet_transfer = FALSE
)

daymet_reqs <- daymet_download(
  daymet_data,
  covariates = "daymet_prcp",
  ed_username = ed_username,
  daymet_transfer = TRUE
)

daymet_data <- daymet_extract(
  daymet_data,
  daymet_reqs = daymet_reqs,
  covariates = "daymet_prcp"
)

# Demonstrate what happens when a site is out of range

outofrange_data <- data

outofrange_data$survey_month[2] <- 1

outofrange_data$survey_day[2] <- 1

outofrange_data$latitude[1] <- 60

outofrange_data <- data_fmt(outofrange_data)

outofrange_data <- data_buff(outofrange_data, buffer = TRUE)

outofrange_data <- daymet_extract(
  outofrange_data,
  daymet_reqs = daymet_reqs,
  covariates = "daymet_prcp"
)

# Tidy up

rm(
  data,
  daymet_files,
  ed_username,
  ed_pw,
  daymet_reqs,
  daymet_data,
  outofrange_data
)

##################### EXAMPLE FULL COVARIATES WORKFLOW #########################

## Load NatureCounts data

# Example data from NatureCounts

data <- naturecounts::bcch %>%
  filter(survey_year %in% c(1995, 2005, 2015, 2020)) %>%
  rename(
    sites = SurveyAreaIdentifier,
    yr = survey_year,
    mth = survey_month,
    dy = survey_day,
    lat = latitude,
    lon = longitude
  ) %>%
  mutate(longitude = "protect me!",  latitude = "protect me!",
         X = "protect me!", Y = "protect me!",
         x = "protect me!", y = "protect me!",
         date = as.Date(paste0(yr, "-", mth, "-", dy)), ordinal = yday(date)) %>%
  relocate(longitude, latitude, X, Y, x, y, .before = species_id)

data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("ESRI:102001")

data_terra <- vect(data_sf)

# Enter EarthData Login info and download data

ed_email <- readline(prompt = "Enter EarthData email: ")

ed_username <- readline(prompt = "Enter EarthData username: ")

output_df <- nc_covariates(
  data,
  covariates = c("daymet_dayl"),
  buffer = TRUE,
  site_name = "sites",
  date_lubridate = "date",
  coord_lat = "lat",
  coord_lon = "lon",
  ed_email = ed_email,
  ed_username = ed_username,
  retain = TRUE,
  merge = TRUE
)

output_df <- nc_covariates(
  data,
  covariates = c("daymet_dayl"),
  buffer = TRUE,
  site_name = "sites",
  date_lubridate = "date",
  coord_lat = "lat",
  coord_lon = "lon",
  ed_email = ed_email,
  ed_username = ed_username,
  daymet_transfer = TRUE,
  retain = TRUE,
  merge = TRUE
)

output_sf <- nc_covariates(
  data_sf,
  covariates = c("modis_lctype1"),
  buffer = TRUE,
  site_name = "sites",
  date_ordinal = "ordinal",
  date_year = "yr",
  ed_email = ed_email,
  retain = TRUE)

output_terra <- nc_covariates(
  data_terra,
  covariates = c("modis_lctype1"),
  buffer = TRUE,
  site_name = "sites",
  date_ordinal = "ordinal",
  date_year = "yr",
  ed_email = ed_email,
  ed_password = ed_pw,
  retain = TRUE,
  merge = FALSE
)
