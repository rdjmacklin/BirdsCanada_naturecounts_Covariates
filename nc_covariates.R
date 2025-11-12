################################################################################

# Script Title: Testing the nc_covariates function.

# Script Author: Rory Macklin (rmacklin@birdscanada.org)

# Date: October 28, 2025

################################################################################

## Setup

ed_login <- readline(prompt = "Enter EarthData username: ")

ed_pw <- readline(prompt = "Enter EarthData password: ")

# Load necessary packages

if(system.file(package = "librarian") == "") {
  
  install.packages("librarian")
  
}

if(system.file(package = "naturecounts") == "") {
  
  install.packages("naturecounts", 
                   repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
                             CRAN = 'https://cloud.r-project.org'))
  
}

librarian::shelf(naturecounts, tidyverse, sf, "USEPA/elevatr", terra, exactextractr, geodata,
                 biooracler, "rspatial/luna", landscapemetrics, measurements)

## Load NatureCounts data

# Land data from the BCMMP

if(!file.exists("./Data/Raw/bcmmp.csv")) {
  
  land.dat <- nc_data_dl(username = "rdjmacklin", request_id = 255574)
  
  write_csv(land.dat, "./Data/Raw/bcmmp.csv")
  
} else {
  
  land.dat <- read_csv("./Data/Raw/bcmmp.csv")
  
}

# Ocean data from the BCCWS

if(!file.exists("./Data/Raw/bccws.csv")) {
  
  ocean.dat <- nc_data_dl(username = "rdjmacklin", request_id = 258864) %>%
    filter(survey_year %in% c(2021:2025))
  
  write_csv(ocean.dat, "./Data/Raw/bccws.csv")
  
} else {
  
  ocean.dat <- read_csv("./Data/Raw/bccws.csv")
  
}

## Write function

# Start with package luna and MODIS

nc_covariate_table <- function() {
  
  cov.table <- data.frame(covariate_name = c("modis_lctype1", "modis_lctype2", "modis_lctype3", "modis_lctype4", "modis_lctype5", "modis_snow", "modis_ndvi", "modis_evi", "elevation", "worldclim_tavg", "worldclim_tmax", "worldclim_tmin", "worldclim_prec", "worldclim_srad", "worldclim_wind", "worldclim_vapr"),
                          covariate_source = c("MODIS Land Cover - IGBP global vegetation classification scheme", "MODIS Land Cover - University of Maryland (UMD) scheme", "MODIS Land Cover - MODIS-derived LAI/fPAR scheme", "MODIS Land Cover - MODIS-derived Net Primary Production scheme", "MODIS Land Cover - Plant Functional Type (PFT) scheme", "MODIS Snow Cover", "MODIS Vegetation Indices - Normalized Difference Vegetation Index", "MODIS Vegetation Indices - Enhanced Vegetation Index", "AWS Terrain Tiles Elevation (m)", "WorldClim - Monthly Average Temperature (degC), 1970-2000", "WorldClim - Monthly Maximum Temperature (degC), 1970-2000", "WorldClim - Monthly Minimum Temperature (degC), 1970-2000", "WorldClim - Monthly Precipitation (mm), 1970-2000", "WorldClim - Monthly Solar Radiation (kJ/m^2/day), 1970-2000", "WorldClim - Monthly Average Wind Speed (m/s), 1970-2000", "WorldClim - Monthly Average Water Vapor Pressure (kPa), 1970-2000"),
                          covariate_source_specific = c(rep("MCD12Q1", times = 5), "MOD10A1", rep("MOD13A1", times = 2), NA, rep("WorldClim Ver. 2.1", times = 7)),
                          temporal_resolution = c(rep("Annual", times = 5), "Daily", rep("16-Day", times = 2), rep("Static", times = 8)),
                          spatial_resolution = c(rep("500 m", times = 8), "~600-800m", rep("~1 km^2", times = 7)),
                          via = c(rep("luna", times = 8), "elevatr", rep("geodata", times = 7)),
                          documentation = c(rep("http://doi.org/10.5067/MODIS/MCD12Q1.006", times = 5), "http://doi.org/10.5067/MODIS/MOD10A1.061", rep("https://doi.org/10.5067/MODIS/MOD13A1.061", times = 2), "https://github.com/USEPA/elevatr", rep("https://worldclim.org/data/worldclim21.html", times = 7)))
  
  return(cov.table)
  
  
}

cov.table <- nc_covariate_table()

nc_covariates <- function(data, data_type = "df", covariates = "none", buffer = TRUE,
                          buffer_radius = NA, buffer_units = NA, latitude_col = "latitude",
                          longitude_col = "longitude", ed_login = NA, ed_password = NA,
                          dl_path = NA, retain = TRUE) {
  
  # First section: data integrity checks
  
  if(!(data_type %in% c("df", "sf"))) {
    
    stop("Only dataframe or simple features (sf) objects are acceptable. Please provide data in one of these formats and set data_type = 'df' or 'sf'.")
    
  }
  
  if(data_type == "df" & class(data)[1] == "sf") {
    
    stop("Dataframe expected but simple features (sf) object detected. Please supply dataframe object or set data_type = 'sf'.")
    
  }
  
  #### ADD CHECK FOR sf DATA TYPE - SEE IF ITS ALREADY BEEN BUFFERED
  
  if(data_type == "sf" & !(class(data)[1] == "sf")) {
    
    stop("Simple features (sf) object expected but dataframe or other data type detected. Please supply sf object or set data_type = 'df'.")
    
  }
  
  if(FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    
    stop("Covariates either not listed or invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().")
    
  }
  
  # First case: dataframe provided, buffering not requested.
  
  if(data_type == "df" & buffer == FALSE) {
    
    if(!(latitude_col %in% names(data)) | !(longitude_col %in% names(data))) {
      
      stop("Latitude/Longitude columns either missing or misspecified. Use latitude_col and longitude_col arguments to specify names if misspecified.")
      
    }
    
    data[,latitude_col] <- tryCatch(as.numeric(unname(unlist(data[,latitude_col]))),
                                    warning = function(w){
                                      
                                      if(conditionMessage(w) == "NAs introduced by coercion") {
                                        
                                        stop("Trouble coercing latitude to numeric - NAs introduced. Check input can all be coerced to numeric.")
                                        
                                      } else {
                                        
                                        cat("Trouble coercing latitude to numeric: ", conditionMessage(w))
                                        
                                      }
                                    },
                                    error = function(e){
                                      
                                      cat("Trouble coercing latitude to numeric: ", conditionMessage(e),".")
                                      
                                    })
    
    data[,longitude_col] <- tryCatch(as.numeric(unname(unlist(data[,longitude_col]))),
                                     warning = function(w){
                                       
                                       if(conditionMessage(w) == "NAs introduced by coercion") {
                                         
                                         stop("Trouble coercing longitude to numeric - NAs introduced. Check input can all be coerced to numeric.")
                                         
                                       } else {
                                         
                                         cat("Trouble coercing longitude to numeric: ", conditionMessage(w), ".")
                                         
                                       }
                                     },
                                     error = function(e){
                                       
                                       cat("Trouble coercing longitude to numeric: ", conditionMessage(e),".")
                                       
                                     }
    )
    
    if(NA %in% unique(data$latitude) | NA %in% unique(data$longitude)) {
      
      warning("Some sites missing latitude/longitude data will be ignored.")
      
    }
    
    dat_sf <- data %>%
      filter(!(is.na(latitude) | is.na(longitude))) %>%
      select(SiteCode, latitude, longitude, survey_year, survey_month, survey_day) %>%
      distinct() %>%
      arrange(SiteCode, survey_year, survey_month) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      st_transform("ESRI:102001")
    
    spatial_set <- dat_sf
    
  }
  
  # Second case: dataframe provided, buffering requested.
  
  if(data_type == "df" & buffer == TRUE) {
  
    if(is.na(buffer_radius) | is.na(buffer_units)) {
      
      stop("Buffer radius and/or units missing. Please specify using buffer_radius and buffer_units arguments.")
      
    }
    
    if(!(latitude_col %in% names(data)) | !(longitude_col %in% names(data))) {
      
      stop("Latitude/Longitude columns either missing or misspecified. Use latitude_col and longitude_col arguments to specify names if misspecified.")
      
    }
    
    data[,latitude_col] <- tryCatch(as.numeric(unname(unlist(data[,latitude_col]))),
                                    warning = function(w){
                                      
                                      if(conditionMessage(w) == "NAs introduced by coercion") {
                                        
                                        stop("Trouble coercing latitude to numeric - NAs introduced. Check input can all be coerced to numeric.")
                                        
                                      } else {
                                        
                                        cat("Trouble coercing latitude to numeric: ", conditionMessage(w))
                                        
                                        }
                                      },
                                    error = function(e){
                                      
                                      cat("Trouble coercing latitude to numeric: ", conditionMessage(e),".")
                                      
                                    })
    
    data[,longitude_col] <- tryCatch(as.numeric(unname(unlist(data[,longitude_col]))),
                                     warning = function(w){
                                       
                                       if(conditionMessage(w) == "NAs introduced by coercion") {
                                         
                                         stop("Trouble coercing longitude to numeric - NAs introduced. Check input can all be coerced to numeric.")
                                         
                                       } else {
                                         
                                         cat("Trouble coercing longitude to numeric: ", conditionMessage(w), ".")
                                         
                                         }
                                       },
                                     error = function(e){
                                       
                                       cat("Trouble coercing longitude to numeric: ", conditionMessage(e),".")
                                       
                                     }
                                     )
    
    if(NA %in% unique(data$latitude) | NA %in% unique(data$longitude)) {
      
      warning("Some sites missing latitude/longitude data will be dropped.")
      
    }
    
    dat_sf <- data %>%
      filter(!(is.na(latitude) | is.na(longitude))) %>%
      select(SiteCode, latitude, longitude, survey_year, survey_month, survey_day) %>%
      distinct() %>%
      arrange(SiteCode, survey_year, survey_month) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      st_transform("ESRI:102001")
    
    if(!(buffer_units %in% c("m", "km", "ft", "yd", "mi", "naut_mi"))) {
      
      stop("Buffer units not recognized: please set buffer_units to one of 'm' [metres], 'km' [kilometers], 'ft' [feet], 'yd' [yards], 'mi' [miles], or 'naut_mi' [nautical miles].")
      
    }
  
    dat_buff <- st_buffer(dat_sf, conv_unit(x = buffer_radius, from = buffer_units, to = "m"))
    
    spatial_set <- dat_buff
  
  }
  
  # Create AOI
  
  study_area <- st_bbox(spatial_set) %>%
    st_as_sfc() %>%
    st_buffer(ifelse(buffer == TRUE, 2*conv_unit(x = buffer_radius, from = buffer_units, to = "m"), 1000)) %>%
    vect()
  
  # Extraction steps: landcover requested.
  
  if("modis_lctype1" %in% covariates | "modis_lctype2" %in% covariates | "modis_lctype3" %in% covariates | "modis_lctype4" %in% covariates | "modis_lctype5" %in% covariates) {
    
    ########### ADD CHECK THAT DATA PROVIDED IS WITHIN THE AVAILABLE MODIS DATA WINDOW

    if(is.na(ed_login) | is.na(ed_password)) {
      
      stop("MODIS data requested but Earthdata system login information not supplied. Please register at https://urs.earthdata.nasa.gov/users/new and supply using ed_login and ed_password parameters.")
      
    }
    
    if(is.na(dl_path) & !dir.exists("./modis/MCD12Q1")) {
      
      dir.create("./modis/MCD12Q1", recursive = T)
      
    }
    
    if(!is.na(dl_path) & !dir.exists(paste0(dl_path, "/modis/MCD12Q1"))) {
      
      dir.create(paste0(dl_path, "/modis/MCD12Q1"), recursive = T)
      
    }
    
    ####### ADD CHECK TO MAKE SURE YEAR DATA IS AVAILABLE
    
    modis.files <- luna::getNASA(product = "MCD12Q1",
                                 start = paste0(min(spatial_set$survey_year), "-01-01"),
                                 end = paste0(max(spatial_set$survey_year), "-12-31"),
                                 aoi=project(study_area, "epsg:4326"),
                                 download=TRUE,
                                 overwrite=FALSE,
                                 path=ifelse(is.na(dl_path), "./modis/MCD12Q1", paste0(dl_path, "/modis/MCD12Q1")),
                                 username=ed_login,
                                 password=ed_password)
    
    modis.files <- modisDate(modis.files)
    modis.files <- cbind(modis.files, as.data.frame(modisExtent(modis.files$filename)))
    
    ######## ADD CHECK TO MAKE SURE YEAR DATA CAN BE CONVERTED TO NUMERIC
    
    modis.files$year <- as.numeric(modis.files$year)
    
    modis.match <- spatial_set %>%
      select(SiteCode, survey_year, geometry) %>%
      st_transform(crs(rast(modis.files$filename[1])))
    
    if(buffer == TRUE) {
      
      suppressWarnings(
        
        modis.match <- cbind(modis.match, st_coordinates(st_centroid(modis.match)))
        
      )
      
      
    } else {
      
      modis.match <- cbind(modis.match, st_coordinates(modis.match))
      
    }
    
    for(i in sort(unique(modis.match$survey_year))) {
      
      if(!(i %in% modis.files$year)) {
        
        warning(paste0("MODIS data not available for ", i, " - using data from nearest year (", unique(modis.files$year)[which(i-unique(modis.files$year) == min(i-unique(modis.files$year)))], ")."))
        
      }
      
    }
    
    for(i in unique(modis.match$SiteCode)) {

      for(j in unique(modis.match$survey_year[modis.match$SiteCode == i])) {

        tmp <- filter(modis.match, SiteCode == i, survey_year == j)

        suppressWarnings(

          if(!(j %in% modis.files$year)) {

            modis.match[modis.match$SiteCode == i & modis.match$survey_year == j, "filename"] <- modis.files$filename[modis.files$year == unique(modis.files$year)[which(j-unique(modis.files$year) == min(j-unique(modis.files$year)))] & modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y]

          } else {

            modis.match[modis.match$SiteCode == i & modis.match$survey_year == j, "filename"] <- modis.files$filename[modis.files$year == tmp$survey_year & modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y]

          }

        )

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
      
      for(j in unique(modis.match$filename)) {
        
        pts_to_fill <- spatial_set[spatial_set$SiteCode %in% modis.match$SiteCode[modis.match$filename == j],]
        
        modis <- rast(j)[index]
        
        for(k in unique(pts_to_fill$SiteCode)) {
          
          if(buffer == TRUE) {
            
            tmp <- spatial_set %>%
              filter(SiteCode == k) %>%
              select(SiteCode, geometry) %>%
              distinct() %>%
              st_transform(crs(modis)) %>%
              vect()
            
            modis_clip <- crop(modis, tmp)
            
            modis_pland <- calculate_lsm(modis_clip, metric = "pland")
            
            for(l in modis_pland$class) {
              
              spatial_set[spatial_set$SiteCode == k & spatial_set$survey_year %in% modis.match$survey_year[modis.match$filename == j], paste0(index, "_", modis.classes[[i]]$name[modis.classes[[i]]$class == l])] <- modis_pland$value[modis_pland$class == l]
              
            }
            
            for(l in paste0(index, "_", modis.classes[[i]]$name[paste0(index, "_", modis.classes[[i]]$name) %in% names(spatial_set)])) {
              
              spatial_set[is.na(spatial_set[,l] %>% st_drop_geometry()), l] <- 0
              
            }
            
          } else {
            
            tmp <- spatial_set %>%
              filter(SiteCode == k) %>%
              select(SiteCode, geometry) %>%
              distinct() %>%
              st_transform(crs(modis))
            
            extr_table <- terra::extract(modis, tmp, fun = unique)[,index]
            
            if(class(extr_table) == "integer") {
              
              extr_table <- extr_table %>%
                as.data.frame()
              
              names(extr_table) <- "class"
              
              extr_table <- left_join(extr_table, modis.classes[[i]], by = "class")
              
            } else {
              
              extr_table <- extr_table %>%
                as.data.frame() %>%
                select(all_of(index))
              
              names(extr_table) <- "class"
              
              extr_table <- left_join(extr_table, modis.classes[[i]], by = "class")
              
            }

            tryCatch(spatial_set[spatial_set$SiteCode == k & spatial_set$survey_year %in% modis.match$survey_year[modis.match$filename == j], paste0(index, "_Class")] <- modis.classes[[i]]$name[modis.classes[[i]]$class == terra::extract(modis, tmp, fun = unique)[,index]],
                     warning = function(w) {

                       if(conditionMessage(w) == "longer object length is not a multiple of shorter object length") {

                         warning(paste0("Site ", k, " in year(s) ", str_flatten_comma(sort(unique(modis.match$survey_year[modis.match$filename == j]))), " touches multiple cells. nc_covariates returned `", suppressWarnings(modis.classes[[i]]$name[modis.classes[[i]]$class == terra::extract(modis, tmp, fun = unique)[,index]]), "` but possible values were `", str_flatten(extr_table$name, collapse = "`, `"), "`. Please examine to choose desired output and replace if necessary."))

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
  
  # Extraction steps: snow cover requested NOT CURRENTLY WORKING, HANGS
  
  # if("modis_snow" %in% covariates) {
  #   
  #   
  #   if(is.na(ed_login) | is.na(ed_password)) {
  #     
  #     stop("MODIS data requested but Earthdata system login information not supplied. Please register at https://urs.earthdata.nasa.gov/users/new and supply using ed_login and ed_password parameters.")
  #     
  #   }
  #   
  #   if(is.na(dl_path) & !dir.exists("./modis")) {
  #     
  #     dir.create("./modis")
  #     
  #   }
  #   
  #   if(!is.na(dl_path) & !dir.exists(paste0(dl_path, "/modis"))) {
  #     
  #     dir.create(paste0(dl_path, "/modis"), recursive = T)
  #     
  #   }
  #   
  #   ####### ADD CHECK TO MAKE SURE YEAR DATA IS AVAILABLE
  #   
  #   modis.files <- luna::getNASA(product = "MOD10A1",
  #                                start = paste0(min(spatial_set$survey_year), "-01-01"),
  #                                end = paste0(max(spatial_set$survey_year), "-12-31"),
  #                                aoi=project(study_area, "epsg:4326"),
  #                                download=TRUE,
  #                                overwrite=FALSE,
  #                                server = "NSIDC_CPRD",
  #                                path=ifelse(is.na(dl_path), "./modis", paste0(dl_path, "/modis")),
  #                                username=ed_login,
  #                                password=ed_password)
  #   
  #   return(modis.files)
  #   
  # }
  
  # Extraction steps: NDVI and EVI
  
  if("modis_ndvi" %in% covariates | "modis_evi" %in% covariates) {
    
    if(is.na(ed_login) | is.na(ed_password)) {
      
      stop("MODIS data requested but Earthdata system login information not supplied. Please register at https://urs.earthdata.nasa.gov/users/new and supply using ed_login and ed_password parameters.")
      
    }
    
    if(is.na(dl_path) & !dir.exists("./modis/MOD13A1")) {
      
      dir.create("./modis/MOD13A1", recursive = T)
      
    }
    
    if(!is.na(dl_path) & !dir.exists(paste0(dl_path, "/modis/MOD13A1"))) {
      
      dir.create(paste0(dl_path, "/modis/MOD13A1"), recursive = T)
      
    }
    
    # Download first year's data
    
    for(i in c(FALSE, TRUE)) {
      
      modis.files <- list()
      
      modis.files[[as.character(min(spatial_set$survey_year))]] <- luna::getNASA(product = "MOD13A1",
                                                                                 start = paste0(min(spatial_set$survey_year), "-", min(spatial_set$survey_month[spatial_set$survey_year == min(spatial_set$survey_year)]), "-01"),
                                                                                 end = paste0(min(spatial_set$survey_year), "-", max(spatial_set$survey_month[spatial_set$survey_year == min(spatial_set$survey_year)]), ifelse(max(spatial_set$survey_month[spatial_set$survey_year == min(spatial_set$survey_year)]) %in% c(1, 3, 5, 7, 8, 10, 12), "-31", ifelse(max(spatial_set$survey_month[spatial_set$survey_year == min(spatial_set$survey_year)]) %in% c(4, 6, 9, 11), "-30", ifelse(leap_year(min(spatial_set$survey_year)), "-29", "-28")))),
                                                                                 aoi=project(study_area, "epsg:4326"),
                                                                                 download=i,
                                                                                 overwrite=FALSE,
                                                                                 path=ifelse(is.na(dl_path), "./modis/MOD13A1", paste0(dl_path, "/modis/MCD12Q1")),
                                                                                 username=ed_login,
                                                                                 password=ed_password)
      
      # Will need a way to generalize month formatting
      
      for(j in sort(unique(spatial_set$survey_year))[2:length(unique(spatial_set$survey_year))]) {
        
        modis.files[[as.character(j)]] <- luna::getNASA(product = "MOD13A1",
                                                        start = paste0(j, "-", min(spatial_set$survey_month[spatial_set$survey_year == j]), "-01"),
                                                        end = paste0(j, "-", max(spatial_set$survey_month[spatial_set$survey_year == j]), ifelse(max(spatial_set$survey_month[spatial_set$survey_year == j]) %in% c(1, 3, 5, 7, 8, 10, 12), "-31", ifelse(max(spatial_set$survey_month[spatial_set$survey_year == j]) %in% c(4, 6, 9, 11), "-30", ifelse(leap_year(j) & max(spatial_set$survey_month[spatial_set$survey_year == j]) == 2, "-29", "-28")))),
                                                        aoi=project(study_area, "epsg:4326"),
                                                        download=i,
                                                        overwrite=FALSE,
                                                        path=ifelse(is.na(dl_path), "./modis/MOD13A1", paste0(dl_path, "/modis/MCD12Q1")),
                                                        username=ed_login,
                                                        password=ed_password)
        
        
      }
      
      modis.files <- unlist(modis.files, use.names = F)
      
      if(i == FALSE) {
        
        message(paste0("MODIS NDVI/EVI products are at a 16 day resolution, resulting in ", length(modis.files), " files to download for your data. This may take some time. In future, set retain = FALSE if you wish to delete these files after use to reduce clutter."))
        
      }
      
    }
    
    modis.files <- modisDate(modis.files)
    
    modis.files$enddate <- modis.files$date + 16
    
    modis.files$year <- as.numeric(modis.files$year)
    modis.files$month <- as.numeric(modis.files$month)
    modis.files$day <- as.numeric(modis.files$day)
    
    modis.files$endyear <- year(modis.files$enddate)
    modis.files$endmonth <- month(modis.files$enddate)
    modis.files$endday <- day(modis.files$enddate)
    
    modis.files$yday <- yday(modis.files$date)
    modis.files$endyday <- yday(modis.files$enddate)
    
    yearyearday <- function(yr, yd) {
           base <- as.Date(paste0(yr, "-01-01")) # take Jan 1 of year
           day <- base + yd - 1
       }
    
    modis.files$productiondate <- yearyearday(as.numeric(substr(modis.files$filename, 61-16, 61-13)), as.numeric(substr(modis.files$filename, 61-12, 61-10))) + hms(paste0(substr(modis.files$filename, 61-9, 61-8), ":", substr(modis.files$filename, 61-7, 61-6), ":", substr(modis.files$filename, 61-5, 61-4)))
    
    modis.files <- cbind(modis.files, as.data.frame(modisExtent(modis.files$filename)))
    

    modis.match <- spatial_set %>%
      mutate(date = as.Date(paste0(survey_year, "-", survey_month, "-", survey_day))) %>%
      mutate(yday = yday(date)) %>%
      select(SiteCode, survey_year, yday, geometry) %>%
      st_transform(crs(rast(modis.files$filename[1])))
    
    if(buffer == TRUE) {
      
      suppressWarnings(
        
        modis.match <- cbind(modis.match, st_coordinates(st_centroid(modis.match)))
        
      )
      
      
    } else {
      
      modis.match <- cbind(modis.match, st_coordinates(modis.match))
      
    }
    
    for(i in unique(modis.match$SiteCode)) {
      
      for(j in unique(modis.match$survey_year[modis.match$SiteCode == i])) {
        
        for(k in unique(modis.match$yday[modis.match$SiteCode == i & modis.match$survey_year == j])) {
          
          tmp <- filter(modis.match, SiteCode == i, survey_year == j, yday == k)
          
          suppressWarnings(
            
            if(!(j %in% modis.files$year)) {
              
              poss.files <- modis.files[modis.files$year == unique(modis.files$year)[which(j-unique(modis.files$year) == min(j-unique(modis.files$year)))] & modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y & modis.files$yday < tmp$yday & modis.files$endyday > tmp$yday,]
              
              modis.match[modis.match$SiteCode == i & modis.match$survey_year == j & modis.match$yday == k, "filename"] <- poss.files$filename[poss.files$productiondate == max(poss.files$productiondate)]
              
            } else {
              
              poss.files <- modis.files[modis.files$year == tmp$survey_year & modis.files$xmin < tmp$X & modis.files$xmax > tmp$X & modis.files$ymin < tmp$Y & modis.files$ymax > tmp$Y & modis.files$yday <= tmp$yday & modis.files$endyday > tmp$yday,]
              
              modis.match[modis.match$SiteCode == i & modis.match$survey_year == j & modis.match$yday == k, "filename"] <- poss.files$filename[poss.files$productiondate == max(poss.files$productiondate)]
              
            }
            
          )
          
        }
        
      }
      
    }
    
    spatial_set$yday <- paste0(spatial_set$survey_year, "-", spatial_set$survey_month, "-", spatial_set$survey_day) %>%
      as.Date() %>%
      yday()
    
    for(i in `if`("modis_ndvi" %in% covariates, `if`("modis_evi" %in% covariates, c("modis_ndvi", "modis_evi"), "modis_ndvi"), "modis_evi")) {
      
      message(paste0("Calculating MODIS ", ifelse(i == "modis_ndvi", "NDVI", "EVI"), "."))
      
      index <- ifelse(i == "modis_ndvi", "\"500m 16 days NDVI\"", "\"500m 16 days EVI\"")
      
      for(j in unique(modis.match$filename)) {
        
        pts_to_fill <- spatial_set[spatial_set$SiteCode %in% modis.match$SiteCode[modis.match$filename == j] & spatial_set$survey_year %in% modis.match$survey_year[modis.match$filename == j & spatial_set$yday %in% modis.match$yday[modis.match$filename == j]],]
        
        modis <- rast(j)[index]
        
        for(k in unique(pts_to_fill$SiteCode)) {
          
          if(buffer == TRUE) {
            
            tmp <- spatial_set %>%
              filter(SiteCode == k, survey_year %in% modis.match$survey_year[modis.match$filename == j]) %>%
              select(SiteCode, geometry) %>%
              distinct() %>%
              st_transform(crs(modis))
            
            modis_clip <- crop(modis, tmp)
            
            spatial_set[spatial_set$SiteCode == k & spatial_set$survey_year == modis.match$survey_year[modis.match$filename == j & modis.match$SiteCode == k] & spatial_set$yday %in% modis.match$yday[modis.match$filename == j & modis.match$SiteCode == k], ifelse(i == "modis_ndvi", "ndvi", "evi")] <- exact_extract(modis_clip, tmp, fun = "mean")
            
          } else {
            
            tmp <- spatial_set %>%
              filter(SiteCode == k, survey_year %in% modis.match$survey_year[modis.match$filename == j]) %>%
              select(SiteCode, geometry) %>%
              distinct() %>%
              st_transform(crs(modis)) %>%
              vect()
            
            spatial_set[spatial_set$SiteCode == k & spatial_set$survey_year == modis.match$survey_year[modis.match$filename == j & modis.match$SiteCode == k] & spatial_set$yday %in% modis.match$yday[modis.match$filename == j & modis.match$SiteCode == k], ifelse(i == "modis_ndvi", "ndvi", "evi")] <- terra::extract(modis, tmp, fun = "mean")[,index]
            
          }
        }
        
      }
      
      
      
      
    }
    
    if(retain == FALSE) {
      
      message(paste0("MODIS NDVI/EVI extraction complete. Removing files."))
      
      file.remove(modis.files$filename)
      
    }
    
    rm(modis.files) 
    
    spatial_set <- select(spatial_set, -yday)
    
  }
  
  # Next up: elevation from elevatr
  
  if("elevation" %in% covariates) {
    
    elev <- get_elev_raster(locations = spatial_set,
                            z = 7,
                            prj = st_crs(spatial_set),
                            src = "aws", # In future, check other sources. Others more appropriate for CDN users?
                            expand = ifelse(buffer == TRUE, 2*conv_unit(x = buffer_radius, from = buffer_units, to = "m"), 1000),
                            neg_to_na = TRUE, # Turn ocean tiles with negative elevation to NAs.
                            verbose = F) %>%
      rast()
    
    for(i in unique(spatial_set$SiteCode)) {
      
      tmp <- spatial_set %>%
        filter(SiteCode == i) %>%
        select(SiteCode, geometry) %>%
        distinct()
      
      if(buffer == FALSE) {
        
        spatial_set[spatial_set$SiteCode == i, "elevation"] <- terra::extract(x = elev,
                                                                              y = tmp,
                                                                              fun = "mean")[,names(elev)] # additionally specify weights if using weighted metric using 'weights' argument.
        
        
      } else {
        
        spatial_set[spatial_set$SiteCode == i, "elevation"] <- exact_extract(x = elev,
                                                                             y = tmp,
                                                                             fun = "mean",
                                                                             progress = FALSE) # additionally specify weights if using weighted metric using 'weights' argument.
        
      }
      
    }
    
    if(TRUE %in% is.na(spatial_set$elevation)) {
      
      warning("AWS Elevation: some points are close to shore, and so fall into cells with negative elevation (below sea level). For these cells, the nearest positive elevation has been used.")
      
      for(i in unique(spatial_set$SiteCode[is.na(spatial_set$elevation)])) {
        
        tmp <- spatial_set %>%
          filter(SiteCode == i) %>%
          select(SiteCode, geometry) %>%
          distinct() %>%
          st_buffer(2500)
        
        elev_crop <- crop(elev, vect(tmp)) %>%
          as.points()
        
        spatial_set$elevation[spatial_set$SiteCode == i] <- values(elev_crop[nearest(vect(tmp), elev_crop)$to_id])
        
      }
      
    }
    
  }
  
  # Climate variables from WorldClim
  
  if(length(grep("worldclim_", covariates)) > 0) {
    
    if(is.na(dl_path) & !dir.exists("./worldclim")) {
      
      dir.create("./worldclim", recursive = T)
      
    }
    
    if(!is.na(dl_path) & !dir.exists(paste0(dl_path, "/worldclim"))) {
      
      dir.create(paste0(dl_path, "/worldclim"), recursive = T)
      
    }
    
    clim_vars <- gsub(pattern = "worldclim_", replacement = "", grep("worldclim_", covariates, value = T))
    
    #### NEED TO CHECK THAT MONTH DATA IS VALID
    
    for(i in clim_vars) {
      
      if(!file.exists(ifelse(is.na(dl_path), paste0("./worldclim/climate/wc2.1_country/CAN_wc2.1_30s_", i, ".tif"), paste0(dl_path, "/worldclim/climate/wc2.1_country/CAN_wc2.1_30s_", i, ".tif")))) {
        
        clim <- worldclim_country(var = i, 
                                  country = "Canada", ### ADD WAY TO INCORPORATE OTHER COUNTRIES?
                                  res = 0.5,
                                  path = ifelse(is.na(dl_path), "./worldclim", paste0(dl_path, "/worldclim")))
        
      } else {
        
        clim <- rast(ifelse(is.na(dl_path), paste0("./worldclim/climate/wc2.1_country/CAN_wc2.1_30s_", i, ".tif"), paste0(dl_path, "/worldclim/climate/wc2.1_country/CAN_wc2.1_30s_", i, ".tif")))
        
      }
      
      clim <- crop(clim, project(study_area, crs(clim))) %>%
        project(crs(spatial_set))
      
      for(j in unique(spatial_set$SiteCode)) {
        
        tmp <- spatial_set %>%
          filter(SiteCode == j) %>%
          select(SiteCode, survey_month, geometry) %>%
          distinct()
        
        for(k in unique(spatial_set$survey_month[spatial_set$SiteCode == j])) {
          
          if(buffer == TRUE) {
            
            spatial_set[spatial_set$SiteCode == j & spatial_set$survey_month == k, i] <- exact_extract(x = clim[[paste0("CAN_wc2.1_30s_", i, "_", k)]], 
                                                                                                       y = tmp %>% filter(survey_month == k), 
                                                                                                       fun = "mean")
            
          } else {
            
            spatial_set[spatial_set$SiteCode == j & spatial_set$survey_month == k, i] <- terra::extract(x = clim[[paste0("CAN_wc2.1_30s_", i, "_", k)]], 
                                                                                                        y = tmp %>% filter(survey_month == k), 
                                                                                                        fun = "mean", na.rm = TRUE)[,paste0("CAN_wc2.1_30s_", i, "_", k)]
            
          }
          
          
        }
        
      }
      
      if(TRUE %in% is.na(spatial_set[,i])) {
        
        warning(paste0("WorldClim [", i, "]: some points are close to shore, and so fall outside of raster coverage. For these cells, the nearest cell value has been used."))
        
        for(j in unique(spatial_set$SiteCode[is.na(spatial_set[,i])])) {
          
          for(k in unique(spatial_set$survey_month[spatial_set$SiteCode == j])) {
            
            tmp <- spatial_set %>%
              filter(SiteCode == j, survey_month == k) %>%
              select(SiteCode, survey_month, geometry) %>%
              distinct() %>%
              st_buffer(2500)
            
            clim_crop <- crop(clim[[paste0("CAN_wc2.1_30s_", i, "_", k)]], vect(tmp)) %>%
              as.points()
            
            spatial_set[spatial_set$SiteCode == j & spatial_set$survey_month == k, i] <- values(clim_crop[nearest(vect(tmp), clim_crop)$to_id])
            
          }
          
        }
        
      }
      
    }
    
    if(retain == FALSE) {
      
      message(paste0("WorldClim extraction complete. Removing files."))
      
      file.remove(list.files(ifelse(is.na(dl_path), "./worldclim", paste0(dl_path, "/worldclim")), full.names = T))
      
    }
    
  }
  
  data <- left_join(data, spatial_set %>% st_drop_geometry(), by = c("SiteCode", "survey_year", "survey_month", "survey_day"))
  
  return(data)
  
}

out <- nc_covariates(land.dat, data_type = "df", covariates = c("worldclim_tavg", "worldclim_prec"), buffer = TRUE, buffer_radius = 500, buffer_units = "m",
                     ed_login = ed_login, ed_password = ed_pw, retain = TRUE)
