############################################################################
#################### PREPARE SDMS FILES AND FOLDERS ########################
############################################################################

# Started on 28.03.2026
# modified on 08.06.2026
# finalized on 28.07.2026
# by LT

# this is a script to download and adjust the variables used in the SDMs  
# based on wiSDM 2.0
# for OneSTOP Task5.4

# Setting up the R environment
rm(list=ls())
setwd("/lisc/data/scratch/botany/tedeschi/Work/oneSTOP") # adjust as needed
getwd()

# Load packages
pacman::p_load(tidyverse, terra, curl, zen4R, rnaturalearth, devtools, sf)

if ("rnaturalearthhires" %in% rownames(installed.packages())) {
  library(rnaturalearthhires)
} else {
  devtools::install_github("ropensci/rnaturalearthhires")
  library(rnaturalearthhires)
}

# Define paths
# adjust as needed
source_path <- paste0(getwd(), "/scripts")
database_path <- "/lisc/data/work/botany/tedeschi/Work/databases" 
output_path <- paste0(getwd(), "/outputs")

# Load functions
# adjust as needed
source(file.path(source_path, "task5.1", "aux_funs.R"))
source(file.path(source_path, "wiSDM_v02", "helper_functions.R"))

# Set terra options
options("rgdal_show_exportToProj4_warnings" = "none")
terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE") #Prevent terra from writing aux.xml files
terraOptions(
  memfrac = 0.3,
  tempdir = file.path(tempdir()),
  todisk = TRUE)

# Specify project name and version for the input data
project_version <- "v02"
project_projection <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#            1. LOAD DATA           #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 


#-------------------------------------------------
#--------------- Create folders ---------------
#-------------------------------------------------

# Define the folder paths
habitat_folder <- paste0(database_path, "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3")
chelsa_current_folder <- paste0(database_path, "/CHELSA_V2.1/chelsa_current")
chelsa_mask_folder <- paste0(database_path, "/CHELSA_V2.1/chelsa_mask")
biasgrids_folder <- paste0(database_path, "/wiSDM_biasgrids/processed/")

# Store in a vector
folders <- c(habitat_folder, chelsa_current_folder, biasgrids_folder)

# Check and create each folder if necessary
for(folder in folders){
  if(!dir.exists(folder)) dir.create(folder, recursive=TRUE)
}

options(timeout = 600) #set time-out to 10 min



#-------------------------------------------------
#--------- Store the CHELSA layers  --------------
#-------------------------------------------------

for(i in c("1", "4", "5", "6","7", "12","13","14","15")){

  # Define CHELSA layer name
  layer_name <- switch(i,
                       "1" = "meantemp",
                       "4" = "temp_seasonality",
                       "5" = "maxTmpWarmestMon",
                       "6"= "minTmpColdestMon",
                       "7"="temp_annRange",
                       "12"="annPrecip",
                       "13"="precipWettestMon",
                       "14"="precipDriestMon",
                       "15"="precipSeasonality")

  destfile <- here::here(chelsa_current_folder,paste0("CHELSA_",layer_name,"_",i,".tif"))

  if( update_files_logic(dest_file = destfile,
                         update_files = update_files)){
    if(grepl("windows", Sys.getenv("OS"), ignore.case = TRUE)) {
      download.file(url = paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio",i,"_1981-2010_V.2.1.tif"),
                    mode = "wb",
                    destfile = destfile)
    }else{
      download.file(url = paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio",i,"_1981-2010_V.2.1.tif"),
                    destfile = destfile)
    }
  }
}



#--------------------------------------------------------------------
#----- Store CHELSA v1 layer as mask template for marine pixels  ----
#--------------------------------------------------------------------

# Download a V1 chelsa layer (check for marine pixels: none seem to be present)
destfile <- here::here(chelsa_mask_folder, paste0("CHELSA_meantemp1.tif"))

if(update_files_logic(dest_file = destfile,
                      update_files = update_files)){
  if(grepl("windows", Sys.getenv("OS"), ignore.case = TRUE)) {
    download.file(url = paste0("https://os.zhdk.cloud.switch.ch/chelsav1/climatologies/bio/CHELSA_bio10_01.tif"),
                  mode = "wb",
                  destfile = destfile)
  }else{
    download.file(url = paste0("https://os.zhdk.cloud.switch.ch/chelsav1/climatologies/bio/CHELSA_bio10_",i,".tif"),
                  destfile = destfile)
  }
}

chelsa_mask <- terra::rast(destfile)


 
#-------------------------------------------------
#----- Scale and mask current CHELSA layer  ------
#-------------------------------------------------

# List files
chelsa_current <- list.files(here::here(chelsa_current_folder),
                             pattern = "^CHELSA_.*\\.tif$",
                             full.names = TRUE)


for (i in seq_along(chelsa_current)) {
  file <- chelsa_current[i]
  layer_name <- sub("\\.tif$", "", basename(file))
  out_name <- here::here(chelsa_current_folder, paste0("scaled_layer_", layer_name, ".tif"))
  
  
  if (update_files == "yes") {
    do_scale <- TRUE
  } else if (update_files == "no") {
    do_scale <- !file.exists(out_name)
  } else if (update_files == "ask") {
    if (file.exists(out_name)) {
      msg <- paste0("Scaled file\n", basename(out_name),
                    "\n already exists. Create and overwrite it again?")
      do_scale <- utils::askYesNo(msg)
    } else {
      do_scale <- TRUE
    }
  }
  
  if (isTRUE(do_scale)) {
    print(paste0("Processing ", layer_name)) 
    
    #Mask raster 
    masked_r<- terra::mask(terra::rast(file), chelsa_mask) 
    
    #Obtain mean and sd of raster and use for scaling 
    m<-global(masked_r,"mean",na.rm=TRUE)$mean 
    s<-global(masked_r,"sd",na.rm=TRUE)$sd 
    scaled_r<-(masked_r - m) / s 
    
    #Round the scaled layer 
    scaled_r<-terra::round(scaled_r, 2) 
    
    #Assign name to raster 
    names(scaled_r)<-layer_name 
    
    #Convert units of temp seasonality layer to °C: not necessary when you scale afterwards 
    # if(names(rast_file) == "CHELSA_temp_seasonality_4"){ 
    # rast_file <- rast_file/100 
    # print("Converted the unit of layer bio 4 (temperature seasonality) to °C") 
    # } 
    
    # Write raster to disk 
    out_name <- here::here(chelsa_current_folder, paste0("scaled_layer_", layer_name,".tif")) 
    terra::writeRaster(scaled_r, filename = out_name, overwrite = TRUE) 
    
    #Print write statement 
    print(paste0("Created rasterlayer ", basename(out_name)," in folder ", basename(chelsa_current_folder))) 
    
    #Clean up 
    rm(masked_r, m, s, scaled_r, layer_name, out_name) 
    gc()
  } 
}



#---------------------------------------------------------
#---------- Store future CHELSA layers  ---------
#---------------------------------------------------------

chelsa_current_layers <- list.files(chelsa_current_folder)[grepl("scaled", list.files(chelsa_current_folder))]
chelsa_current_layers <- sub("_1981-2010.*$", "", chelsa_current_layers)

# Note that there are values missing in all future layers of Precipitation driest month (bio14), this is not a problem as they fall outside of the EU!
for (period in c("2041-2070","2071-2100")) {
  for (scenario in c("ssp126","ssp370","ssp585")) {
    
    future_folder <- file.path(database_path, "CHELSA_V2.1", "chelsa_future", period, scenario)
    if(!dir.exists(future_folder)) dir.create(future_folder, recursive=TRUE)
    
    dest_files <- data.frame(
      file = c(
        paste0("scaled_layer_CHELSA_meantemp_1_",        period, "_", scenario, ".tif"),
        paste0("scaled_layer_CHELSA_temp_seasonality_4_",period, "_", scenario, ".tif"),
        paste0("scaled_layer_CHELSA_maxTmpWarmestMon_5_",period, "_", scenario, ".tif"),
        paste0("scaled_layer_CHELSA_minTmpColdestMon_6_",period, "_", scenario, ".tif"),
        paste0("scaled_layer_CHELSA_temp_annRange_7_",   period, "_", scenario, ".tif"),
        paste0("scaled_layer_CHELSA_annPrecip_12_",      period, "_", scenario, ".tif"),
        paste0("scaled_layer_CHELSA_precipWettestMon_13_",period,"_", scenario, ".tif"),
        paste0("scaled_layer_CHELSA_precipDriestMon_14_", period,"_", scenario, ".tif"),
        paste0("scaled_layer_CHELSA_precipSeasonality_15_",period,"_",scenario,".tif")
      ),
      update_file = NA,
      stringsAsFactors = FALSE
    )
    
    dest_files <- update_files_logic(dest_file = dest_files,
                                     dest_folder = future_folder,
                                     update_files = update_files) %>% 
      dplyr::pull(file)
    
    if(length(dest_files)>0){
      safe_download_zenodo(
        doi = "10.5281/zenodo.17724735",
        path = future_folder,
        files = dest_files,
        timeout=600,
        quiet = FALSE
      )
    }
    
    #Check if file is not corrupt, if so, redownload
    for(file in dest_files){
      read_or_redownload(file=file, 
                         folder = future_folder,
                         doi =  "10.5281/zenodo.17724735")
    }
  }
}


#-------------------------------------------------
#-- Store habitat layers for the European model --
#-------------------------------------------------

# not used in Task5.4 as we use Chen et al. 2022 data

# dest_files <- data.frame(
#   file = c("Agriculture.tif",
#            "Artificial.tif",
#            "Coastal_wetland.tif",
#            "Coniferous_forest.tif",
#            "Deciduous_forest.tif",
#            "Inland_wetland.tif",
#            "Mixed_forest.tif",
#            "Shrub_and_herbaceous.tif",
#            "log_distance_to_water.tif",
#            "log_total_water_length.tif",
#            "proportion_total_water_polygon_cover.tif"),
#   update_file = NA,
#   stringsAsFactors = FALSE
# )
# 
# dest_files <- update_files_logic(dest_file = dest_files,
#                                  dest_folder = habitat_folder,
#                                  update_files = update_files) %>% 
#   dplyr::pull(file)
# 
# if(length(dest_files)>0){
#   zen4R::download_zenodo(doi = "10.5281/zenodo.17724735", 
#                          path = habitat_folder, 
#                          files = dest_files,
#                          timeout = 600,
#                          quiet=FALSE)
# }
# 
# #Check if file is not corrupt, if so, redownload
# for(file in dest_files){
#   read_or_redownload(file = file, 
#                      folder = habitat_folder,
#                      doi =  "10.5281/zenodo.17724735")
# }



#-------------------------------------------------
#----- Store the country boundary shapefile  -----
#-------------------------------------------------

# not needed in Task 5.4 as we work in EU

# #This may take some time!
# if(tolower(country_of_interest)!="europe"||!is.null(custom_country_boundary_path)){
#   if(is.null(custom_country_boundary_path)){
#     country <- rnaturalearth::ne_countries(country=country_of_interest, scale=10)[1]
#   }else{
#     country <- sf::st_read(custom_country_boundary_path)%>%
#       st_transform(crs=4326)
#   }
#   country_vector <- terra::vect(country) #Convert to a SpatVector, used for masking
#   country_ext <- terra::ext(country_vector) 
#   sf::write_sf(country, here::here(country_folder,"country.shp"))
# }



#-----------------------------------------------
#------- Process global climate rasters --------
#-----------------------------------------------

# Only include files that start with "scaled_layer_" and end with .tif: 
scaled_files <- list.files(chelsa_current_folder,
                           pattern = "^scaled_layer.*\\.tif$",
                           full.names = TRUE)

# Load and stack
globalclimpreds_terra <- terra::rast(scaled_files)
invisible(gc())

# Remove NA pixels and mask to European ext
# This ensures all rasters have the same NA structure
na_mask_globalclimpreds_terra <- anyNA(globalclimpreds_terra)
globalclimpreds_terra  <- terra::mask(
  globalclimpreds_terra,
  na_mask_globalclimpreds_terra,
  maskvalue = 1)

# Write to disk with compression
processed_folder <- file.path(database_path, "CHELSA_V2.1", "chelsa_current")
if(!dir.exists(processed_folder)) dir.create(processed_folder)

globalclimpreds_file <- file.path(processed_folder, "globalclimpreds.tif")

# reproject to project projection
globalclimpreds_terra_cea <- terra::project(globalclimpreds_terra, crs(euboundary))

terra::writeRaster(globalclimpreds_terra_cea,
                   filename = globalclimpreds_file,
                   overwrite = TRUE,
                   wopt = list(gdal = c("COMPRESS=LZW")))

rm(na_mask_globalclimpreds_terra)

gc()


#---------------------------------------------------------
#---Create CHELSA predictors at 5k res. for background selection
#---------------------------------------------------------

# not needed in  Task 5.4 as we do models at 1x1km

# #Decrease resolution to match coordinate uncertainty of global occurrences: use around 5km at equator by averaging
# globalclimpreds_terra_5k <- terra::aggregate(globalclimpreds_terra[[1]], fact = 5, fun = mean, na.rm = TRUE)
# 
# # Write to disk with compression
# globalclimpreds_5k_file <- file.path(processed_folder,"globalclim_5k.tif")
# terra::writeRaster(globalclimpreds_terra_5k,
#                    filename = globalclimpreds_5k_file ,
#                    overwrite = TRUE,
#                    wopt = list(gdal = c("COMPRESS=LZW")))
# 
# 
# rm(globalclimpreds_terra_5k)
# gc() 


#-------------------------------------------------
#---------------- Store biasgrids  ---------------
#-------------------------------------------------

dest_files <- data.frame(
  file = c("log_amphibians_1degree_layer.tif",
           "log_birds_1degree_layer.tif",
           "log_fish_1degree_layer.tif",
           "log_hydrozoa_1degree_layer.tif",
           "log_insects_1degree_layer.tif",
           "log_malacostraca_1degree_layer.tif",
           "log_mammals_1degree_layer.tif",
           "log_mollusca_1degree_layer.tif",
           "log_plants_1degree_layer.tif",
           "log_reptiles_1degree_layer.tif"),
  update_file = NA,
  stringsAsFactors = FALSE
)

dest_files <- update_files_logic(dest_file = dest_files,
                                 dest_folder = biasgrids_folder,
                                 update_files = update_files) %>%
  dplyr::pull(file)

if(length(dest_files)>0){
  zen4R::download_zenodo(doi="https://doi.org/10.5281/zenodo.17724735",
                         path=biasgrids_folder,
                         files=dest_files,
                         timeout = 600,
                         quiet=FALSE)
}

#Check if file is not corrupt, if so, redownload
for(file in dest_files){
  read_or_redownload(file = file,
                     folder = biasgrids_folder,
                     doi =  "10.5281/zenodo.17724735")
}



#-------------------------------------------------
#---------------- Process biasgrids  ---------------
#-------------------------------------------------

# avoid doing this for every species by doing it once here
# and store the processed biasgrids

# Load CHELSA global stack of bioclimatic variables with NAs removed
processed_folder <- paste0(database_path, "/CHELSA_V2.1/chelsa_current")
globalclimpreds_file <- file.path(processed_folder, "/globalclimpreds.tif")
globalclimpreds_terra <- terra::rast(globalclimpreds_file)

# process and store biasgrids

bias_grid_paths <- list(
  Molluscs = paste0(database_path, "/wiSDM_biasgrids/processed/Molluscs.tif"),
  Mammals = paste0(database_path, "/wiSDM_biasgrids/processed/Mammals.tif"),
  Insects = paste0(database_path, "/wiSDM_biasgrids/processed/Insects.tif"),
  Birds = paste0(database_path, "/wiSDM_biasgrids/processed/Birds.tif"),
  Amphibians = paste0(database_path, "/wiSDM_biasgrids/processed/Amphibians.tif"),
  Reptiles = paste0(database_path, "/wiSDM_biasgrids/processed/Reptiles.tif"),
  Plants = paste0(database_path, "/wiSDM_biasgrids/processed/Plants.tif"))

for(speciesgroup in names(bias_grid_paths)) {
  
  biasgrid_group <- terra::rast(bias_grid_paths[[speciesgroup]])
  biasgrid_group <- terra::project(biasgrid_group, globalclimpreds_terra[[1]], method = "bilinear")
  
  # Resample biasgrid to match the resolution of globalclimpreds_terra
  biasgrid_group <- terra::resample(biasgrid_group, globalclimpreds_terra[[1]], method = "bilinear")
  
  writeRaster(biasgrid_group,
              paste0("../databases/wiSDM_biasgrids/processed/", speciesgroup, ".tif"),
              overwrite = T)
}


#--------------------------------------------
#----------- Load boundary layers -----------
#--------------------------------------------

# euboundary <- terra::rast(file.path("data", "external", "habitat", "Agriculture.tif"))%>%
#   terra::project(globalclimpreds_terra[[1]])%>%
#   terra::crop(terra::ext(-38, 50,  24.29152732065, 72.66652712715))
# 
# if(tolower(country_of_interest)!="europe"){
#   country_boundary<-sf::read_sf(here::here("data","external","GIS","Country","country.shp"))%>%
#     sf::st_transform(crs(globalclimpreds_terra))%>%
#     terra::vect()
# }else{
#   country_boundary<-euboundary
# }

lulc_folder <- paste0(database_path, "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3")
euboundary <- terra::rast(paste0(lulc_folder, "/", list.files(lulc_folder)[1]))



#-----------------------------------------------
#-------- Create European climate layers -------
#-----------------------------------------------

# Crop and mask scaled_stack to European extent
# eu_climpreds.10 <- globalclimpreds_terra %>%
#   terra::crop(euboundary) %>%
#   terra::mask(euboundary)

# Crop scaled_stack to European extent
# NB here if I mask the global predictors to the european land-cover, 
# it excludes areas where the land-cover has no value,
# such as the sea (desired) but also islands like Malta or areas like the Alps!
# so it's better to crop using the extent of the file and not masking
# (you can always mask the SDMs projections afterwards)

globalclimpreds_terra_cea <- terra::project(globalclimpreds_terra, crs(euboundary))
gc()

eu_climpreds.10 <- globalclimpreds_terra_cea %>%
  terra::crop(ext(euboundary)) 

gc()

# Write to disk with compression
processed_folder <- file.path(database_path, "CHELSA_V2.1", "chelsa_current")
if(!dir.exists(processed_folder)) dir.create(processed_folder)

eu_climpreds_file <- file.path(processed_folder, "euclimpreds.tif")

terra::writeRaster(eu_climpreds.10,
                   filename = eu_climpreds_file,
                   overwrite = TRUE,
                   wopt = list(gdal = c("COMPRESS=LZW")))

# Clean up
rm(eu_climpreds.10)
invisible(gc())



#--------------------------------------------
#--------Create country climate layers -------
#--------------------------------------------

# not needed in Task 5.4 as we work in EU

# if(tolower(country_of_interest)!="europe"){
#   country_climpreds <- terra::crop(globalclimpreds_terra, country_boundary)
#   country_climpreds <- terra::mask(country_climpreds, country_boundary)
#   
#   # Write to disk with compression
#   country_climpreds_file <- file.path(processed_folder, "country_climpreds.tif")
#   terra::writeRaster(country_climpreds,
#                      filename = country_climpreds_file,
#                      overwrite = TRUE,
#                      wopt = list(gdal = c("COMPRESS=LZW")))
#   
#   rm(country_climpreds)
#   gc()
# }else{
#   country_climpreds_file<-eu_climpreds_file
# }



#---------------------------------------------
#------ Load future climate rasters ----------
#---------------------------------------------

for (period in c("2041-2070","2071-2100")){
  for(scenario in c("ssp126", "ssp370", "ssp585")){

    # List future raster files
    future_files <- list.files(file.path(database_path, "CHELSA_V2.1", "chelsa_future", period,scenario),
                               pattern = "^scaled_layer.*\\.tif$", full.names = TRUE)

    # Stack them together
    future_stack <- terra::rast(future_files)
    
    # Reproject
    future_stack <- terra::project(future_stack, project_projection)

    # #Aggregate at a resolution of 5km
    # future_stack <- terra::aggregate(future_stack, fact=5, fun = mean, na.rm=TRUE)

    # Resample
    future_stack <- terra::resample(future_stack, euboundary, method = "bilinear")
    
    # Mask future stack to resolution of country or region of interest
    future_country <- terra::crop(future_stack, ext(euboundary))
    future_country <- terra::mask(future_country, ext(euboundary))

    # Define preprocessed dir
    preprocessed_dir <- file.path(database_path, "CHELSA_V2.1", "chelsa_future","europe", period, scenario)
    if(!dir.exists(preprocessed_dir)) dir.create(preprocessed_dir, recursive=TRUE)

    # Define output file
    out_file <- file.path(preprocessed_dir, paste0(period, "_", scenario, "_masked.tif"))

    # Save processed rasterstack
    terra::writeRaster(future_country,
                       filename = out_file,
                       overwrite = TRUE,
                       wopt = list(gdal = c("COMPRESS=LZW")))

 # Clean up
    rm(future_country, future_stack)
    gc()

  }
}



#--------------------------------------------
#-------- Load European habitat rasters -----
#--------------------------------------------

# # Load all habitat rasters
# habitat_files <- list.files(file.path("./data/external/habitat"), pattern = 'tif$', full.names = TRUE)
# habitat_rasters <- lapply(habitat_files, terra::rast)
# 
# # compute common intersection extent across all rasters
# common_ext <- Reduce(intersect, lapply(habitat_rasters, ext))
# 
# # Crop all rasters to the common (smallest) extent
# habitat_rasters <- lapply(habitat_rasters, terra::crop, common_ext)
# 
# # Combine into raster stack 
# habitat_stack <- terra::rast(habitat_rasters)
# rm(habitat_rasters)
# 
# #Scale habitat rasters
# habitat_stack <- terra::scale(habitat_stack, center = TRUE, scale = TRUE)

# Define habitat (lulc) rasters
lulc_periods <- c(
  "2055",
  "2085"
)

lulc_scenarios <- c(
  "SSP1_RCP26",
  "SSP3_RCP70",
  "SSP5_RCP85"
)

# Load EUROPEAN historical LULC raster
habitat_stack <- list()

hist_lu_rst <- rast(paste0(database_path, 
                           "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3/global_PFT_2015_recl_eur_mw3.tif"))

# Stored values are scaled integers: value = round(percent * 100).
# To get percent back: percent = value / 100.
# To get proportions: proportions = value / 10000.

habitat_stack[["2015"]] <- hist_lu_rst/10000

# Load future LULC raster

lulc_rst_paths <- list.files(paste0(database_path,
                                    "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3"),
                             pattern = "\\.tif$",
                             full.names = TRUE,
                             recursive = TRUE)

for(i in 1:length(lulc_periods)){
  
  period <- lulc_periods[i]
  
  for(j in 1:length(lulc_scenarios)){
    
    scenario <- lulc_scenarios[j]
    
    period_scenario_code <- gsub("\\-","_",paste(period, scenario,sep="_"))
    
    ### ---- Get raster file paths ---- ###
    lulc_fut_rst_paths <-  match_patterns("_recl_eur_mw3.tif$",
                                          lulc_periods[i],
                                          lulc_scenarios[j],
                                          lulc_rst_paths)
    
    ### ---- Load LULC data ---- ###
    lu_fut_rst <- rast(lulc_fut_rst_paths)
    lu_fut_rst <- lu_fut_rst/10000
    
    ### ---- Merge the LULC rasters in the scenarios list ---- ###
    
    habitat_stack[[period_scenario_code]] <- c(lu_fut_rst)
    
    message("Finished loading EUROPEAN LULC future raster data for: ", period_scenario_code)
    rm(lu_fut_rst)
    
  }
  rm(scenario)
  rm(period_scenario_code)
  rm(lulc_fut_rst_paths)
}

invisible(gc())

habitat_stack



#---------------------------------------------
#----- Remove NA pixels from predictors ------
#---------------------------------------------

# This is to avoid that some layers have NA while others have values in certain pixels
habitat_stack_no_nas <- list()

for(layer in names(habitat_stack)){
  na_mask_habitat_stack <- anyNA(habitat_stack[[layer]])
  habitat_stack_no_nas[[layer]] <- terra::mask(habitat_stack[[layer]], na_mask_habitat_stack, maskvalue=1)
  rm(na_mask_habitat_stack)
  gc()
}

terra::writeRaster(rast(habitat_stack_no_nas),
                     filename = "/lisc/data/work/botany/tedeschi/Work/databases/Chen_et_al_2022/habitat_stack_no_nas.tif",
                     overwrite = TRUE,
                     wopt = list(gdal = c("COMPRESS=LZW")))



#---------------------------------------------
#----------- Store habitat layers ------------
#---------------------------------------------

# processed_folder<-file.path("data", "external", "habitat", "processed")
# if(!dir.exists(processed_folder)) dir.create(processed_folder)
# habitatstack_file <- file.path(processed_folder, "habitat_stack.tif")
# 
# terra::writeRaster(habitat_stack,
#                    filename = habitatstack_file,
#                    overwrite = TRUE,
#                    wopt = list(gdal = c("COMPRESS=LZW")))

# Write to disk with compression
processed_folder <- paste0(database_path, "/Chen_et_al_2022")

habitatstack_file <- file.path(processed_folder, "habitat_stack.tif")

terra::writeRaster(terra::rast(habitat_stack),
                   filename = habitatstack_file,
                   overwrite = TRUE,
                   wopt = list(gdal = c("COMPRESS=LZW")))

# Clean up
rm(processed_folder)
rm(habitat_stack)
invisible(gc())


#--------------------------------------------
#---------- Clean R environment --------------
#--------------------------------------------

rm(list = ls())
