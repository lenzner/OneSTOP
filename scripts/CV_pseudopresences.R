#############################################################
#################### SPATIAL CV VALIDATION ##################
#############################################################

# Started on 20.04.2026
# finalized on 28.07.2026
# by LT

# this is a script to validate all the models 
# (climate, habitat, and combined models)
# using the new wiSDM version
# using pseudo-presences sampled from IUCN and AOH
# based on oneSTOP Task5.1 scripts
# and wiSDM_v02

# Setting up the R environment
rm(list=ls())
setwd("/lisc/data/scratch/botany/tedeschi/Work/oneSTOP") # adjust as needed
getwd()

# Load packages
library(tictoc)
library(qs2)
library(progressr)
library(rnaturalearthdata)
library(tidyverse)
library(caret)
library(rJava)
library(grid)
library(tidyterra)
library(RStoolbox)
library(sdm)
library(sf)
library(terra)
library(blockCV)

# Define paths
source_path <- paste0(getwd(), "/scripts")
database_path <- "/lisc/data/work/botany/tedeschi/Work/databases"
output_path <- file.path(getwd(), "outputs", "runs", "run02")

# Load functions
source(file.path(source_path, "task5.1", "aux_funs.R"))
source(file.path(source_path, "wiSDM_v02", "helper_functions.R"))
source(file.path(getwd(), "slurm", "runs", "run02", "pseudopresences", "configurations_pseudopresences.R"))

# Assign species name
args = commandArgs(trailingOnly = TRUE)
print(args)
sp <- args[1]
sp <- sub("_", " ", sp)

message("========= Processing species ", sp, " for CV validation with pseudo-presences")

# Set R tempdir
tmpdir <- args[2]

if (!is.null(tmpdir)) {
  Sys.setenv(TMPDIR = tmpdir)
  tempdir <- tmpdir
}

# Set terra options
options("rgdal_show_exportToProj4_warnings" = "none")
terra::setGDALconfig("GDAL_PAM_ENABLED", "FALSE") #Prevent terra from writing aux.xml files
terraOptions(
  memfrac = 0.3,
  tempdir = file.path(tempdir()),
  todisk = TRUE)

start_time <- Sys.time()



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#             LOAD DATA             #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 


#-------------------------------------------------------------------------------
#-------------------------- Define file paths ----------------------------------
#-------------------------------------------------------------------------------

# Global climate stack
processed_folder <- paste0(database_path, "/CHELSA_V2.1/chelsa_current")
climate_path <- file.path(processed_folder, "globalclimpreds.tif")

# EU climate stack
eu_climpreds_path <- paste0(processed_folder, "/euclimpreds.tif")

# EU habitat stack
habitat_path <- paste0(database_path, "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3/global_PFT_2015_recl_eur_mw3.tif")

# Biomes/ecoregions
if(pseudo_absences_background == "continents"){
  
  # this is how I upload the continents
  files <- list.files(paste0(database_path, "/natural_earth/continents/processed/"))
  continents <- c()
  
  for(c in files){
    
    x <- terra::vect(paste0(database_path, "/natural_earth/continents/processed/", c))
    
    # aggregate countries so the whole continent is together
    x <- terra::aggregate(x)
    
    # reproject
    x <- terra::project(x, project_projection)
    x <- terra::makeValid(x)
    continents <- terra::vect(c(x, continents))
    
  }
  
  continents <- terra::project(continents, 
                               project_projection)
  
  # check CRS of continents
  crs_env <- crs(continents, proj = T)
  
  if(crs_env != project_projection){
    
    warning("Continent vectors are in a different CRS than the one you selected!")
    
  }
  
}else if(pseudo_absences_background == "wwf_ecoregions"){
  
  # this is how we uploaded the WWF ecoregions in the old version
  wwf_eco <- sf::st_read(paste0(database_path, ("/wwf_terrestrial_ecoregions/data/commondata/data0/wwf_terr_ecos.shp")))
  
  # Project the data to the same CRS as the predictors raster stack
  wwf_eco <- sf::st_transform(wwf_eco, st_crs(project_projection)) %>%
    sf::st_make_valid()
  
  # check CRS of WWF ecoregions
  crs_env <- crs(wwf_eco, proj = T)
  
  if(crs_env != project_projection){
    
    warning("WWF ecoregions vectors are in a different CRS than the one you selected!")
    
  }
  
}else if(pseudo_absences_background == "biomes"){
  
  # load biomes
  wwf_eco <- sf::st_read(file.path(database_path, "wwf_terrestrial_ecoregions", "newRealms_cea.gpkg"))
  
  # check CRS of biomes
  crs_env <- crs(wwf_eco, proj = T)
  
  if(crs_env != project_projection){
    warning("WWF ecoregions vectors are in a different CRS than the one you selected!")
  }
}


#--------------------------------------------
#--- Load global occurrences and taxa info---
#--------------------------------------------

# NB decide if you want to upload the WGS84 (lat/lon) or CEA WGS84 occurrences
if(project_projection == "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"){
  
  global <- read.csv(paste0("./data/processed/range_sampling/global.occ.cea.wgs84.LL_", project_version, ".csv"))
  
}else if(project_projection == "EPSG:4326"){
  
  global <- read.csv(paste0("./data/processed/range_sampling/global.occ.wgs84.LL_", project_version, ".csv"))
  
}



#--------------------------------------------
#------- Split dataframe by taxonkey --------
#--------------------------------------------

split_df <- global %>% filter(species == sp)

species <- sp

# if there is a value in acceptedUsageKey, use that one
# otherwise use speciesKey
if(!is.na(unique(split_df$acceptedUsageKey))){
  
  message(sp, " is a synonym - using accepted taxon key...")
  taxonkey <- unique(split_df$acceptedUsageKey)
  
}else{
  
  taxonkey <- unique(split_df$speciesKey)
  
}

speciesName <- sub("^(\\w+)\\s+(\\w+).*", "\\1_\\2", species)  # Extract first two words of species name
speciesgroup <- unique(split_df$Group)

message(paste0("Starting number of pseudo-presences for ", sp, ": ", nrow(split_df)))

rm(global)



#--------------------------------------------
#------------ Load euboundary  --------------
#--------------------------------------------

lulc_folder <- paste0(database_path, "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3")
euboundary <- terra::rast(paste0(lulc_folder, "/", list.files(lulc_folder)[1]))

if(tolower(country_of_interest)!="europe"){
  country_boundary<-sf::read_sf(here::here("data","external","GIS","Country","country.shp"))%>%
    sf::st_transform(crs(chelsa_example_raster))%>%
    terra::vect()
}else{
  country_boundary<-euboundary
}


#--------------------------------------------
#----------------- Load rasters -------------
#--------------------------------------------

# Load rasters 
climate_stack <- terra::rast(climate_path)

# assign correct names
climate_stack <- climate_stack %>% 
  tidyterra::rename("CHELSA_meantemp_1" = "CHELSA_bio1_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_temp_seasonality_4" = "CHELSA_bio4_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_maxTmpWarmestMon_5" = "CHELSA_bio5_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_minTmpColdestMon_6" = "CHELSA_bio6_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_temp_annRange_7" = "CHELSA_bio7_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_annPrecip_12" = "CHELSA_bio12_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipWettestMon_13" = "CHELSA_bio13_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipDriestMon_14" = "CHELSA_bio14_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipSeasonality_15" = "CHELSA_bio15_1981-2010_V.2.1") 

habitat_stack <- terra::rast(habitat_path)
# get %
habitat_stack <- habitat_stack/10000

eu_climate_stack <- terra::rast(eu_climpreds_path) 

# assign correct names
eu_climate_stack <- eu_climate_stack %>% 
  tidyterra::rename("CHELSA_meantemp_1" = "CHELSA_bio1_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_temp_seasonality_4" = "CHELSA_bio4_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_maxTmpWarmestMon_5" = "CHELSA_bio5_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_minTmpColdestMon_6" = "CHELSA_bio6_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_temp_annRange_7" = "CHELSA_bio7_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_annPrecip_12" = "CHELSA_bio12_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipWettestMon_13" = "CHELSA_bio13_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipDriestMon_14" = "CHELSA_bio14_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipSeasonality_15" = "CHELSA_bio15_1981-2010_V.2.1") 

if( sum(names(climate_stack) != names(eu_climate_stack)) > 0){
  warning("Global CHELSA layer names and European layer names do not match!")
}



#-----------------------------------------------------------
#---------------- Sample background data once --------------
#-----------------------------------------------------------

# Extract a subsample of European pixels for Boyce calculation 
set.seed(728)

eu_subsample <- terra::spatSample(
  habitat_stack[[1]],
  size = boyce_background_size, 
  method = "random", 
  na.rm = TRUE, # Ignore NA pixels
  as.points = TRUE)

# Extract climate data at eu subsample points
eu_climate_sub <- terra::extract(eu_climate_stack, eu_subsample, ID = FALSE, xy = FALSE)

# Extract habitat data at eu subsample points
eu_habitat_sub <- terra::extract(habitat_stack, eu_subsample, ID = FALSE, xy = FALSE)


#----------------------------------------------
#----------- Define results directory ---------
#----------------------------------------------

# Define base project folder and create if necessary
base_dir <- file.path(output_path, projectname, paste0(speciesName, "_", taxonkey))
validation_dir <- file.path(base_dir, "Model_validation")
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
validation_summary <- list()



# - # - # - # - # - # - # - # - # - # - # - # - # 
#                                               #
#                                               #
#                SPECIES VALIDATION             #
#                                               #
#                                               #
# - # - # - # - # - # - # - # - # - # - # - # - #

species_validation_summary <- data.frame()
  
#--------------------------------------------
#----------- Load species details -----------
#--------------------------------------------

speciesName <- sub("^(\\w+)\\s+(\\w+).*", "\\1_\\2", species)  # Extract first two words of species name


#==============================================
#=                                            =
#=           PART 1: Climate model            =
#=                                            =
#==============================================

message("\n--- Part 1: Global climate model ---")

#--------------------------------------------
#---------- Define qs file paths ------------
#--------------------------------------------

climate_qs_file <- file.path(base_dir, "Climate",
                             paste0("Climate_model_", speciesName, "_", taxonkey, ".qs"))

habitat_qs_file <- file.path(base_dir, "Habitat",
                             paste0("Habitat_model_", speciesName, "_", taxonkey, ".qs"))


#--------------------------------------------------------
#-------- Only do validation if climate model exists ----
#--------------------------------------------------------

if (!file.exists(climate_qs_file)) {
  warning("No climate model was found for ",species,
          "\nRun 03_fit_climate_model.R first.\n Skipping species.")
  next
}

climatemodel <- qs2::qs_read(climate_qs_file)
top5_methods  <- climatemodel$top5_models
global_presabs <- climatemodel$global_presabs
climate_predictors <- climatemodel$selected_predictors
suppressWarnings(rm(climatemodel, pres, abs))



#-------------------------------------------------------
#------------------ Define validation types ------------
#-------------------------------------------------------

# get EU occurrences
eu_occ <- global_presabs %>%
  dplyr::filter(species==1) %>%
  sf::st_filter(sf::st_as_sfc(sf::st_bbox(euboundary))) # using bounding box to avoid loosing e.g., Malta

# Only validate climate model in Europe if 40 or more occs 
eu_climate_validation <- nrow(eu_occ) >= 40

# Only validate ensemble model if habitat model could be fitted
ensemble_validation <- file.exists(habitat_qs_file)


#---------------------------------------------------------------------
#---------- Select climate rasters used in 03_fit_climate_model.R ----
#---------------------------------------------------------------------

climate_selection <- terra::subset(climate_stack,
                                   climate_predictors[climate_predictors %in%
                                                        names(climate_stack)])


#---------------------------------------------------------------------------
#------ Obtain global climate subsample values for selected predictors -----
#---------------------------------------------------------------------------

if(pseudo_absences_background == "continents"){
  
  # this is how I upload the continents
  files <- list.files(paste0(database_path, "/natural_earth/continents/processed/"))
  continents <- c()
  
  for(c in files){
    
    x <- terra::vect(paste0(database_path, "/natural_earth/continents/processed/", c))
    
    # aggregate countries so the whole continent is together
    x <- terra::aggregate(x)
    
    # reproject
    x <- terra::project(x, project_projection)
    x <- terra::makeValid(x)
    continents <- terra::vect(c(x, continents))
    
  }
  
  continents <- terra::project(continents, 
                               project_projection)
  
  # check CRS of continents
  crs_env <- crs(continents, proj = T)
  
  if(crs_env != project_projection){
    
    warning("Continent vectors are in a different CRS than the one you selected!")
    
  }
  
}else if(pseudo_absences_background == "wwf_ecoregions"){
  
  # this is how we uploaded the WWF ecoregions in the old version
  wwf_eco <- sf::st_read(paste0(database_path, ("/wwf_terrestrial_ecoregions/data/commondata/data0/wwf_terr_ecos.shp")))
  
  # Project the data to the same CRS as the predictors raster stack
  wwf_eco <- sf::st_transform(wwf_eco, st_crs(project_projection)) %>%
    sf::st_make_valid()
  
  # check CRS of WWF ecoregions
  crs_env <- crs(wwf_eco, proj = T)
  
  if(crs_env != project_projection){
    
    warning("WWF ecoregions vectors are in a different CRS than the one you selected!")
    
  }
  
}else if(pseudo_absences_background == "biomes"){
  
  # load biomes
  wwf_eco <- sf::st_read(file.path(database_path, "wwf_terrestrial_ecoregions", "newRealms_cea.gpkg"))
  
  # check CRS of WWF ecoregions
  crs_env <- crs(wwf_eco, proj = T)
  
  if(crs_env != project_projection){
    
    warning("WWF ecoregions vectors are in a different CRS than the one you selected!")
    
  }
}

# Keep only biome polygons that intersect at least one occurrence point
global_presences<-dplyr::filter(global_presabs, species==1)

# Disable S2 geometry engine to avoid topological issues
sf::sf_use_s2(FALSE)

# Keep only polygons that intersect at least one occurrence point
if(pseudo_absences_background == "continents"){
  
  occ_ecoIntersect <- sf::st_intersects(sf::st_as_sf(continents), global_presences) 
  wwf_ecoSub1 <- continents[lengths(occ_ecoIntersect) > 0,1]
  
}else if(pseudo_absences_background %in% c("wwf_ecoregions", "biomes")){
  
  has_occurrence <- lengths(sf::st_intersects(sf::st_as_sf(wwf_eco), global_presences)) > 0
  wwf_ecoSub1 <- wwf_eco[has_occurrence, ]
  
}

sf::sf_use_s2(TRUE)

# Mask Chelsa layer with biomes with occurrences
wwf_ecoSub1_ext <- terra::ext(wwf_ecoSub1) 
climate_sub <- terra::crop(climate_selection[[1]], wwf_ecoSub1_ext) 
climate_sub <- terra::mask(climate_sub, wwf_ecoSub1)

# Extract a subsample of global pixels for Boyce calculation
set.seed(728)
global_subsample <- terra::spatSample(
  climate_sub,
  size = boyce_background_size, 
  method = "random", 
  na.rm = TRUE, #Ignore NA pixels
  as.points = TRUE) 

# Extract climate data at global subsample points
global_points <- terra::extract(climate_selection, global_subsample, ID = FALSE, xy = FALSE) %>%
  dplyr::mutate(ID = dplyr::row_number())

# Clean up
suppressWarnings(rm(wwf_ecoSub1, wwf_ecoSub1_ext, wwf_ecoSub1_vector,climate_sub, global_subsample))


#-----------------------------------------------------------
#------ Obtain European climate subsample values for selected predictors
#-----------------------------------------------------------

if(eu_climate_validation || ensemble_validation){
  eu_points <- eu_climate_sub %>%
    dplyr::select(any_of(climate_predictors))%>%
    dplyr::mutate(ID = dplyr::row_number())
}


#-----------------------------------------------------------------
#----------- Define if cross validation can be done and for how many folds -
#-----------------------------------------------------------------

# Default is 0 folds and no CV
cv_folds <- 0L
use_cv <- FALSE
use_cv_climate_only <- FALSE

# If ensemble validation is done, let EU data drive number of folds
if(ensemble_validation){
  
  # Load presabs data of habitat model
  habitatmodel <- qs2::qs_read(habitat_qs_file)
  eu_presabs <- habitatmodel$eu_presabs
  n_pres_ensemble <- sum(eu_presabs$species == 1)
  rm(habitatmodel)
  
  if (n_pres_ensemble>= 40L) {
    use_cv <- TRUE
    cv_folds <- min(5L, floor(n_pres_ensemble / 20L))
  }else if (nrow(global_presences) >= 40L) {
    use_cv_climate_only <- TRUE
    cv_folds <- min(5L, floor(nrow(global_presences) / 20L))
    eu_presabs <- eu_presabs%>%
      dplyr::mutate(ID = dplyr::row_number())
    
  }else{
    eu_presabs <- eu_presabs%>%
      dplyr::mutate(ID = dplyr::row_number())
  }
  
}else{
  
  if (nrow(global_presences) >= 40L) {
    use_cv <- TRUE
    cv_folds <- min(5L, floor(nrow(global_presences) / 20L))
  }else{
    global_presabs <- global_presabs %>%
      dplyr::mutate(ID = dplyr::row_number())
  }
}


#-----------------------------------------------------------------
#            OPTION 1: SPATIAL CROSS VALIDATION
#-----------------------------------------------------------------

tictoc::tic("Spatial cross-validation for climate model")

if (use_cv || use_cv_climate_only) {
  
  #---------------------------------
  #----- Create spatial folds-------
  #---------------------------------
  
  if(ensemble_validation && !use_cv_climate_only){
    
    #-----------------------------------------------------------------
    #--- Prepare combined dataset of global_presabs and eu_presabs ---
    #-----------------------------------------------------------------
    
    # Combine data
    all_presabs <- eu_presabs %>%
      sf::st_transform(crs=sf::st_crs(global_presabs))%>%
      dplyr::mutate(decimalLatitude = sf::st_coordinates(.)[, "Y"],
                    decimalLongitude = sf::st_coordinates(.)[, "X"])%>%
      dplyr::bind_rows(global_presabs)
    
    # Remove duplicates
    all_presabs$cell <- terra::cellFromXY( climate_stack[[1]], 
                                           all_presabs%>%
                                             st_coordinates%>%
                                             as.data.frame()) 
    
    all_presabs <- all_presabs %>%
      dplyr::filter(!is.na(cell))%>%
      group_by(cell) %>%
      dplyr::distinct(cell, .keep_all=TRUE)%>%
      dplyr::ungroup()
    
    # some endemic species may have empty folds because they are too clustered
    # eg Rana pyrenaica
    # so I have to twist the code to first perform with the original number of folds
    # and then if that doesn't work because a fold is empty, it reduces the folds at every run 
    
    valid_folds <- FALSE
    
    while (!valid_folds && cv_folds >= 2L) {
      
      message("Trying spatial CV fold with ", cv_folds, " folds")
      
      #--------------------------------
      #--- Generate spatial folds -----
      #--------------------------------
      
      sf::sf_use_s2(FALSE)
      set.seed(123)
      sb <- blockCV::cv_spatial(
        x         = vect(all_presabs),
        column    = "species",
        k         = cv_folds,
        hexagon   = TRUE, # Creates hexagonal (default) spatial blocks.
        selection = "random",
        iteration = 200,
        size      = 100000) # 100 km. numeric value of the specified range by which blocks are created and training/testing data are separated. This distance should be in metres
      
      sf::sf_use_s2(TRUE)
      fold_structure <- sb$blocks["folds"]
      
      
      #-------------------------------------------------
      #----- Assign occs of ensemble model to folds ----
      #-------------------------------------------------
      
      eu_presabs_perfold <- sf::st_join(sf::st_transform(eu_presabs, crs=sf::st_crs(global_presabs)),
                                        fold_structure,  
                                        join = sf::st_within,        
                                        left = TRUE)%>%
        dplyr::filter(!is.na(folds))%>%
        dplyr::mutate(ID = dplyr::row_number())
      
      # check presences per fold
      fold_summary <- eu_presabs_perfold %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(folds) %>%
        dplyr::summarise(
          n_pres = sum(species == 1),
          n_abs  = sum(species == 0)
        )
      
      message("Spatial CV fold summary with ", cv_folds, " folds:")
      print(fold_summary)
      
      valid_folds <- all(fold_summary$n_pres > 0)
      
      if (!valid_folds) {
        
        if (cv_folds == 2L) {
          stop("Unable to create valid folds with at least one presence. Can't go below 2 folds.")
        }
        
        message("Some folds contain no presences. Reducing folds.")
        
        cv_folds <- cv_folds - 1L
        
      }
      }

    if(nrow(eu_presabs_perfold)!=nrow(eu_presabs)){
      warning(nrow(eu_presabs)- nrow(eu_presabs_perfold)," Ensemble model point(s) not assigned to a fold and removed from dataset.")
    }
    
  }else{
    
    sf::sf_use_s2(FALSE)
    
    # Hex, class-balanced spatial folds
    set.seed(123)
    sb <- blockCV::cv_spatial(
      x         = vect(global_presabs),
      column    = "species",
      k         = cv_folds,
      hexagon   = TRUE,
      selection = "random",
      iteration = 200,
      size      = 100000) #100 km
    
    sf::sf_use_s2(TRUE)
    fold_structure<-sb$blocks["folds"]
  }
  
  
  #--------------------------------------------------
  #------- Assign occs of climate model to folds ----
  #--------------------------------------------------
  
  global_presabs_perfold <- sf::st_join(global_presabs,
                                        fold_structure,  
                                        join = sf::st_within,        
                                        left = TRUE)%>%
    dplyr::filter(!is.na(folds))%>%
    dplyr::mutate(ID = dplyr::row_number())
  
  if(nrow(global_presabs_perfold)!=nrow(global_presabs)){
    warning(nrow(global_presabs)- nrow(global_presabs_perfold)," global point(s) not assigned to a fold and removed from dataset.")
  }
  
  
  #-------------------------------------------------
  #-------- Create lists for storing results -------
  #-------------------------------------------------
  global_validation_climate <- list()
  if(eu_climate_validation) eu_validation_climate <- list()
  median_favourability_climate_perfold <- vector("list", cv_folds)
  
  
  #------------------------------------------------------------------
  #-------- Fit models on each training set and predict test set ----
  #------------------------------------------------------------------
  
  # Start loop per fold
  for (fold in seq_len(cv_folds)) {
    
    message(sprintf("Creating climate validation metrics for fold %d/%d: use folds %s for training", 
                    fold, cv_folds, paste(seq_len(cv_folds)[-fold], collapse = ", ")))
    
    
    #--------------------------------------
    #-          Define train data         -
    #--------------------------------------
    
    # Create training dataset
    train_data  <- global_presabs_perfold%>%
      dplyr::filter(folds!=fold)
    
    # Prevalence ratio from training data
    pres_train <- sum(train_data$species == 1)
    abs_train  <- sum(train_data$species == 0)
    prev_ratio <- pres_train/abs_train
    
    
    #--------------------------------------
    #-      Fit models on train data      -
    #--------------------------------------
    
    # Prepare model framework
    sdm_data <- sdm::sdmData(
      species ~ .,
      train      = vect(train_data),
      predictors = climate_selection
    )
    
    # Fit models
    model <- sdm::sdm(species ~ ., data = sdm_data, methods = top5_methods)
    
    # after fitting, some models may fail, so I can't use them
    # so i need to identify only the successful algorithms
    successful_methods <- as.vector(getModelInfo(model) %>% 
                                      filter(success == TRUE) %>% 
                                      filter(training == TRUE) %>% 
                                      pull(method))
    
    if(length(successful_methods) != length(top5_methods)){
      message("Not all the top 5 algorithms were successful in fold number ", fold, "! Reducing them to ", length(successful_methods))
      top5_methods <- successful_methods
      }

    #-----------------------------------------------------------
    #------ Prepare datasets with climate data for predictions --
    #-----------------------------------------------------------
    
    # Extract data for global validation
    test_data  <- global_presabs_perfold %>%
      dplyr::filter(folds == fold)
    
    global_env <- extract_env(test_data, climate_selection)
    
    datasets <- list(global_points = global_points,
                     occ_env       = global_env$presences,
                     abs_env       = global_env$absences)
    
    # Extract data for validation in Europe
    if(eu_climate_validation){
      
      # eu_test_data  <- test_data %>%
      #   sf::st_filter(euboundary_wgs84)

      eu_test_data <- test_data %>%
        sf::st_filter(sf::st_as_sfc(sf::st_bbox(euboundary))) # using bounding box to avoid loosing e.g., Malta
      
      eu_env <- extract_env(eu_test_data, climate_selection)
      datasets$eu_occ_env <- eu_env$presences
      datasets$eu_abs_env <- eu_env$absences
    }
    
    # Extract data for validation of ensemble model
    if(ensemble_validation & !use_cv_climate_only){
      ensemble_test_data <- eu_presabs_perfold %>%
        dplyr::filter(folds == fold)
      
      ensemble_env <- extract_env(ensemble_test_data, climate_selection)
      datasets$ens_occ_env <- ensemble_env$presences
      datasets$ens_abs_env <- ensemble_env$absences
    }
    
    # Add EU background data for validation of ensemble and Europe
    if (eu_climate_validation || ensemble_validation) {datasets$eu_points  <- eu_points}
    
    
    #-----------------------------------------------------------------------------
    #---- Make predictions per model algorithm and dataset and get median --------
    #-----------------------------------------------------------------------------
    
    median_favourability_climate_perfold[[fold]] <- compute_median_favourability(model,
                                                                                datasets,
                                                                                top5_methods,
                                                                                prev_ratio)
    
    #-----------------------------------------
    #------- Compute Boyce, AUC, and TSS -----
    #-----------------------------------------
    
    climate_fav <- median_favourability_climate_perfold[[fold]]
    
    # Global
    global_validation_climate[[fold]] <- compute_validation_metrics(
      species= speciesName,
      type = "Climate",
      region = "Global",
      fold = fold,
      all_suit_vals = climate_fav$global_points$median_favourability,
      occ_suit_vals = climate_fav$occ_env$median_favourability,
      abs_suit_vals = climate_fav$abs_env$median_favourability)
    
    # EU
    if(eu_climate_validation){
      eu_validation_climate[[fold]] <- compute_validation_metrics(
        species= speciesName,
        type = "Climate",
        region = "Europe",
        fold = fold,
        all_suit_vals = climate_fav$eu_points$median_favourability,
        occ_suit_vals = climate_fav$eu_occ_env$median_favourability,
        abs_suit_vals = climate_fav$eu_abs_env$median_favourability)
    }
    
    # Clean
    terra::tmpFiles(remove = TRUE)
    rm(successful_methods)
  }
  
  
  #-----------------------------------------
  #---- Store validation metrics in dfs ----
  #-----------------------------------------
  
  # Set AUC and tss to NA in global validation as these are calculated for different regions per species and, hence, are not comparable
  global_validation_climate <- dplyr::bind_rows(global_validation_climate)
  if(eu_climate_validation){
    eu_validation_climate <- dplyr::bind_rows(eu_validation_climate)
  } 
  tictoc::toc()
} else if (!use_cv) {
  tictoc::tic("No cross-validation for climate model")
  #--------------------------------------------------
  #-          OPTION 2: NO CROSS VALIDATION
  #--------------------------------------------------
  
  #--------------------------------------
  #-      Fit models on full data      -
  #--------------------------------------
  
  # Prepare model framework
  sdm_data <- sdm::sdmData(
    species ~ .,
    train      = vect(global_presabs),
    predictors = climate_selection
  )
  
  # Fit models
  model <- sdm::sdm(species ~ ., data = sdm_data, methods = top5_methods)
  
  
  #--------------------------------------
  #------- Define prevalence ratio ------
  #--------------------------------------
  
  pres_total <- sum(global_presabs$species == 1)
  abs_total  <- sum(global_presabs$species == 0)
  prev_ratio <- pres_total / abs_total
  
  
  #-----------------------------------------------------------
  #---- Prepare datasets with climate data for predictions --
  #-----------------------------------------------------------
  
  datasets<-list()
  
  if(!use_cv_climate_only){
    
    # Extract data for global validation
    global_env <- extract_env(global_presabs, climate_selection)
    datasets <- list(global_points = global_points,
                     occ_env       = global_env$presences,
                     abs_env       = global_env$absences)
    
    # Extract data for validation in Europe
    if(eu_climate_validation){
      
      # euboundary_presabs  <- global_presabs%>%
      #   sf::st_filter(euboundary_wgs84)
      
      euboundary_presabs <- global_presabs %>%
        sf::st_filter(sf::st_as_sfc(sf::st_bbox(euboundary))) # using bounding box to avoid loosing e.g., Malta
      
      eu_env<-extract_env(euboundary_presabs, climate_selection)
      datasets$eu_occ_env <- eu_env$presences
      datasets$eu_abs_env <- eu_env$absences
    }
  }
  
  # Extract data for validation of ensemble model
  if(ensemble_validation){
    
    ensemble_presabs <- eu_presabs%>%
      st_transform(crs=sf::st_crs(global_presabs))
    
    ensemble_env <- extract_env(ensemble_presabs, climate_selection)
    datasets$ens_occ_env <- ensemble_env$presences
    datasets$ens_abs_env <- ensemble_env$absences
  }
  
  # Add EU background data for validation of ensemble and Europe
  if (eu_climate_validation || ensemble_validation) {datasets$eu_points  <- eu_points}
  
  
  #-----------------------------------------------------------
  #---- Make predictions per model algorithm and dataset -----
  #-----------------------------------------------------------
  
  median_fav_climate <- compute_median_favourability(model,
                                                    datasets,
                                                    top5_methods,
                                                    prev_ratio)
  
  
  #-----------------------------------------
  #------- Compute Boyce, AUC, and TSS -----
  #-----------------------------------------
  
  if(!use_cv_climate_only){
    message("Calculating validation metrics (no cross-validation)")
    
    # Global
    global_validation_climate <- compute_validation_metrics(
      species = speciesName,
      type =  "Climate",
      region = "Global",
      fold = "No cross-validation",
      all_suit_vals = median_fav_climate$global_points$median_favourability,
      occ_suit_vals = median_fav_climate$occ_env$median_favourability,
      abs_suit_vals = median_fav_climate$abs_env$median_favourability)
    
    # EU
    if(eu_climate_validation){
      eu_validation_climate <- compute_validation_metrics(
        species = speciesName,
        type =  "Climate",
        region = "Europe",
        fold = "No cross-validation",
        all_suit_vals = median_fav_climate$eu_points$median_favourability,
        occ_suit_vals = median_fav_climate$eu_occ_env$median_favourability,
        abs_suit_vals = median_fav_climate$eu_abs_env$median_favourability)
      
    }
  }
  
}

#--------------------------------------------
#-------------- Export results --------------
#--------------------------------------------

# Define directories
climate_validation_dir <- file.path(base_dir, "Climate", "Current", "Diagnostics", "Model_validation")
if(!dir.exists(climate_validation_dir)) dir.create(climate_validation_dir, recursive = TRUE, showWarnings = FALSE)

# Export validation summary (mean across folds) when relevant
if(use_cv || use_cv_climate_only){
  
  # Export per fold validation
  readr::write_csv(global_validation_climate,
                   file.path(climate_validation_dir, paste0(speciesName, "_global_climate_validation_per_fold.csv"))) 
  
  # Export summary
  global_validation_clim_mean <- summarise_validation(df = global_validation_climate, 
                                                      validation = "Cross-validation")
  readr::write_csv(global_validation_clim_mean,
                   file.path(climate_validation_dir, paste0(speciesName, "_global_climate_validation_summary.csv"))) 
  
  # Bind results to validation summary
  species_validation_summary <- species_validation_summary %>%
    dplyr::bind_rows(global_validation_clim_mean)
  
  if (eu_climate_validation) {
    
    # Export per fold validation
    readr::write_csv(eu_validation_climate,
                     file.path(climate_validation_dir, paste0(speciesName, "_eu_climate_validation_per_fold.csv")))
    
    # Export summary
    eu_validation_clim_mean <- summarise_validation(eu_validation_climate, 
                                                    validation ="Cross-validation")
    readr::write_csv(eu_validation_clim_mean,
                     file.path(climate_validation_dir, paste0(speciesName, "_eu_climate_validation_summary.csv")))
    
    # Add summary to species validation df
    species_validation_summary<-species_validation_summary%>%
      dplyr::bind_rows(eu_validation_clim_mean)
    
  }
  
}else{
  
  # Export non-cross-validated results
  readr::write_csv(global_validation_climate,
                   file.path(climate_validation_dir, paste0(speciesName, "_global_climate_validation_summary.csv")))
  
  # Add summary to species validation df
  global_validation_clim_mean<-summarise_validation(df = global_validation_climate,
                                                    validation = "No cross-validation")
  
  species_validation_summary<-species_validation_summary%>%
    dplyr::bind_rows(global_validation_clim_mean)
  
  if (eu_climate_validation) {
    # Export non crossvalidated results
    readr::write_csv(eu_validation_climate,
                     file.path(climate_validation_dir, paste0(speciesName, "_eu_climate_validation_summary.csv")))
    
    # Store summary in species validation df
    eu_validation_clim_mean <- summarise_validation(eu_validation_climate,
                                                  validation = "No cross-validation")
    
    species_validation_summary <- species_validation_summary%>%
      dplyr::bind_rows(eu_validation_clim_mean)
    
  }
  tictoc::toc()
}
invisible(gc())



#==============================================
#=                                            =
#=     PART 2: European landcover model       =
#=                                            =
#==============================================

message("\n--- Part 2: European land-cover model ---")

#--------------------------------------------
#---- Should habitat validation be done? ----
#--------------------------------------------

if (!ensemble_validation) {
  warning("No habitat model was fitted for species ", species,
          "\n Skipping habitat model validation.")
  validation_summary[[speciesName]]<-  species_validation_summary
  next}


#-----------------------------------------------------
#--------- Load  data stored in climate model qs -----
#-----------------------------------------------------

habitatmodel <- qs2::qs_read(habitat_qs_file)
eu_presabs <- habitatmodel$eu_presabs
top5_habitat_methods  <- habitatmodel$top5_models
habitat_predictors <- habitatmodel$selected_predictors
rm(habitatmodel)



#-------------------------------------------------------------------------
#-------- Select landcover rasters used in 04_fit_climate_model.R --------
#-------------------------------------------------------------------------

# Load again just in case
habitat_stack <- terra::rast(habitat_path)
# get %
habitat_stack <- habitat_stack/10000

habitat_selection <- terra::subset(habitat_stack,
                                   habitat_predictors[habitat_predictors %in%
                                                        names(habitat_stack)])


#--------------------------------------------------------------------------
#----------- Obtain habitat subsample values for selected predictors ------
#--------------------------------------------------------------------------

eu_habitat_points <- eu_habitat_sub %>%
  dplyr::select(any_of(habitat_predictors)) %>%
  dplyr::mutate(ID = dplyr::row_number())


#-----------------------------------------------------------------
#            OPTION 1: SPATIAL CROSS VALIDATION
#-----------------------------------------------------------------

tictoc::tic("Spatial cross-validation for habitat model")

if (use_cv) {

  #---------------------------------------------------
  #-------- Put fold assignment data in right CRS ----
  #---------------------------------------------------
  
  if(crs(eu_presabs_perfold, proj=T) != crs(eu_presabs, proj=T)){
    eu_presabs_perfold <- eu_presabs_perfold %>%
      sf::st_transform(crs=sf::st_crs(eu_presabs))
  }
  
  #--------------------------------------------------------------------
  #--------- Fit models on each training set and predict test set -----
  #--------------------------------------------------------------------
  
  # Define lists to store validation metrics
  validation_habitat <- list()
  median_favourability_habitat_perfold <- vector("list", cv_folds)
  
  # Start loop per fold
  for (fold in seq_len(cv_folds)) {
    
    message(sprintf("Creating habitat validation metrics for fold %d/%d: use folds %s for training", 
                    fold, cv_folds, paste(seq_len(cv_folds)[-fold], collapse = ", ")))
    
    
    #-----------------------------------
    #---------- Define train data ------
    #-----------------------------------
    
    # Create training dataset
    train_data  <- eu_presabs_perfold%>%
      dplyr::filter(folds!=fold)
    
    # Prevalence ratio from training data
    pres_train <- sum(train_data$species == 1)
    abs_train  <- sum(train_data$species == 0)
    prev_ratio <- pres_train/abs_train
    
    
    #--------------------------------------
    #-------- Fit models on train data ----      
    #--------------------------------------
    
    # Load again just in case
    habitat_stack <- terra::rast(habitat_path)
    # get %
    habitat_stack <- habitat_stack/10000
    
    habitat_selection <- terra::subset(habitat_stack,
                                       habitat_predictors[habitat_predictors %in%
                                                            names(habitat_stack)])
    # Prepare model framework
    sdm_data <- sdm::sdmData(
      species ~ .,
      train      = vect(train_data),
      predictors = habitat_selection)
    
    # Fit models
    habitat_model <- sdm::sdm(species ~ ., data = sdm_data, methods = top5_habitat_methods)
    
    # after fitting, some models may fail, so I can't use them
    # so i need to identify only the successful algorithms
    successful_methods_habitat <- as.vector(getModelInfo(habitat_model) %>% 
                                      filter(success == TRUE) %>% 
                                      filter(training == TRUE) %>% 
                                      pull(method))
    
    if(length(successful_methods_habitat) != length(top5_habitat_methods)){
      message("Not all the top 5 algorithms were successful in fold number ", fold, "! Reducing them to ", length(successful_methods_habitat))
      top5_habitat_methods <- successful_methods_habitat
    }
    
    
    #-----------------------------------------------------------
    #---- Prepare datasets with habitat data for predictions --
    #-----------------------------------------------------------
    
    # Extract data for validation in Europe
    test_data  <- eu_presabs_perfold %>%
      dplyr::filter(folds == fold)
    
    europe_hab <- extract_env(test_data, habitat_selection)
    
    habitat_datasets <- list(eu_habitat_points = eu_habitat_points,
                             occ_hab       = europe_hab$presences,
                             abs_hab       = europe_hab$absences)
    
    
    #--------------------------------------------------------------------------
    #---- Make predictions per model algorithm and dataset and get median -----
    #--------------------------------------------------------------------------
    
    median_favourability_habitat_perfold[[fold]]<- compute_median_favourability(habitat_model,
                                                                                habitat_datasets,
                                                                                top5_habitat_methods,
                                                                                prev_ratio)
    #-----------------------------------------
    #------- Compute Boyce, AUC, and TSS -----
    #-----------------------------------------
    
    habitat_fav <- median_favourability_habitat_perfold[[fold]]
    
    # EU
    validation_habitat[[fold]] <- compute_validation_metrics(
      species= speciesName,
      type = "Habitat",
      region = "Europe",
      fold = fold,
      all_suit_vals = habitat_fav$eu_habitat_points$median_favourability,
      occ_suit_vals = habitat_fav$occ_hab$median_favourability,
      abs_suit_vals = habitat_fav$abs_hab$median_favourability)
    
    # Clean
    terra::tmpFiles(remove = TRUE)
  }
  
  
  #-----------------------------------------
  #---- Store validation metrics in dfs ----
  #-----------------------------------------
  
  eu_validation_habitat <- dplyr::bind_rows(validation_habitat)
  
  tictoc::toc()
} else {
  tictoc::tic("No cross-validation for habitat model")
  #--------------------------------------------------
  #-          OPTION 2: NO CROSS VALIDATION
  #--------------------------------------------------
  
  #--------------------------------------
  #------------ Fit models on full data -----------
  #--------------------------------------
  
  # Load again just in case
  habitat_stack <- terra::rast(habitat_path)
  # get %
  habitat_stack <- habitat_stack/10000
  
  habitat_selection <- terra::subset(habitat_stack,
                                     habitat_predictors[habitat_predictors %in%
                                                          names(habitat_stack)])
  
  # Prepare habitat_model framework
  sdm_data <- sdm::sdmData(
    species ~ .,
    train      = vect(eu_presabs),
    predictors = habitat_selection)
  
  # Fit models
  habitat_model <- sdm::sdm(species ~ ., data = sdm_data, methods = top5_habitat_methods)
  
  
  #--------------------------------------
  #------- Define prevalence ratio ------
  #--------------------------------------
  
  pres_total <- sum(eu_presabs$species == 1)
  abs_total  <- sum(eu_presabs$species == 0)
  prev_ratio <- pres_total / abs_total
  
  
  #-----------------------------------------------------------
  #---- Prepare datasets with habitat data for predictions --
  #-----------------------------------------------------------
  
  # Extract data for validation in Europe
  europe_hab <- extract_env(eu_presabs, habitat_selection)
  
  habitat_datasets <- list(eu_habitat_points = eu_habitat_points,
                           occ_hab       = europe_hab$presences,
                           abs_hab       = europe_hab$absences)
  
  
  #---------------------------------------------------------------------
  #---- Make predictions per model algorithm and dataset and get median 
  #----------------------------------------------------------------------
  
  median_fav_habitat <- compute_median_favourability(habitat_model,
                                                    habitat_datasets,
                                                    top5_habitat_methods,
                                                    prev_ratio)
  
  
  #-----------------------------------------
  #------- Compute Boyce, AUC, and TSS -----
  #-----------------------------------------
  
  eu_validation_habitat <- compute_validation_metrics(
    species= speciesName,
    type = "Habitat",
    region = "Europe",
    fold = "No cross-validation",
    all_suit_vals = median_fav_habitat$eu_habitat_points$median_favourability,
    occ_suit_vals = median_fav_habitat$occ_hab$median_favourability,
    abs_suit_vals = median_fav_habitat$abs_hab$median_favourability)
  
  tictoc::toc()
}

invisible(gc())



#--------------------------------------------
#-------------- Export results --------------
#--------------------------------------------

# Export validation overview
habitat_validation_dir <- file.path(base_dir, "Habitat", "Current", "Diagnostics", "Model_validation")
if(!dir.exists(habitat_validation_dir)) dir.create(habitat_validation_dir, recursive = TRUE, showWarnings = FALSE)

# Export validation summary (mean across folds) when relevant
if(use_cv){
  
  # Export per fold validation metrics
  readr::write_csv(eu_validation_habitat,
                   file.path(habitat_validation_dir, paste0(speciesName, "_habitat_validation_per_fold.csv")))
  
  # Export summary validation metrics
  validation_hab_mean<-summarise_validation(eu_validation_habitat,
                                            validation = "Cross-validation")
  readr::write_csv(validation_hab_mean,
                   file.path(habitat_validation_dir, paste0(speciesName, "_habitat_validation_summary.csv")))
  
}else{
  validation_hab_mean<-summarise_validation(eu_validation_habitat,
                                            validation = "No cross-validation")
  
  readr::write_csv(eu_validation_habitat,
                   file.path(habitat_validation_dir, paste0(speciesName, "_habitat_validation_summary.csv")))
}

species_validation_summary<-species_validation_summary%>%
  dplyr::bind_rows(validation_hab_mean)



#==============================================
#=                                            =
#=         PART 3: Ensemble validation        =
#=                                            =
#==============================================

message("\n--- Part 3: Combined model ---")
tictoc::tic("Spatial cross-validation for combined model")

#-----------------------------------------------------------------
#            OPTION 1: SPATIAL CROSS VALIDATION
#----------------------------------------------------------------

if (use_cv) {
  
  #----------------------------------------------------------
  #------- Combine predictions of habitat and climate model --
  #----------------------------------------------------------
  
  # Define lists to store validation metrics
  validation_ensemble<- list()
  
  # Start loop per fold
  for (fold in seq_len(cv_folds)) {
    
    message(sprintf("Calculating ensemble validation metrics for test fold %d/%d", fold,cv_folds))
    
    # Extract median favourability for the current fold
    hab_fav <- median_favourability_habitat_perfold[[fold]]
    clim_fav <- median_favourability_climate_perfold[[fold]]
    
    # Generate ensemble favourability for background points, occs, and abs.
    ensemble_background_fav <- ensemble_geom_mean(hab_fav$eu_habitat_points, clim_fav$eu_points, type="background")
    ensemble_occ_fav <- ensemble_geom_mean(hab_fav$occ_hab,clim_fav$ens_occ_env, type="occurrence")
    ensemble_abs_fav <- ensemble_geom_mean(hab_fav$abs_hab,clim_fav$ens_abs_env, type="absence")
    
    
    #-----------------------------------------
    #------- Compute Boyce, AUC, and TSS -----
    #-----------------------------------------
    
    validation_ensemble[[fold]] <- compute_validation_metrics(
      species= speciesName,
      type = "Ensemble",
      region = "Europe",
      fold = fold,
      all_suit_vals = ensemble_background_fav,
      occ_suit_vals =  ensemble_occ_fav ,
      abs_suit_vals = ensemble_abs_fav)}
  
  #-----------------------------------------
  #---- Store validation metrics in df ----
  #-----------------------------------------
  
  eu_validation_ensemble <- dplyr::bind_rows(validation_ensemble)
  tictoc::toc()
  
} else{
  tictoc::tic("No cross-validation for combined model")
  #--------------------------------------------------
  #-          OPTION 2: NO CROSS VALIDATION
  #--------------------------------------------------
  
  # Extract median favourability for habitat and climate
  hab_fav <- median_fav_habitat
  clim_fav <- median_fav_climate
  
  
  # Generate ensemble favourability for background points, occs, and abs.
  ensemble_background_fav <- ensemble_geom_mean(hab_fav$eu_habitat_points, clim_fav$eu_points, type="background")
  ensemble_occ_fav <- ensemble_geom_mean(hab_fav$occ_hab,clim_fav$ens_occ_env, type="occurrence")
  ensemble_abs_fav <- ensemble_geom_mean(hab_fav$abs_hab,clim_fav$ens_abs_env, type="absence")
  
  
  #-----------------------------------------
  #------- Compute Boyce, AUC, and TSS -----
  #-----------------------------------------
  message("Calculating ensemble validation metrics (no cross-validation)")
  
  eu_validation_ensemble <- compute_validation_metrics(
    species= speciesName,
    type = "Ensemble",
    region= "Europe",
    fold = "No cross-validation",
    all_suit_vals = ensemble_background_fav,
    occ_suit_vals =  ensemble_occ_fav ,
    abs_suit_vals = ensemble_abs_fav)
  tictoc::toc()
}

invisible(gc())

#--------------------------------------------
#-------------- Export results --------------
#--------------------------------------------

# Define directory
ensemble_validation_dir<-file.path(base_dir, "Combined", "Current", "Diagnostics", "Model_validation")
if(!dir.exists(ensemble_validation_dir)) dir.create(ensemble_validation_dir, recursive = TRUE, showWarnings = FALSE)

# Export validation summary (mean across folds) when relevant
if(use_cv){
  
  #Export per fold validation metrics
  readr::write_csv(eu_validation_ensemble,
                   file.path(ensemble_validation_dir, paste0(speciesName, "_combined_validation_per_fold.csv")))
  
  #Export summary validation metrics
  validation_ens_mean<- summarise_validation(eu_validation_ensemble,
                                             validation = "Cross-validation")
  readr::write_csv(validation_ens_mean,
                   file.path(ensemble_validation_dir, paste0(speciesName, "_combined_validation_summary.csv")))
  
}else{
  
  #Export summary validation metrics
  validation_ens_mean<-summarise_validation(eu_validation_ensemble,
                                            validation="No cross-validation")
  readr::write_csv(eu_validation_ensemble,
                   file.path(ensemble_validation_dir, paste0(speciesName, "_combined_validation_summary.csv")))
  
}

species_validation_summary <- species_validation_summary%>%
  dplyr::bind_rows(validation_ens_mean)

#---------------------------------------------
#----- Store results in validation summary ---
#---------------------------------------------

validation_summary[[speciesName]] <- species_validation_summary

#-------------------------------------------------------
#----- Export combined validation results --------------
#-------------------------------------------------------

final_validation <- bind_rows(validation_summary)

readr::write_csv(final_validation,
                 file.path(validation_dir, "Validation_summary.csv"))

#--------------------------------------------
#-------- End of loop -----------------------
#--------------------------------------------

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "hours")
cat("Success! Spatial CV validation for model with pseudo-presences has been created for", sp, "in", round(elapsed, 2), "hours\n\n")

# Clean up 
rm(list = ls())
terra::tmpFiles(remove = TRUE)

