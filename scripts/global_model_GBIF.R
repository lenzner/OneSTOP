#############################################################
#################### GLOBAL CLIMATE MODEL ###################
#############################################################

# Started on 30.03.2026
# modified on 15.05.2026
# finalized on 28.07.2026
# by LT

# this is a script to fit a global climate model
# for IUCN RL species
# using the new wiSDM version
# using GBIF presences
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

# Define paths
source_path <- paste0(getwd(), "/scripts")
database_path <- "/lisc/data/work/botany/tedeschi/Work/databases"
output_path <- file.path(getwd(), "outputs", "runs", "run02")

# Load functions
source(file.path(source_path, "task5.1", "aux_funs.R"))
source(file.path(source_path, "wiSDM_v02", "helper_functions.R"))
source(file.path(getwd(), "slurm", "runs", "run02", "GBIF", "configurations_GBIF.R"))

# Assign species name
args = commandArgs(trailingOnly = TRUE)
print(args)
sp <- args[1]
sp <- sub("_", " ", sp)

message("========= Processing species ", sp, " for global climate model with GBIF presences")

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



#--------------------------------------------
#--- Load global occurrences and taxa info ---
#--------------------------------------------

# NB decide if you want to upload the WGS84 (lat/lon) or CEA WGS84 occurrences
# and which ones (> 1981 or not)
# load also pseudopresence dataset that has taxonkey info
if(project_projection == "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"){
  
  global_pseudopresences <- read.csv(paste0("./data/processed/range_sampling/global.occ.cea.wgs84.LL_", project_version, ".csv"))
  global_gbif <- qs2::qs_read(paste0("./data/processed/GBIF/IUCN_RL_sp/", project_version, "/IUCN_RL_sp_occurrences_cleaned.1981.cea.wgs84.qs"))
  
}else if(project_projection == "EPSG:4326"){
  
  global_pseudopresences <- read.csv(paste0("./data/processed/range_sampling/global.occ.wgs84.LL_", project_version, ".csv"))
  global_gbif <- qs2::qs_read(paste0("./data/processed/GBIF/IUCN_RL_sp/", project_version, "/IUCN_RL_sp_occurrences_cleaned.1981.qs"))
  
}



#--------------------------------------------
#------- Split dataframe by taxonkey --------
#--------------------------------------------

split_df_pseudopresences <- global_pseudopresences %>% filter(species == sp) 

if(!is.na(unique(split_df_pseudopresences$acceptedUsageKey))){
    
    message(sp, " is a synonym - using accepted taxon key...")
    taxonkey <- unique(split_df_pseudopresences$acceptedUsageKey)
    split_df <- global_gbif %>% filter(acceptedTaxonKey == taxonkey)
    message("Changing name of ", sp, " to ", unique(split_df$species))
    sp <- unique(split_df$species)
    
  }else{
    
    taxonkey <- unique(split_df_pseudopresences$speciesKey)
    split_df <- global_gbif %>% filter(acceptedTaxonKey == taxonkey)
  }

message(paste0("Starting number of GBIF presences for ", sp, ": ", nrow(split_df)))
species <- sp
speciesName <- sub("^(\\w+)\\s+(\\w+).*", "\\1_\\2", species)  # Extract first two words of species name
speciesgroup <- unique(split_df$Group)
rm(global_pseudopresences, global_gbif, split_df_pseudopresences)



#------------------------------------------------------------
#------ Define file paths of current environmental layers ---
#------------------------------------------------------------

processed_folder <- paste0(database_path, "/CHELSA_V2.1/chelsa_current")
globalclimpreds_file <- file.path(processed_folder, "/globalclimpreds.tif")
eu_climpreds_file <- paste0(processed_folder, "/euclimpreds.tif")

if(tolower(country_of_interest)!="europe"){
  country_climpreds_file <- file.path(processed_folder, "country_climpreds.tif")
}else{
  country_climpreds_file <- eu_climpreds_file
}



#----------------------------------------------------------------
#---- Define file paths of Europe future environmental layers ---
#----------------------------------------------------------------

future_paths <- list()

for (period in c("2041-2070","2071-2100")){
  for(scenario in c("ssp126", "ssp370", "ssp585")){
    
    # Define preprocessed dir
    scenario_folder <- ifelse(scenario == "ssp126", "1-2.6", 
                              ifelse(scenario == "ssp370", "3-7.0",
                                     ifelse(scenario == "ssp585", "5-8.5", NA)))
    
    preprocessed_dir <- file.path(database_path, "CHELSA_V2.1", "chelsa_future","europe", period,scenario)
  
    # Define output file
    out_file <- paste0(preprocessed_dir, "/", list.files(preprocessed_dir))
    
    # Store path for later use
    future_paths[[paste0(period, "_", scenario)]] <- out_file # these will need to be stacked
    
  }
}



#--------------------------------------------
#----------- Load boundary layers -----------
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
#--------- Load shape of the world ----------
#--------------------------------------------
world <- rnaturalearth::ne_countries(scale=50)

# reproject to the same CRS of the climate data
world <- terra::project(vect(world), 
                        project_projection)


#--------------------------------------------------------------
#--------------Load ecoregions/biomes/continents --------------
#--------------------------------------------------------------

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
  
  #Sys.setenv(OGR_ORGANIZE_POLYGONS = "SKIP")
  
  # load biomes
  wwf_eco <- sf::st_read(file.path(database_path, "wwf_terrestrial_ecoregions", "newRealms_cea.gpkg"))

  # check CRS of WWF ecoregions
  crs_env <- crs(wwf_eco, proj = T)
  
  if(crs_env != project_projection){
    
    warning("WWF ecoregions vectors are in a different CRS than the one you selected!")
    
  }
}



#--------------------------------------------
#-------Load file paths to bias grids -------
#--------------------------------------------

# for(speciesgroup in names(bias_grid_paths)) {
#   
#   biasgrid_group <- terra::rast(bias_grid_paths[[speciesgroup]])
#   biasgrid_group <- terra::project(biasgrid_group, globalclimpreds_terra[[1]], method = "bilinear")
#   
#   # Resample biasgrid to match the resolution of globalclimpreds_terra
#   biasgrid_group <- terra::resample(biasgrid_group, globalclimpreds_terra[[1]], method = "bilinear")
#   
#   writeRaster(biasgrid_group, 
#               paste0("../databases/wiSDM_biasgrids/processed/", speciesgroup, ".tif"),
#               overwrite = T)
# }

# load the biasgrids already projected and resampled

bias_grid_paths <- list(
  Molluscs = paste0(database_path, "/wiSDM_biasgrids/processed/Molluscs.tif"),
  Mammals = paste0(database_path, "/wiSDM_biasgrids/processed/Mammals.tif"),
  Insects = paste0(database_path, "/wiSDM_biasgrids/processed/Insects.tif"),
  Birds = paste0(database_path, "/wiSDM_biasgrids/processed/Birds.tif"),
  Amphibians = paste0(database_path, "/wiSDM_biasgrids/processed/Amphibians.tif"),
  Reptiles = paste0(database_path, "/wiSDM_biasgrids/processed/Reptiles.tif"),
  Plants = paste0(database_path, "/wiSDM_biasgrids/processed/Plants.tif"))

invisible(gc())



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#          SET UP MODELING          #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 



#--------------------------------------------
#----------- Load occurrence data -----------
#--------------------------------------------

global.occ.LL.cleaned <- split_df %>%
  dplyr::select(c(decimalLongitude,decimalLatitude))

# Generate file for informing PA selection containing all occurrences (no thinning, in case we thinned split_df)
for_PA_selection <- split_df %>%
  dplyr::select(c(decimalLongitude, decimalLatitude)) %>%
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = project_projection)


#---------------------------------------------------
#------ Prepare filenames and titles for export ----
#---------------------------------------------------

# Prepare PDF title 
nameExtension <- if (grepl("^\\S+\\s+\\S+$", species)) "" else sub("^\\S+\\s+\\S+\\s+", "", species)
PDF_title <- bquote(italic(.(gsub("_", " ", speciesName))) ~ .(nameExtension) ~ "(" * .(taxonkey) * ")")

# Prepare current and future basefile
basefile <-  paste0(speciesName,"_Climate_")


#--------------------------------------------
#------------- Create folders ---------------
#--------------------------------------------

# Define base project folder and create if necessary
base_dir <- file.path(output_path, projectname, paste0(speciesName, "_", taxonkey))

if(!dir.exists(base_dir)){
  message("Creating base directory for ", speciesName, ", project version: ", projectname)
  dir.create(base_dir)}

# Define outputs, periods, and scenarios
periods   <- c("Current","2041-2070", "2071-2100")
scenarios <- c("ssp126", "ssp370", "ssp585")
outputs   <- c("Rasters", "PDFs", "PNGs")

# Create folders for each combination
scenario_folders <- list()

for(period in periods){
  for(output in outputs){
    if(period == "Current"){
      loop_list <- list(list(path = file.path(base_dir, "Climate", period,"Predictions",output),
                             name = paste("Climate", period, "Predictions",output,  sep = "/")),
                        list(path = file.path(base_dir, "Climate", period,"Diagnostics", "Variable_importance"),
                             name = paste("Climate", period, "Diagnostics", "Variable_importance",  sep = "/")),
                        list(path = file.path(base_dir, "Climate", period,"Diagnostics", "Response_curves"),
                             name = paste("Climate", period,"Diagnostics", "Response_curves", sep = "/")),
                        list(path = file.path(base_dir, "Climate", period,"Diagnostics", "Confidence_maps",output),
                             name = paste("Climate", period, "Diagnostics", "Confidence_maps", output,  sep = "/")))
      scenario_folders <- c(scenario_folders, loop_list)  
      
    }else{
      for(scenario in scenarios){
        loop_list <- list(list(path = file.path(base_dir, "Climate", period, scenario, "Predictions", output),
                               name = paste("Climate", period, scenario, "Predictions", output, sep = "/")),
                          list(path = file.path(base_dir, "Climate", period, scenario, "Diagnostics", "Confidence_maps",output),
                               name = paste("Climate", period, scenario, "Diagnostics", "Confidence_maps", output,  sep = "/")))
        scenario_folders <- c(scenario_folders, loop_list)
      }
    }
  }
}

# Add Rasters/Interim folder
fixed_folders <- list(
  list(path = file.path(base_dir, "Climate", "Current", "Interim"), 
       name = "Interim"))

# Combine 
folder_paths <- c(fixed_folders, scenario_folders)

# Check and create each folder if necessary
lapply(folder_paths, function(folder){
  create_folder(folder$path, folder$name)
})


#--------------------------------------------
#---------- Load environmental data ---------
#--------------------------------------------

# CHELSA global stack of bioclimatic variables with NAs removed
globalclimpreds_terra <- terra::rast(globalclimpreds_file)

# assign correct names
globalclimpreds_terra <- globalclimpreds_terra %>% 
  tidyterra::rename("CHELSA_meantemp_1" = "CHELSA_bio1_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_temp_seasonality_4" = "CHELSA_bio4_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_maxTmpWarmestMon_5" = "CHELSA_bio5_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_minTmpColdestMon_6" = "CHELSA_bio6_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_temp_annRange_7" = "CHELSA_bio7_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_annPrecip_12" = "CHELSA_bio12_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipWettestMon_13" = "CHELSA_bio13_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipDriestMon_14" = "CHELSA_bio14_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipSeasonality_15" = "CHELSA_bio15_1981-2010_V.2.1") 

# CHELSA European stack of bioclimatic variables 
eu_climpreds.10 <- terra::rast(country_climpreds_file)
#names(eu_climpreds.10) <- clim_vars$bio

# assign correct names
eu_climpreds.10 <- eu_climpreds.10 %>% 
  tidyterra::rename("CHELSA_meantemp_1" = "CHELSA_bio1_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_temp_seasonality_4" = "CHELSA_bio4_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_maxTmpWarmestMon_5" = "CHELSA_bio5_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_minTmpColdestMon_6" = "CHELSA_bio6_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_temp_annRange_7" = "CHELSA_bio7_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_annPrecip_12" = "CHELSA_bio12_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipWettestMon_13" = "CHELSA_bio13_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipDriestMon_14" = "CHELSA_bio14_1981-2010_V.2.1")  %>%
  tidyterra::rename("CHELSA_precipSeasonality_15" = "CHELSA_bio15_1981-2010_V.2.1") 

if( sum(names(globalclimpreds_terra) != names(eu_climpreds.10)) > 0){
  warning("Global CHELSA layer names and European layer names do not match!")
}



#--------------------------------------------
#---------- Process occurrence data ---------
#--------------------------------------------

# Keep only one occurrence per grid cell
global.occ.LL.cleaned <- remove_duplicates(occurrences = global.occ.LL.cleaned, 
                                           rast_template = globalclimpreds_terra[[1]])

# Remove occurrences within grid cells with NA values
# this sometimes fail because terra is picky, so I may need to transform my occurrences to a vector

global.occ.sf <- tryCatch({
  
  # First attempt: Using the raw data frame
  remove_nodata_occurrences(occurrences = global.occ.LL.cleaned, 
                                           rast_template = globalclimpreds_terra[[1]], 
                                           crs = project_projection)
  }, error = function(e){
    
    message("Duplicated occurrence removal failed with error: ", e$message)
    message("Attempting recovery by converting to SpatVector...")
    
    # Convert to SpatVector (fallback)
    occ_vect <- terra::vect(global.occ.LL.cleaned, 
                            geom = c("decimalLongitude", "decimalLatitude"), 
                            crs = project_projection,
                            keepgeom = T)
    
    # Second attempt: Using the SpatVector
    return(remove_nodata_occurrences(occ_vect, globalclimpreds_terra[[1]], project_projection))
  })
                                             
# Add column indicating species presence (1) for modeling
global.occ.sf$species <- rep(1, nrow(global.occ.sf)) 


#-----------------------------------------------
#------ Limit to 10,000 occupied grid cells ----
#-----------------------------------------------

if(nrow(global.occ.sf) > 10000){
  if(occurrence_thinning_method == "random"){
    print("Thinning occurrences randomly")
    set.seed(101) 
    global.occ.sf <- global.occ.sf[sample(nrow(global.occ.sf), 10000, replace=FALSE), ]
  }else if (occurrence_thinning_method == "kmeans_clustering"){
    
    print("Thinning occurrences based on k-means clustering")
    
    # Extract environmental data in each occurrence grid cell
    env_data <- terra::extract(globalclimpreds_terra, global.occ.sf, ID = FALSE)
    
    # Check how many unique rows there are and set centers to lowest of either 10000 or #unique rows
    unique_centers <- nrow(unique(env_data))
    center_number <- min(unique_centers, 10000)
    
    # K-means clustering
    set.seed(101)
    clust <- kmeans(env_data, centers = center_number, iter.max = 10, nstart = 1)$cluster
    occ_env <- cbind(global.occ.sf, env_data, clust)%>%
      dplyr::mutate(rID =row_number())
    
    # Keep 1 occurrence per cluster
    sampled <- occ_env %>%
      dplyr::group_by(clust) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::ungroup()
    
    # How many presences do we still need
    remaining <- 10000 - nrow(sampled)
    
    # sample extra occurrences if fewer than 10000
    if (remaining > 0) {
      
      # Randomly sample additional presences excluding already chosen ones
      extra_occ <- occ_env %>%
        dplyr::filter(!rID %in% sampled$rID)%>%
        dplyr::slice_sample(n = remaining) 
      
      global.occ.sf <- bind_rows(sampled, extra_occ)
      rm(extra_occ)
      
    } else {
      global.occ.sf <- sampled
    }
    
    # Keep only occurrence columns
    global.occ.sf <- global.occ.sf %>%
      dplyr::select(decimalLongitude, decimalLatitude, geometry, species)
    
    rm(env_data, occ_env, sampled, remaining, unique_centers, center_number, clust)
    
  }
}

message(paste0("Cleaned number of GBIF presences for ", sp, ": ", nrow(global.occ.sf)))



#-----------------------------------------------------------
#---- Don't fit model if less than 20 global presences -----
#-----------------------------------------------------------

if(nrow(global.occ.sf) < 20){
  warning(paste0("Skipping species ", species, " because the number of occurrences is less than 20 (n =",nrow(global.occ.sf),")"))
  next  # Skip the rest of the loop and move to the next iteration
}



#--------------------------------------------------------------------
#----- Select ecoregions/biomes/continents containing occurrences ---
#--------------------------------------------------------------------

# Disable S2 geometry engine to avoid topological issues
sf::sf_use_s2(FALSE)

# Keep only polygons that intersect at least one occurrence point
if(pseudo_absences_background == "continents"){
  
  occ_ecoIntersect <- sf::st_intersects(sf::st_as_sf(continents), global.occ.sf) 
  wwf_ecoSub1 <- continents[lengths(occ_ecoIntersect) > 0,1]
  
}else if(pseudo_absences_background %in% c("wwf_ecoregions", "biomes")){

  has_occurrence <- lengths(sf::st_intersects(sf::st_as_sf(wwf_eco), global.occ.sf)) > 0
  wwf_ecoSub1 <- wwf_eco[has_occurrence, ]
  
}

sf::sf_use_s2(TRUE)



#-----------------------------------------------
#--------- Import right bias grid --------------
#-----------------------------------------------

if (speciesgroup %in% names(bias_grid_paths)) {
  
  biasgrid <- terra::rast(bias_grid_paths[[speciesgroup]])
  
} else {
  
  warning("No bias grid available for this group. Group has to be one of the following:
Plants, Amphibians, Birds, Mammals, Molluscs, Insects, or Reptiles.")
  
  biasgrid <- NULL
  
}


#---------------------------------------------------------
#------ Mask biasgrid by ecoregions with occurrences -----
#---------------------------------------------------------

wwf_ecoSub1_ext <- terra::ext(wwf_ecoSub1)

# Convert wwf_ecoSub1 to a SpatVector that can be used for masking
if(sum(class(wwf_ecoSub1)[1] != "SpatVector") > 0){wwf_ecoSub1_vector <- vect(wwf_ecoSub1) 
}else{wwf_ecoSub1_vector <- wwf_ecoSub1}


if(!is.null(biasgrid)){
  
  # Crop biasgrid to extent wwf_ecoSub1
  biasgrid_crop <- terra::crop(biasgrid, wwf_ecoSub1_ext) 
  
  # Mask cropped biasgrid with SpatVector
  biasgrid_sub <- terra::mask(biasgrid_crop, wwf_ecoSub1_vector)
  
  # Mask biasgrid with one of the climatic layers, to make sure it doesn't extend beyond them
  climategrid_rast <- terra::crop(globalclimpreds_terra[[1]], wwf_ecoSub1_ext)
  
  # if the extents do not match, resample
  if(ext(biasgrid_sub) != ext(climategrid_rast)){
    biasgrid_sub <- terra::resample(biasgrid_sub, climategrid_rast, method = "bilinear")
  }
  
  biasgrid_sub <- terra::mask(biasgrid_sub, climategrid_rast) 
  
  
  ######### not sure about this passage?
  # Rescale raster values to range from 1 to 20
  
  # min_val <- global(biasgrid_sub, fun = "min", na.rm = TRUE)[[1]]
  # max_val <- global(biasgrid_sub, fun = "max", na.rm = TRUE)[[1]]
  # biasgrid <- ((biasgrid_sub - min_val) / (max_val - min_val)) * 19 + 1
  
}

# Create alternative raster consisting of only ecoregions without biasgrid mask, 
# used only when not enough pseudoabsence points can be generated using biasgrid_sub as mask layer
# First crop one of the climate rasters to extent ecoregions
ecoregions_crop   <- terra::crop(globalclimpreds_terra[[1]], wwf_ecoSub1_ext) 
# Mask with ecoregions vector
ecoregions_raster <- terra::mask(ecoregions_crop,wwf_ecoSub1_vector) 




#--------------------------------------------
#---------- Generate pseudoabsences ---------
#--------------------------------------------

# Mask cells that contain occurrences
for_PA_vect <- terra::vect(for_PA_selection)
cells_with_occurrences <- terra::cellFromXY(biasgrid_sub, terra::crds(for_PA_vect))
biasgrid_sub[cells_with_occurrences] <- NA

# Generate 30000 random pseudoabsences
set.seed(728)
global_points <- terra::spatSample(
  biasgrid_sub,
  size = 30000,           # three times the number we need
  method = "weights",     # weighted random sampling
  as.points = TRUE,       # return SpatVector of points
  na.rm = TRUE            # ignore NA pixels
)

invisible(gc())

# Select 10000 pseudoabsences
if(pseudoabsence_thinning_method == "random"){
  print("Thinning pseudoabsences randomly")
  set.seed(101) 
  global_points <- global_points[sample(nrow(global_points), 10000, replace=FALSE), ]%>%
    sf::st_as_sf()
  
  coords <- sf::st_coordinates(global_points)
  
  global_points <- global_points %>%
    dplyr::mutate(decimalLongitude = coords[, "X"],
                  decimalLatitude  = coords[, "Y"]) %>%
    dplyr::select(decimalLongitude, decimalLatitude, geometry)
  
}else if (pseudoabsence_thinning_method == "kmeans_clustering"){
  print("Thinning pseudoabsences based on k-means clustering")
  
  # Extract environmental data from filtered pseudoabsences
  pa_climate_data <- terra::extract(globalclimpreds_terra, global_points, ID = FALSE, xy = TRUE)
  
  # Remove rows with any NA values (could happen as they are extracted from 5k aggregated pixels)
  pa_climate_data <- na.omit(pa_climate_data)
  
  #Check how many unique rows there are and set centers to lowest of either 10000 or #unique rows
  unique_centers <- nrow(unique(pa_climate_data))
  center_number <- min(unique_centers, 10000)
  
  # K-means clustering
  set.seed(101)
  clust <- kmeans(pa_climate_data[, !names(pa_climate_data) %in% c("x", "y")], centers = center_number,iter.max = 10, nstart = 1)$cluster
  pa_climate <- cbind(pa_climate_data, clust)%>%
    dplyr::mutate(rID = row_number())
  
  # Keep 1 pseudoabsence per cluster
  sampled <- pa_climate %>%
    dplyr::group_by(clust) %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::ungroup()
  
  # How many pseudoabsences do we still need
  remaining <- 10000 - nrow(sampled)
  
  # sample extra pseudoabsences if fewer than 10000
  if (remaining > 0) {
    # Randomly sample additional pseudoabsences excluding already chosen ones
    extra_pa <- pa_climate %>%
      dplyr::filter(!rID %in% sampled$rID)%>%
      dplyr::slice_sample(n = remaining) 
    
    global_points <- bind_rows(sampled, extra_pa)
    rm(extra_pa)
    
  } else {
    global_points <- sampled
  }
  
  # Keep only occurrence columns
  global_points <- global_points %>%
    dplyr::rename("decimalLongitude" = x,
                  "decimalLatitude" = y)%>%
    dplyr::select(decimalLongitude, decimalLatitude)%>%
    sf::st_as_sf(coords=c("decimalLongitude", "decimalLatitude"), crs=project_projection, remove=FALSE)
  
  rm(pa_climate_data, pa_climate, sampled, remaining, unique_centers, center_number, clust)
}

invisible(gc())



#--------------------------------------------
#--- Create presence-pseudoabsence dataset---
#--------------------------------------------

# Add coordinates and convert
global_pseudoAbs <- global_points %>%
  dplyr::mutate(species = 0)

# Combine with presence data
global_presabs <- rbind(global.occ.sf, global_pseudoAbs)

# Clean up
rm(global_points, global_pseudoAbs)



#--------------------------------------------
#---- Extract climate data for modelling-----
#--------------------------------------------

global.data.df <- sdm::sdmData(species~.,
                               train = vect(global_presabs),
                               predictors = globalclimpreds_terra) %>%
  as.data.frame()



#--------------------------------------------
#--- Remove highly correlated predictors---
#--------------------------------------------

# Calculate correlation matrix (excluding rID and species)
correlationMatrix <- cor(global.data.df[, -c(1, 2)])

# Identify highly correlated variables (cutoff = 0.7)
highlyCorrelated <- caret::findCorrelation(correlationMatrix, 
                                           cutoff = 0.7, 
                                           exact = TRUE, 
                                           names = TRUE)

# Remove highly correlated predictors and rID, and prepare species factor
global.data.df.uncor <- global.data.df %>%
  dplyr::select(-all_of(highlyCorrelated), -rID) %>%
  dplyr::mutate(
    species = as.factor(species),
    species = recode_factor(species, '0' = "absent", '1' = "present"),
    species = relevel(species, ref = "present")
  )

# Remove them from global climate stack
globalclimpreds_terra_selection <- globalclimpreds_terra %>%
  subset(!names(globalclimpreds_terra) %in% highlyCorrelated)

# Remove them from European climate stack
eu_climpreds.10_selection <- eu_climpreds.10 %>%
  subset(!names(eu_climpreds.10) %in% highlyCorrelated)

# Remove them from the country stack
if(tolower(country_of_interest)!="europe"){
  country_climpreds <- terra::rast(country_climpreds_file)
  country_climpreds_selection <- country_climpreds %>%
    subset(!names(country_climpreds) %in% highlyCorrelated)
  rm(country_climpreds)
  gc()
}else{
  country_climpreds_selection <- eu_climpreds.10_selection
}


# - # - # - # - # - # - # - # - # - # - # - # - # - # 
#                                                   #
#                                                   #
#           MODELING - PREDICTIONS (CURRENT)        #
#                                                   #
#                                                   #
# - # - # - # - # - # - # - # - # - # - # - # - # - # 



#--------------------------------------------
#--- Run multiple machine learning models ---
#--------------------------------------------

# here, multiple SDM algorithms are trained, to generate global favourability maps for each algorithm
# then PCA is used to identify which algorithms best represent the dominant spatial pattern of predicted suitability

# Define prevalence ratio
n1 <- nrow(global.occ.sf)  # presences
n0 <- sum(global_presabs$species == 0) # pseudoabsences (adjust to your setup if different)
prev_ratio <- n1 / n0

# Define methods and data
sdm_data <- sdm::sdmData(species~.,
                         train = vect(global_presabs),
                         predictors = globalclimpreds_terra_selection)

methods <- c("glm", "gam", "bioclim", "brt", "rf", "glmpoly", "mars", "maxent", "fda", "cart") 

# Run model
model <- sdm::sdm(
  species ~ ., 
  data = sdm_data,
  methods = methods # 10 models
)

print(model)



#-------------------------------------------------------------------
#--- Make predictions for each model that will be used for PCAm ----
#-------------------------------------------------------------------

# the model above is fitted globally, but current predictions are made only for Europe

# Get model info
info <- sdm::getModelInfo(model)

# custom function for row cutting of rasters
make_row_extents <- function(r, nblocks, overlap_rows = 0) {
  stopifnot(inherits(r, "SpatRaster"))
  nr <- terra::nrow(r)
  resy <- terra::res(r)[2]
  xmn <- terra::xmin(r); xmx <- terra::xmax(r)
  ymn <- terra::ymin(r); ymx <- terra::ymax(r)
  
  # Use block_size to avoid duplicate row breaks from rounding
  block_size <- ceiling(nr / nblocks)
  
  exts <- vector("list", nblocks)
  for (i in seq_len(nblocks)) {
    r1 <- (i - 1) * block_size + 1
    r2 <- min(i * block_size, nr)
    # Add overlap and clamp to [1, nr]
    r1 <- max(1, r1 - overlap_rows)
    r2 <- min(nr, r2 + overlap_rows)
    if (r2 < r1) next  # should not happen, but guard anyway
    
    # Centers of first/last rows
    y_center_top    <- terra::yFromRow(r, r1)  # larger y
    y_center_bottom <- terra::yFromRow(r, r2)  # smaller y
    
    # Convert to edges (top and bottom of the rows)
    y_top    <- min(ymx, y_center_top + resy / 2)     # top edge (max y)
    y_bottom <- max(ymn, y_center_bottom - resy / 2)  # bottom edge (min y)
    
    # Now ymin < ymax as required
    exts[[i]] <- terra::ext(xmn, xmx, y_bottom, y_top)
  }
  exts
}

# decide if you want to do the cutting according to wiSDM or the other one
if(latitudinal_cutting == "wiSDM"){
  
  # Define extent to cut eu_climpreds.10_selection in 4 latitudinal blocks to make predictions more efficient
  nblocks <- 4 # number of chunks in which the raster is splitted
  e <- terra::ext(eu_climpreds.10_selection) # Europe
  ybreaks <- seq(e$ymin, e$ymax, length.out = nblocks + 1)
  exts <- lapply(1:nblocks, function(i) ext(e$xmin, e$xmax, ybreaks[i], ybreaks[i+1]))
  
  pred_blocks <- vector("list", nblocks)
  
  # Create empty list to store models in
  modeloutput <- list()
  
  tic("Creating favorability raster")
  
  for(modelmethod in methods){
    
    print(modelmethod)
    
    pred_raster <- try({
      
      for(rasterblock in seq_along(exts)) {
        
        block_r <- crop(eu_climpreds.10_selection, exts[[rasterblock]]) # Europe
        
        # Make predictions for each block
        pred_blocks[[rasterblock]] <- predict(model,
                                              newdata = block_r,
                                              method = modelmethod)
      }
      
      # Merge blocks only if all succeed
      do.call(terra::merge, pred_blocks)
      
    }, silent = TRUE)
    
    invisible(gc())
    
    # If prediction failed entirely (full raster + blocks), skip to next method
    if(inherits(pred_raster, "try-error")) {
      message("Skipping method ", modelmethod, " due to prediction failure.")
      next
    } else{
      message("Predictions successfully completed for method '", modelmethod, "'.")
    }
    
    # Get model IDs
    model_ids <- info$modelID[info$method == modelmethod]
    
    # Subset using those IDs
    method_model <- model[[model_ids]]  
    
    # Apply the transformation to the raster
    fav_raster <- favourability_from_prob(pred_raster, prev_ratio)
    
    # Store
    modeloutput[[modelmethod]] <- fav_raster
    
    rm(fav_raster, method_model)
  }
  
  toc()
  
}else if(latitudinal_cutting == "row_indices"){
  
  # there the problem is that dividing in blocks risk to create little mismatches,
  # and it can happen that those mismatches are exactly where the (few) presences of the species are
  # so we can still divide in blocks but using row indices
  
  tic("Creating favorability raster using row indices approach")
  
  nblocks <- 4 # number of chunks in which the raster is splitted
  # get an example raster
  r0 <- eu_climpreds.10_selection[[1]]
  nr <- terra::nrow(r0)
  row_breaks <- round(seq(1, nr + 1, length.out = nblocks + 1))
  nblocks <- 4
  #overlap_rows <- 1
  #exts <- make_row_extents(r0, nblocks = nblocks, overlap_rows = overlap_rows)
  
  exts <- make_row_extents(r0, nblocks = nblocks)
  
  # check 
  all(sapply(exts, function(e) e$ymin < e$ymax))
  
  pred_blocks <- vector("list", nblocks)
  
  # Create empty list to store models in
  modeloutput <- list()
  
  for(modelmethod in methods){
    
    print(modelmethod)
    
    pred_raster <- try({
      
      for(rasterblock in seq_along(exts)) {
        
        block_r <- crop(eu_climpreds.10_selection, exts[[rasterblock]]) # Europe
        
        # Make predictions for each block
        pred_blocks[[rasterblock]] <- predict(model,
                                              newdata = block_r,
                                              method = modelmethod)
      }
      
      # Merge blocks only if all succeed
      do.call(terra::merge, pred_blocks)
      
    }, silent = TRUE)
    
    invisible(gc())
    
    # If prediction failed entirely (full raster + blocks), skip to next method
    if(inherits(pred_raster, "try-error")) {
      message("Skipping method ", modelmethod, " due to prediction failure.")
      next
    } else{
      message("Predictions successfully completed for method '", modelmethod, "'.")
    }
    
    # Get model IDs
    model_ids <- info$modelID[info$method == modelmethod]
    
    # Subset using those IDs
    method_model <- model[[model_ids]]  
    
    # Apply the transformation to the raster
    fav_raster <- favourability_from_prob(pred_raster, prev_ratio)
    
    # Store
    modeloutput[[modelmethod]] <- fav_raster
    
    rm(fav_raster, method_model)
  }
  
  toc()
  
}

# Combine into a SpatRaster stack
fav_stack <- terra::rast(modeloutput)

# avoid rounding for now, because for some very endemic species rounding 
# to 3 decimals collapses small but non-zero predictions to 0 and large to 1, 
# creating an artificial “binary” look 

# fav_stack <- round(fav_stack, 3)

# Assign layer names based on model methods
names(fav_stack) <- names(modeloutput)
invisible(gc())



#---------------------------------------------
#-- Choose five final models PCAm method -----
#---------------------------------------------

# a PCA needs prediction layers that contain valid, finite, non-constant information
# so I can't do it if I have all 0s or NaN in a layer

fav_stack_copy <- fav_stack

# create a custom function to evaluate the layers
layer_check <- function(r) {
  v <- terra::values(r, mat = FALSE)
  v <- v[is.finite(v)]
  
  if (length(v) == 0) return("all_invalid")
  if (stats::var(v) == 0) return("constant")
  "ok"
}

status <- sapply(1:nlyr(fav_stack), function(i) layer_check(fav_stack[[i]]))
names(status) <- names(fav_stack)
status # here I can see which algorithms performed well and which ones didn't

# now I keep only those that can go with the PCA
fav_stack_clean <- fav_stack[[status == "ok"]]

# # PCA needs pixels with valid values in ALL layers, so we mask NAs
# valid_mask <- terra::app(fav_stack_clean, fun = function(x) all(is.finite(x)))
# fav_stack <- terra::mask(fav_stack_clean, valid_mask, maskvalues = 0)

fav_stack <- fav_stack_clean

# Step 0: make PCA
set.seed(100)
pca_result <- RStoolbox::rasterPCA(fav_stack, nSamples = 100000, spca = FALSE, maskCheck = FALSE)

# Step 1: Extract PC1 loadings (PC1 explains the vast majority of variance)
loadings <- pca_result$model$loadings[, 1]  # Comp.1 = PC1
names(loadings) <- rownames(pca_result$model$loadings)

# Step 2: Convert raster stack to matrix (rows = pixels, cols = models)
invisible(gc())
fav_matrix <- as.matrix(fav_stack)

# Step 3: Calculate variance along PC1 for each model
model_variances <- setNames(numeric(nlyr(fav_stack)), names(fav_stack))

for (lyr in 1:nlyr(fav_stack)) {
  model_vals <- fav_matrix[, lyr]
  centered <- model_vals - mean(model_vals, na.rm = TRUE)
  projection <- centered * loadings[lyr]
  model_variances[lyr] <- var(projection, na.rm = TRUE)
}

# Step 4: Select top 5 models with highest variance on PC1
top5_models <- names(sort(model_variances, decreasing = TRUE))[1:5]
cat("Top 5 models by variance along PC1:\n", top5_models)

# Get model IDs
top_ids <- info$modelID[info$method %in% top5_models]

# Subset using those IDs
top5models <- model[[top_ids]]  

# Step 5: Subset fav_stack to top 5 layers
top5_stack <- subset(fav_stack, top5_models)

# Step 6: Compute pixel-wise median = consensus model
consensus_median <- app(top5_stack, median)

# Step 7: Compute pixel-wise mean for SD calculation
consensus_mean <- mean(top5_stack, na.rm=TRUE)

# Step 8: Compute pixel-wise population SD
consensus_sd <- stdev(top5_stack, pop=TRUE)

# Step 9: Create country_level layers if relevant
if(tolower(country_of_interest) == "europe"){
  ensemble_suitability <- consensus_median
  ensemble_sd <- consensus_sd
  ensemble_mean <- consensus_mean
}else{
  ensemble_suitability<- consensus_median%>%
    terra::crop(country_boundary)%>%
    terra::mask(country_boundary)
  
  ensemble_sd<- consensus_sd%>%
    terra::crop(country_boundary)%>%
    terra::mask(country_boundary)
  
  ensemble_mean<- consensus_mean%>%
    terra::crop(country_boundary)%>%
    terra::mask(country_boundary)
}

# Clean up
rm(fav_matrix)
invisible(gc())


#------------------------------------------
#-- Create map with ensemble suitability --
#------------------------------------------

# Define name of files
base_file <- paste0(basefile, "current_ensemble")

# Export PDFs with and without occurrences plotted
for (occs in list(NULL, global.occ.sf)){
  filename <- ifelse(is.null(occs), base_file, paste0(base_file, "_occ"))
  
  exportPDF(predictions = ensemble_suitability,
            dataType = "Suit",
            period = "Current",
            returnPredictions = FALSE,
            returnPNG = FALSE,
            occ_data=occs,
            exportPNG=TRUE,
            PDF_title = PDF_title,
            PNG_folder=file.path(base_dir, "Climate", "Current", "Predictions", "PNGs"),
            PDF_folder=file.path(base_dir, "Climate", "Current", "Predictions","PDFs"),
            filename = filename)
}


#------------------------------------------
#---------- Create map with ensemble SD ---
#------------------------------------------

# Define name of files
filename <- paste0(basefile, "current_ensemble_SD")

exportPDF(predictions = ensemble_sd,
          dataType = "Stdev",
          period = "Current",
          returnPredictions = FALSE,
          returnPNG = FALSE,
          occ_data=NULL,
          exportPNG=TRUE,
          PDF_title = PDF_title,
          PNG_folder=file.path(base_dir, "Climate", "Current", "Diagnostics", "Confidence_maps", "PNGs"),
          PDF_folder=file.path(base_dir, "Climate", "Current", "Diagnostics", "Confidence_maps","PDFs"),
          filename = filename)


#------------------------------------------
#------------ Create binary map -----------
#------------------------------------------

# Get predictor values at occurrence points
# to predict only where the occurrences are
# and not across the whole globe/region of interest

predictors_only <- global.data.df.uncor%>%
  dplyr::filter(species == "present")%>%
  dplyr::select(-species)

# Predict for top 5 models
pred_vals <- list()
for (method in top5_models) {
  pred_vals[[method]] <- predict(model, newdata = predictors_only, method = tolower(method))
}

# Favourability transformation
fav_vals <- lapply(pred_vals, function(p) favourability_from_prob(p[[1]], prev_ratio))

# Create one df with the median favorability value for each occurrence
fav_vals <- fav_vals %>%
  do.call(cbind, .) %>%
  as.data.frame() %>%
  dplyr::mutate(median = apply(., 1, median, na.rm = TRUE)) %>% #1 = apply to rows
  dplyr::select(median)

# Create binary maps
binary_maps <- list()

raster_folder <- file.path(base_dir, "Climate","Current", "Predictions", "Rasters")

for (probs in mtp_probabilities){
  
  # Define mtp_pct and mtp_value
  mtp_value <- probs*100
  mtp_pct <- paste0(mtp_value, "%")
  mtp_text <- paste0(mtp_value,"pct")
  
  # Obtain threshold
  to_omit <- floor(probs * nrow(fav_vals)) #Define how many lowest ranked occs to omit based on mtp threshold
  thr <- sort(fav_vals$median)[to_omit + 1]
  cat(paste0("Mean ",mtp_pct," minimum training presence threshold climate model: ", round(thr, 4), "\n"))
  
  # Create binary raster using MTP threshold
  binary_map_pct <- ensemble_suitability >= thr 
  binary_map_pct <- as.factor( binary_map_pct*1) #Convert TRUE/FALSE to 1/0 and then to Present/Absent
  levels( binary_map_pct) <- data.frame(ID = c(0, 1),
                                        class = c("Absent", "Present"))
  
  # Store raster
  binary_file <- file.path (raster_folder, paste0(basefile,"current_binary",mtp_value,"pct.tif"))
  terra::writeRaster(binary_map_pct, filename = binary_file, overwrite = TRUE)
  
  # export as PDF and PNG with and without occurrences plotted 
  base_file <- paste0(basefile, "current_binary",mtp_value,"pct")
  for (occs in list(NULL, global.occ.sf)){
    filename <- ifelse(is.null(occs), base_file, paste0(base_file, "_occ"))
    exportPDF(predictions = binary_map_pct,
              dataType = "Binary",
              period = "Current",
              returnPredictions = FALSE,
              returnPNG = TRUE,
              occ_data=occs,
              exportPNG=TRUE,
              LabelValue= round(thr,3),
              LabelName=paste0(mtp_pct, " MTP threshold"),
              PDF_title = PDF_title,
              PNG_folder=file.path(base_dir, "Climate","Current", "Predictions", "PNGs"),
              PDF_folder=file.path(base_dir,"Climate" ,"Current", "Predictions", "PDFs"),
              filename = filename)
  }
  
  #assign(paste0(mtp_value,"pct"), thr)
  binary_maps[[mtp_pct]] <- list(binary_raster=binary_map_pct,
                                 mean_MTP= thr)
  rm(binary_map_pct, binary_file, mtp_value, mtp_pct, to_omit, thr)
}



# - # - # - # - # - # - # - # - # - # - # - # - # - # 
#                                                   #
#                                                   #
#           MODELING - PROJECTIONS (FUTURE)         #
#                                                   #
#                                                   #
# - # - # - # - # - # - # - # - # - # - # - # - # - # 



#--------------------------------------------------------------
#------- Create maps with future projections for Europe -------
#--------------------------------------------------------------

for (period in c("2041-2070","2071-2100")){
  for(scenario in c("ssp126", "ssp370", "ssp585")){
    
    #if(period == "2041-2070" && scenario == "ssp126"){next}
    
    print(paste("[FUTURE] Projecting:", period, scenario))
    
    # Get future climate data for specific period and scenario
    future_rast <- terra::rast(future_paths[[paste0(period, "_", scenario)]])
    
    # Keep relevant predictors in the raster stack
    future_selection <- future_rast %>%
      subset(names(country_climpreds_selection))
    
    # decide if you want to do the cutting according to wiSDM or the other one
    if(latitudinal_cutting == "wiSDM"){
      
      tictoc::tic("Creating favorability raster using wiSDM approach")
      
      # Define extents to cut future climate rasters into 4 latitudinal blocks
      nblocks <- 4
      e <- terra::ext(future_selection)
      ybreaks <- seq(e$ymin, e$ymax, length.out = nblocks + 1)
      exts <- lapply(1:nblocks, function(i) ext(e$xmin, e$xmax, ybreaks[i], ybreaks[i+1]))
      pred_blocks <- vector("list", nblocks)
      
      # Project each of the top 5 models
      future_modeloutput <- list()
      
      for(modelmethod in top5_models){
        
        pred_raster_future  <- try({
          
          for(rasterblock in seq_along(exts)) {
            
            # Crop climate rasters into one of the 4 latitudinal rasterblocks
            block_r <- crop(future_selection, exts[[rasterblock]])
            
            # Make predictions for that block
            pred_blocks[[rasterblock]] <- predict(model,
                                                  newdata = block_r,
                                                  method = modelmethod)
          }
          
          # Merge blocks only if all succeed
          do.call(terra::merge, pred_blocks)
          
        }, silent = TRUE)
        
        
        # If prediction failed entirely (full raster + blocks), skip to next method
        if(inherits( pred_raster_future , "try-error" )) {
          message("Skipping method ", modelmethod, " due to prediction failure.")
          next
        } else{
          message("Predictions successfully completed for method '", modelmethod, "'.")
        }
        
        fav_raster_future <- favourability_from_prob(pred_raster_future, prev_ratio)
        future_modeloutput[[modelmethod]] <- fav_raster_future
        rm(fav_raster_future, pred_raster_future)
      }
      tictoc::toc()
    }else if(latitudinal_cutting == "row_indices"){
      
      # there the problem is that dividing in blocks risk to create little mismatches,
      # and it can happen that those mismatches are exactly where the (few) presences of the species are
      # so we can still divide in blocks but using row indices
      
      tictoc::tic("Creating favorability raster using row indices approach")
      
      # get an example raster
      r0 <- future_selection[[1]]
      nr <- terra::nrow(r0)
      row_breaks <- round(seq(1, nr + 1, length.out = nblocks + 1))
      nblocks <- 4
      #overlap_rows <- 1
      #exts <- make_row_extents(r0, nblocks = nblocks, overlap_rows = overlap_rows)
      
      exts <- make_row_extents(r0, nblocks = nblocks)
      
      # check 
      all(sapply(exts, function(e) e$ymin < e$ymax))
      
      pred_blocks <- vector("list", nblocks)
      
      # Project each of the top 5 models
      future_modeloutput <- list()
      
      for(modelmethod in top5_models){
        
        pred_raster_future  <- try({
          
          for(rasterblock in seq_along(exts)) {
            
            # Crop climate rasters into one of the 4 latitudinal rasterblocks
            block_r <- crop(future_selection, exts[[rasterblock]])
            
            # Make predictions for that block
            pred_blocks[[rasterblock]] <- predict(model,
                                                  newdata = block_r,
                                                  method = modelmethod)
          }
          
          # Merge blocks only if all succeed
          do.call(terra::merge, pred_blocks)
          
        }, silent = TRUE)
        
        
        # If prediction failed entirely (full raster + blocks), skip to next method
        if(inherits( pred_raster_future , "try-error" )) {
          message("Skipping method ", modelmethod, " due to prediction failure.")
          next
        } else{
          message("Predictions successfully completed for method '", modelmethod, "'.")
        }
        
        fav_raster_future <- favourability_from_prob(pred_raster_future, prev_ratio)
        future_modeloutput[[modelmethod]] <- fav_raster_future
        rm(fav_raster_future, pred_raster_future)
      }
      tictoc::toc()
    }
    
    # Create Ensemble predictions for future
    future_fav_stack <- terra::rast(future_modeloutput)
    future_consensus_median <- app(future_fav_stack, median)
    future_consensus_mean <- mean(future_fav_stack, na.rm=TRUE)
    future_consensus_sd <- stdev(future_fav_stack, pop=TRUE)
    
    # Export future ensemble raster (favorability) 
    future_folder <- file.path(base_dir, "Climate", period, scenario, "Predictions", "Rasters")
    ensemble_file <- file.path(future_folder, paste0(basefile, period,"_",scenario,"_ensemble.tif"))
    terra::writeRaster(future_consensus_median, filename = ensemble_file, overwrite = TRUE)
    
    # Export future sd raster 
    future_sd_folder <- file.path(base_dir, "Climate", period, scenario, "Diagnostics", "Confidence_maps", "Rasters")
    ensemble_sd_file <- file.path(future_sd_folder, paste0(basefile, period,"_",scenario,"_ensemble_SD.tif"))
    terra::writeRaster(future_consensus_sd, filename = ensemble_sd_file, overwrite = TRUE)
    
    # Export future mean raster 
    future_mean_folder <- file.path(base_dir, "Climate", "Current", "Interim")
    ensemble_mean_file <- file.path(future_mean_folder, paste0(basefile, period,"_",scenario,"_ensemble_mean.tif"))
    terra::writeRaster(future_consensus_mean, filename = ensemble_mean_file, overwrite = TRUE)
    
    # Export ensemble predictions as PDF and PNG with and without occurrences
    base_file <- paste0(basefile, scenario,"_", period,"_ensemble")
    
    for (occs in list(NULL, global.occ.sf)){
      filename <- ifelse(is.null(occs), base_file, paste0(base_file, "_occ"))
      
      exportPDF(predictions = future_consensus_median,
                dataType = "Suit",
                period = period,
                scenario = scenario,
                returnPredictions = FALSE,
                returnPNG = TRUE,
                occ_data=occs,
                exportPNG=TRUE,
                PDF_title=PDF_title,
                PNG_folder=file.path(base_dir, "Climate", period, scenario, "Predictions", "PNGs"),
                PDF_folder=file.path(base_dir, "Climate", period, scenario, "Predictions", "PDFs"),
                filename = filename)
    }
    
    # Export ensemble SD predictions as PDF and PNG 
    filename<- paste0(basefile, scenario,"_", period,"_ensemble_SD")
    
    exportPDF(predictions = future_consensus_sd,
              dataType = "Stdev",
              period = period,
              scenario = scenario,
              returnPredictions = FALSE,
              returnPNG = TRUE,
              occ_data=NULL,
              exportPNG=TRUE,
              PDF_title=PDF_title,
              PNG_folder=file.path(base_dir, "Climate", period, scenario, "Diagnostics", "Confidence_maps", "PNGs"),
              PDF_folder=file.path(base_dir, "Climate", period, scenario, "Diagnostics", "Confidence_maps", "PDFs"),
              filename = filename)
    
    
    # Create binarized ensemble predictions for future
    for(probs in mtp_probabilities){
      
      # Define mtp_pct and mtp_value
      mtp_label <- paste0(probs*100, "%")
      mtp_text <- paste0(probs*100,"pct")
      
      # Get threshold value and apply to consensus predictions
      threshold <- binary_maps[[mtp_label]]$mean_MTP
      binary_map_future <- future_consensus_median  >= threshold
      binary_map_future <- as.factor( binary_map_future*1) #Convert TRUE/FALSE to 1/0 and then to Present/Absent
      levels( binary_map_future) <- data.frame(ID = c(0, 1),
                                               class = c("Absent", "Present"))
      
      # Store raster
      binary_file <- file.path(future_folder, 
                               paste0(basefile, period,"_",scenario,"_binary",mtp_text,".tif"))
      terra::writeRaster(binary_map_future, filename = binary_file, overwrite = TRUE)
      
      # Export binarized ensemble predictions as PDF and PNG with and without occurrences 
      base_file <- paste0(basefile, period,"_", scenario, "_binary",mtp_text)
      
      for (occs in list(NULL, global.occ.sf)){
        
        filename <- ifelse(is.null(occs), base_file, paste0(base_file, "_occ"))
        exportPDF(predictions = binary_map_future,
                  dataType = "Binary",
                  period = period,
                  scenario = scenario,
                  occ_data=occs,
                  returnPredictions = FALSE,
                  returnPNG = TRUE,
                  exportPNG=TRUE,
                  LabelValue= round(threshold,3),
                  LabelName=paste0(mtp_label, " MTP threshold"),
                  PDF_title=PDF_title,
                  PNG_folder=file.path(base_dir,"Climate", period, scenario, "Predictions", "PNGs"),
                  PDF_folder=file.path(base_dir, "Climate",period, scenario, "Predictions", "PDFs"),
                  filename=filename)
      }
      rm(mtp_label, mtp_text, threshold, binary_map_future)
    }
  }
}



#--------------------------------------------------------
#------ Get response curves and variable importance -----
#--------------------------------------------------------

response_list <- list()
varimp_list <- list()

for(topmethod in top5_models){
  
  # Get model id
  id <- info$modelID[info$method == topmethod]
  
  # Get response curve
  response_curves <- sdm::getResponseCurve(model,id)@response
  
  # Get variable importance
  varimp <- sdm::getVarImp(model,id)@varImportance
  
  # Store
  response_list[[topmethod]] <- response_curves
  varimp_list[[topmethod]] <- varimp
  
}

# Convert list to a dataframe
response_df <- purrr::imap_dfr(response_list, function(model_list, model_name) {
  purrr::imap_dfr(model_list, function(df, var_name) {
    response_df <- df %>%
      setNames(c("Predictor_value", "Response"))%>%
      mutate( Algorithm = model_name,
              Predictor = var_name)})
}) %>%
  dplyr::select(Algorithm,Predictor, Predictor_value, Response)


varimp_df <- purrr::imap_dfr(varimp_list, function(df, model_name) {
  df %>%
    setNames(c("Predictor", "corTest" , "AUCtest"))%>%
    dplyr::mutate(Algorithm = model_name)
})%>%
  dplyr::select(Algorithm,Predictor, corTest, AUCtest)


# Plot response curves
response_plot <- ggplot(response_df, aes(x = Predictor_value,
                                         y = Response, 
                                         color = Algorithm)) +
  geom_line(linewidth=0.8) +
  facet_wrap(~ Predictor, scales = "free_x")+
  labs(title= "Climatological response curves" ,x= "Predictor value")+
  theme_bw()

# Plot variable importance 
varimp_plot <- ggplot(varimp_df, aes(x = Predictor, y = corTest)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  #horizontal bars
  facet_wrap(~ Algorithm) +  
  geom_hline(yintercept = 0, color = "black") + 
  labs(
    x = "Variable",
    y = "Importance",
    title = "Variable importance per model"
  ) +
  theme_bw()

# Save plot
PNG_folder <- file.path(base_dir, "Climate", "Current", "Diagnostics")

ggplot2::ggsave(filename = paste0(basefile, "variable_importance.png"), plot = varimp_plot ,  device = "png", width =8.27 , height = 5.845, path= file.path(PNG_folder, "Variable_importance") )
ggplot2::ggsave(filename = paste0(basefile, "response_curves.png"), plot = response_plot,  device = "png", width =8.27 , height = 5.845, path=  file.path(PNG_folder, "Response_curves") )



#---------------------------------------------------
#--------- Export results as .qs list --------------
#---------------------------------------------------

climatemodel <-list(species = species, #Species name
                    taxonkey = taxonkey, #Species taxonkey
                    global_data_df_uncor = global.data.df.uncor, #Data used to fit the model (climate data for each presence/pseudoabsence)
                    global_presabs = global_presabs,#xy coordinates of presences and pseudoabsences used to fit the models
                    occurrences = for_PA_selection, #All raw occurrences
                    # occurrences5km = global.occ.sf, #Processed occurrences used to fit the models
                    # occurrences1km = global.occ_1KM,
                    occurrences1km = global.occ.sf, #Processed occurrences used to fit the models
                    mtp_5_threshold = binary_maps$`5%`$mean_MTP,
                    mtp_1_threshold = binary_maps$`1%`$mean_MTP,
                    sdm_model = model,
                    pca_result = pca_result,
                    top5_models = top5_models,
                    response_df = response_df,
                    varimp_df = varimp_df,
                    top5models = top5models,
                    selected_predictors = names(globalclimpreds_terra_selection),
                    future_consensus_median = future_consensus_median) # NB this saves only the consensus from the last period/scenario combination


qs2::qs_save(climatemodel, file.path(base_dir, "Climate", paste0("Climate_model_",speciesName,"_",taxonkey,".qs")))


#-------------------------------------------------------
#--------- Export raster layers in folder "Rasters" ----
#-------------------------------------------------------

# We don't store them in .qs file as some important metadata would be stored in a temp folder, which would be removed after a while 
biasgrid_file<- file.path(base_dir,"Climate", "Current", "Interim", 
                          paste0("Biasgrid_",speciesName,"_",taxonkey,".tif"))
ensemble_median_file <- file.path( base_dir,"Climate", "Current", "Predictions", "Rasters",
                                   paste0(basefile, "current_ensemble.tif"))
ensemble_mean_file <- file.path( base_dir,"Climate", "Current", "Interim",
                                 paste0(basefile, "current_ensemble_mean.tif"))
ensemble_sd_file <- file.path( base_dir,"Climate", "Current","Diagnostics", "Confidence_maps", "Rasters",
                               paste0(basefile, "current_ensemble_SD.tif"))

terra::writeRaster(biasgrid_sub, filename = biasgrid_file, overwrite = TRUE)


#Export suitability predictions for europe (needed for mtp calculation in habitat script) and, if relevant, for country of interest
if(tolower(country_of_interest)=="europe"){
  terra::writeRaster(consensus_median, filename = ensemble_median_file, overwrite = TRUE)
  terra::writeRaster(consensus_mean, filename = ensemble_mean_file, overwrite = TRUE)
  terra::writeRaster(consensus_sd, filename = ensemble_sd_file, overwrite = TRUE)
}else{
  europe_ensemble_median_file<- file.path( base_dir,"Climate", "Current", "Predictions", "Rasters",
                                           paste0(basefile, "current_ensemble_Europe.tif"))
  terra::writeRaster(consensus_median, filename = europe_ensemble_median_file, overwrite = TRUE)
  terra::writeRaster(ensemble_suitability, filename = ensemble_median_file, overwrite = TRUE)
  terra::writeRaster(ensemble_mean, filename = ensemble_mean_file, overwrite = TRUE)
  terra::writeRaster(ensemble_sd, filename = ensemble_sd_file, overwrite = TRUE)
}



#--------------------------------------------
#-------- End of loop -----------------------
#--------------------------------------------
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units="hours")
cat("Success! Climate model with GBIF presences has been created for", sp, "in", round(elapsed, 2), "hours\n\n")
rm(list = ls())

# Clean terra tempfiles
terra::tmpFiles(remove = TRUE)