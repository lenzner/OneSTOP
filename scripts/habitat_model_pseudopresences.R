###############################################################
#################### EUROPEAN HABITAT MODEL ###################
###############################################################

# Started on 10.03.2026
# modified on 19.05.2026
# finalized on 28.07.2026
# by LT

# this is a script to fit a european habitat model
# for IUCN RL species
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
library(viridis)
library(RColorBrewer)
library(magick)
library(patchwork)
library(randomForest)
library(raster)
library(dismo)
library(caretEnsemble)
library(kableExtra)
library(gbm)
library(PresenceAbsence)
library(purrr)
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
source(file.path(getwd(), "slurm", "runs", "run02", "pseudopresences", "configurations_pseudopresences.R"))

# Assign species name
args = commandArgs(trailingOnly = TRUE)
print(args)
sp <- args[1]
sp <- sub("_", " ", sp)

message("========= Processing species ", sp, " for European habitat model with pseudo-presences")

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
#---- Define file paths of Europe current and future habitat layers ------------
#-------------------------------------------------------------------------------

# current
habitatstack_file <- paste0(database_path, "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3/global_PFT_2015_recl_eur_mw3.tif")

# future
future_paths <- list()

preprocessed_dir <- paste0(database_path, "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3")
files <- paste0(preprocessed_dir, "/", list.files(preprocessed_dir))[grepl(".tif", paste0(preprocessed_dir, "/", list.files(preprocessed_dir)))] 

for (period in c("2055","2085")){

  # reassign period name to be in accordance with CHELSA
  p <- ifelse(period == "2055", "2041-2070",
                   ifelse(period == "2085", "2071-2100", NA))
  
  for(scenario in c("SSP1_RCP26", "SSP3_RCP70", "SSP5_RCP85")){
    
    # Define output file
    out_file <- files[grepl(paste0(scenario, "_", period), files)]
    
    # reassign scenario name to be in accordance with CHELSA
    scenario <- ifelse(scenario == "SSP1_RCP26", "ssp126",
                               ifelse(scenario == "SSP3_RCP70", "ssp370",
                                      ifelse(scenario == "SSP5_RCP85", "ssp585", NA)))
    
    # Store path for later use
    future_paths[[paste0(p, "_", scenario)]] <- out_file # these will need to be stacked
    
  }
}


#--------------------------------------------
#---------   Load euboundary  ---------
#--------------------------------------------

lulc_folder <- paste0(database_path, "/Chen_et_al_2022/Global_PFT_Projections_recl_eur_mw3")
euboundary <- terra::rast(paste0(lulc_folder, "/", list.files(lulc_folder)[1]))
euboundary <- (euboundary*0+1)
euboundary <- terra::as.polygons(euboundary, dissolve = TRUE)  # merge adjacent cells
euboundary <- sf::st_as_sf(euboundary)  # convert to sf


#---------------------------------------------
#----- Load country boundary -----------------
#---------------------------------------------

if(tolower(country_of_interest)!="europe"){
  country_boundary <- sf::read_sf(here::here("data","external","GIS","Country","country.shp"))%>%
    sf::st_transform(crs(habitat_stack[[1]]))%>%
    terra::vect()
}else{
  country_boundary <- euboundary %>%
    terra::vect()
}



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#          SET UP MODELING          #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 
    


#--------------------------------------------
#--------Extract species-specific data  -----
#--------------------------------------------

# NB decide if you want to upload the WGS84 (lat/lon) or CEA WGS84 occurrences
if(project_projection == "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"){
  
  global <- read.csv(paste0("./data/processed/range_sampling/global.occ.cea.wgs84.LL_", project_version, ".csv"))
  
}else if(project_projection == "EPSG:4326"){
  
  global <- read.csv(paste0("./data/processed/range_sampling/global.occ.wgs84.LL_", project_version, ".csv"))
  
}

split_df <- global %>% filter(species == sp)

species <- sp

# Extract first two words of species name
speciesName <- sub("^(\\w+)\\s+(\\w+).*", "\\1_\\2", species)

# Extract rest of species name
nameExtension <- if (grepl("^\\S+\\s+\\S+$", species)) "" else sub("^\\S+\\s+\\S+\\s+", "", species)

# Specify species for plot title
species_title <- gsub("_", " ", speciesName)

# if there is a value in acceptedUsageKey, use that one
# otherwise use speciesKey
if(!is.na(unique(split_df$acceptedUsageKey))){
  
  message(sp, " is a synonym - using accepted taxon key...")
  taxonkey <- unique(split_df$acceptedUsageKey)
  
}else{
  
  taxonkey <- unique(split_df$speciesKey)
  
}

rm(global)



#-------------------------------------------------------
#-------- Prepare filenames and titles for export ------
#-------------------------------------------------------

# Prepare PDF title 
PDF_title <- bquote(italic(.(gsub("_", " ", speciesName))) ~ .(nameExtension) ~ "(" * .(taxonkey) * ")")

#Prepare current and future basefile
basefile <-  paste0(speciesName, "_Habitat_")
combined_basefile <-  paste0(speciesName, "_Combined_")
global_basefile <-  paste0(speciesName, "_Climate_")


#-----------------------------------------------------
#-------- Define file path of global model file ------
#-----------------------------------------------------

# Define base project folder 
base_dir <- file.path(output_path, projectname, paste0(speciesName, "_", taxonkey))

global_model_file <- file.path(base_dir,"Climate",
                               paste0("Climate_model_",speciesName,"_",taxonkey,".qs"))


#----------------------------------------------------------
#--------- Check if global model exists, if not, skip -----
#----------------------------------------------------------

if(file.exists(global_model_file)){
  
  # This was stored as part of  script 02_fit_global_model
  globalmodels <- qs2::qs_read(global_model_file)
  
  # Extract different data objects stored in globalmodels
  # since I didn't fit the climate model at 5x5 but at 1x1 km
  global.occ.sf <- globalmodels$occurrences1km %>% # FULL occurrence with coordinateUncertainty <= 1km
    sf::st_as_sf(.,coords = c("decimalLongitude", "decimalLatitude"), crs = project_projection)
  
}else{
  warning(paste0("Skipping species ", species, " because no global model could be fitted"))
  next  # Skip the rest of the loop and move to the next iteration
}

# check if the species has already been modelled for the habitat
habitat_qs_file <- file.path(base_dir, "Habitat",
                             paste0("Habitat_model_", speciesName, "_", taxonkey, ".qs"))

if(file.exists(habitat_qs_file)){ 
  message("Habitat model already exists for ", speciesName, ". Skipping.")
  quit(save = "no", status = 0)
}



#---------------------------------------------
#------------- Define folders ----------------
#---------------------------------------------

raster_folder <- file.path(base_dir, "Habitat", "Current", "Predictions", "Rasters")
climate_raster_folder <- file.path( base_dir,"Climate", "Current", "Predictions", "Rasters")


#--------------------------------------------
#------------ Import raster layers ----------
#--------------------------------------------

# Define file paths
biasgrid_file <- file.path(base_dir,"Climate", "Current", "Interim", paste0("Biasgrid_",speciesName,"_",taxonkey,".tif"))

# Load rasterlayers
habitat_stack <- terra::rast(habitatstack_file)
# get %
habitat_stack <- habitat_stack/10000

biasgrid_sub <- terra::rast(biasgrid_file)



#-------------------------------------------------
#--------------- Create EU folders ---------------
#-------------------------------------------------

# Define outputs, periods, and scenarios
# NB, LULC Chen et al. 2022 data is for 2055 and 2085
periods   <- c("Current","2041-2070", "2071-2100")
scenarios <- c("ssp126", "ssp370", "ssp585")
outputs   <- c("Rasters", "PDFs", "PNGs")

#Create folders for each combination
scenario_folders <- list()

for(period in periods){
  for(output in outputs){
    if(period=="Current"){
      loop_list <- list(list(path = file.path(base_dir, "Habitat", period,"Predictions",output),
                             name = paste("Habitat", period, "Predictions", output,  sep = "/")),
                        list(path = file.path(base_dir, "Combined", period,"Predictions",output),
                             name = paste("Combined", period,"Predictions", output,  sep = "/")),
                        list(path = file.path(base_dir, "Habitat", period,"Diagnostics", "Variable_importance"),
                             name = paste("Habitat", period, "Diagnostics", "Variable_importance", output,  sep = "/")),
                        list(path = file.path(base_dir, "Habitat", period,"Diagnostics", "Response_curves"),
                             name = paste("Habitat", period, "Diagnostics", "Response_curves", output,  sep = "/")),
                        list(path = file.path(base_dir, "Habitat", period,"Diagnostics", "Confidence_maps",output),
                             name = paste("Habitat", period, "Diagnostics", "Confidence_maps", output,  sep = "/")),
                        list(path = file.path(base_dir, "Combined", period,"Diagnostics", "Confidence_maps",output),
                             name = paste("Combined", period, "Diagnostics", "Confidence_maps", output,  sep = "/")))
      scenario_folders <- c(scenario_folders, loop_list)  
      
    }else{
      for(scenario in scenarios){
        loop_list <- list(list(path = file.path(base_dir, "Habitat", period, scenario, "Predictions", output),
                               name = paste("Habitat", period, scenario, output, sep = "/")),
                          list(path = file.path(base_dir, "Combined", period, scenario, "Predictions", output),
                               name = paste("Combined", period, scenario, output, sep = "/")),
                          list(path = file.path(base_dir, "Habitat", period, scenario, "Diagnostics", "Confidence_maps", output),
                               name = paste("Habitat", period, scenario,"Diagnostics", "Confidence_maps",  output, sep = "/")),
                          list(path = file.path(base_dir, "Combined", period, scenario, "Diagnostics", "Confidence_maps", output),
                               name = paste("Combined", period, scenario,"Diagnostics", "Confidence_maps",  output, sep = "/")))
        scenario_folders <- c(scenario_folders, loop_list)
      }
    }
  }
}

# Add Rasters/Interim folder
fixed_folders <- list(
  list(path = file.path(base_dir, "Habitat", "Current", "Interim"), 
       name = "Interim"))

# Combine 
folder_paths <- c(fixed_folders, scenario_folders)

# Check and create each folder if necessary
lapply(folder_paths, function(folder){
  create_folder(folder$path, folder$name)
})



#-----------------------------------------------
#----- Create subset of European records -------
#-----------------------------------------------

# Check for occurrences that fall within Europe
eu_occ <- global.occ.sf %>%
  st_transform(crs = st_crs(habitat_stack)) %>%
  sf::st_filter(sf::st_as_sfc(sf::st_bbox(euboundary))) %>% # using bounding box to avoid loosing e.g., Malta
  sf::st_coordinates() %>%
  as.data.frame() %>%
  dplyr::rename(decimalLongitude = X) %>%
  dplyr::rename(decimalLatitude = Y) 

message("Number of pseudo-presences in Europe for ", sp, ": ", nrow(eu_occ), " out of ", nrow(global.occ.sf), " globally")

rm(global.occ.sf)
invisible(gc())


#-----------------------------------------------
#----------- Process occurrences ---------------
#-----------------------------------------------

# Keep only one occurrence per grid cell
eu_occ <- remove_duplicates(occurrences =  eu_occ, rast_template = habitat_stack[[1]])

# Remove occurrences within grid cells with NA values
eu_occ <- tryCatch({
  
  # First attempt: Using the raw data frame
  remove_nodata_occurrences(occurrences = eu_occ, 
                            rast_template = habitat_stack[[1]], 
                            crs = project_projection)
}, error = function(e){
  
  message("Duplicated occurrence removal failed with error: ", e$message)
  message("Attempting recovery by converting to SpatVector...")
  
  # Convert to SpatVector (fallback)
  occ_vect <- terra::vect(eu_occ, 
                          geom = c("decimalLongitude", "decimalLatitude"), 
                          crs = project_projection,
                          keepgeom = T)
  
  # Second attempt: Using the SpatVector
  return(remove_nodata_occurrences(occ_vect, habitat_stack[[1]], project_projection))
})



#-----------------------------------------------
#------ Limit to 10,000 occupied grid cells ----
#-----------------------------------------------

if(nrow(eu_occ) > 10000){
  if(occurrence_thinning_method == "random"){
    print("Thinning occurrences randomly")
    set.seed(101) 
    eu_occ <- eu_occ[sample(nrow(eu_occ), 10000, replace=FALSE), ]
  }else if (occurrence_thinning_method == "kmeans_clustering"){
    print("Thinning occurrences based on k-means clustering")
    #Extract environmental data in each occurrence grid cell
    habitat_data <- terra::extract(habitat_stack, eu_occ, ID = FALSE)
    
    #Check how many unique rows there are and set centers to lowest of either 10000 or #unique rows
    unique_centers<-nrow(unique(habitat_data))
    center_number<-min(unique_centers, 10000)
    
    # K-means clustering
    set.seed(101)
    clust <- kmeans(habitat_data, centers = center_number,iter.max = 10, nstart = 1)$cluster
    occ_habitat <- cbind(eu_occ, habitat_data, clust)%>%
      dplyr::mutate(rID =row_number())
    
    # Keep 1 occurrence per cluster
    sampled <- occ_habitat %>%
      dplyr::group_by(clust) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::ungroup()
    
    # How many presences do we still need
    remaining <- 10000 - nrow(sampled)
    
    # sample extra occurrences if fewer than 10000
    if (remaining > 0) {
      # Randomly sample additional presences excluding already chosen ones
      extra_occ <- occ_habitat %>%
        dplyr::filter(!rID %in% sampled$rID)%>%
        dplyr::slice_sample(n = remaining) 
      
      eu_occ <- bind_rows(sampled, extra_occ)
      rm(extra_occ)
      
    } else {
      eu_occ <- sampled
    }
    
    # Keep only occurrence columns
    eu_occ <- eu_occ %>%
      dplyr::select(decimalLongitude, decimalLatitude, geometry)
    
    rm(habitat_data, occ_habitat, sampled, remaining, unique_centers, center_number, clust)
    
  }
}


#------------------------------------------------
#----- Check if at least 20 European records ----
#------------------------------------------------
if (nrow(eu_occ) < 20) {
  warning(paste(nrow(eu_occ)," occurrences in Europe for species:", species, 
                "\n- European model cannot be constructed, skipping to the next species."))
  next  # Skip to the next species in the loop
}


#--------------------------------------------
#----- Clip biasgrid to European extent -----
#--------------------------------------------

# Reproject biasgrid_sub to match CRS, extent, and resolution of habitat_stack

if( !crs(biasgrid_sub, proj = T) == project_projection){
  
  warning("Biasgrid is not in the same CRS of the project! Reprojecting...")
  
  biasgrid_aligned <- terra::project(
    biasgrid_sub,
    habitat_stack[[1]],
    method = "bilinear")
  
}else{biasgrid_aligned <- biasgrid_sub}

# Mask biasgrid with habitat raster (so no PA can be selected in cells that are NA in habitat rasters)
biasgrid_aligned <- terra::resample(biasgrid_aligned, habitat_stack[[1]], method = "near")
# ext(biasgrid_aligned) == ext(habitat_stack[[1]])
biasgrid_aligned <- terra::mask(biasgrid_aligned, habitat_stack[[1]])



#-------------------------------------------
#------- Select invaded WWF ecoregions------
#-------------------------------------------

# Load WWF ecoregions
wwf_ecoregions <- sf::st_read(file.path(database_path, "wwf_terrestrial_ecoregions", "newRealms_cea.gpkg"))

# Project the data to the same CRS as the predictors raster stack
wwf_ecoregions <- sf::st_transform(wwf_ecoregions, st_crs(habitat_stack[[1]])) %>%
  sf::st_make_valid()

# Identify which polygons contain at least one occurrence
polygons_with_points <- lengths(sf::st_intersects(wwf_ecoregions, eu_occ)) > 0

# Subset only those polygons
wwf_ecoregions_filtered <- wwf_ecoregions[polygons_with_points, ]

# Clean up ecoregions
rm(wwf_ecoregions)
invisible(gc())



#----------------------------------------------------------------------------------------
#---- biasgrid: keep values inside invaded ecoregions, set outside to 1 (lowest value)---
#----------------------------------------------------------------------------------------

# Step 1: Rasterize WWF polygons to match biasgrid_aligned
inside_mask <- terra::rasterize(vect(wwf_ecoregions_filtered), biasgrid_aligned, field = 1, background = NA)

# Step 2: Apply logic — keep original where inside_mask, else 1
biasgrid_temp <- terra::ifel(!is.na(inside_mask), biasgrid_aligned, 1)

# Step 3: Restore NA values from the original biasgrid
biasgrid_eu <- mask(biasgrid_temp, biasgrid_aligned)




#-----------------------------------------------------------------------
#----------- Generate pseudoabsences weighted by sampling bias ---------
#-----------------------------------------------------------------------

set.seed(728)
global_points <- terra::spatSample(
  biasgrid_eu,
  size = 30000, #three times the number we need
  method = "weights",     # weighted random sampling
  as.points = TRUE,       # return SpatVector of points
  na.rm = TRUE            # ignore NA pixels
)

# Select 10000 pseudoabsences
if(nrow(global_points) > 10000){
  if(pseudoabsence_thinning_method == "random"){
    print("Thinning pseudoabsences randomly")
    set.seed(101) 
    global_points <- global_points[sample(nrow(global_points), 10000, replace=FALSE), ]%>%
      sf::st_as_sf()
    
    coords <- sf::st_coordinates(global_points)
    
    global_points<-global_points%>%
      dplyr::mutate(decimalLongitude = coords[, "X"],
                    decimalLatitude  = coords[, "Y"])%>%
      dplyr::select(decimalLongitude, decimalLatitude, geometry)
    
  }else if (pseudoabsence_thinning_method == "kmeans_clustering"){
    print("Thinning pseudoabsences based on k-means clustering")
    
    #Extract environmental data from pseudoabsences
    pa_habitat_data <- terra::extract(habitat_stack, global_points, ID = FALSE, xy = TRUE)
    
    #Check how many unique rows there are and set centers to lowest of either 10000 or #unique rows
    unique_centers<-nrow(unique(pa_habitat_data))
    center_number<-min(unique_centers, 10000)
    
    # K-means clustering
    set.seed(101)
    clust <- kmeans(pa_habitat_data[, !names(pa_habitat_data) %in% c("x", "y")], centers = center_number,iter.max = 10, nstart = 1)$cluster
    pa_habitat <- cbind(pa_habitat_data, clust)%>%
      mutate(rID =row_number())
    
    # Keep 1 pseudoabsence per cluster
    sampled <- pa_habitat %>%
      dplyr::group_by(clust) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::ungroup()
    
    # How many pseudoabsences do we still need
    remaining <- 10000 - nrow(sampled)
    
    # Randomly sample extra pseudoabsences if fewer than 10000
    if (remaining > 0) {
      extra_pa <- pa_habitat %>%
        dplyr::filter(!rID %in% sampled$rID)%>%
        dplyr::slice_sample(n = remaining) 
      
      global_points <- bind_rows(sampled, extra_pa)
      rm(extra_pa)
      
    } else {
      global_points<- sampled
    }
    
    # Keep only three columns
    global_points <- global_points %>%
      dplyr::rename("decimalLongitude" = x,
                    "decimalLatitude" = y)%>%
      dplyr::select(decimalLongitude, decimalLatitude)%>%
      sf::st_as_sf(coords=c("decimalLongitude", "decimalLatitude"), crs=crs(biasgrid_eu), remove=FALSE)
    
    rm(pa_habitat_data, pa_habitat, sampled, remaining, unique_centers, center_number, clust)
    
  }
}


#--------------------------------------------------
#-------- Create presence-pseudoabsence dataset ---
#--------------------------------------------------

# Format presence data (eu_occ)
eu_occ <- eu_occ %>%
  dplyr::mutate(species = "present") %>%
  dplyr::relocate(decimalLongitude, decimalLatitude, species, geometry)

#Format pseudoabsence data (global_points) 
global_points_sf <- global_points %>% #Keep only geometry
  dplyr::mutate(species = "absent") %>%
  dplyr::relocate(decimalLongitude, decimalLatitude, species, geometry) #Reorder columns

#Combine presence and pseudoabsence data
eu_presabs <- rbind(eu_occ, global_points_sf)


#---------------------------------------------------------------------
#-------- Remove highly correlated predictors from training data ----
#--------------------------------------------------------------------

# the problem here is that we extract habitat percentages at presence points:
# For some predictors (e.g. broadleaf evergreen forest) all extracted values are: 0 0 0 0 0 0 ...
# because that habitat does not exist in mainland Europe
# So the variable has variance = 0
# The Pearson correlation formula divides by the standard deviation, and when sd(x) = 0
# correlation becomes  0 / 0  = NA
# So cor() returns NA rows/columns
# so I should remove constant predictors

# Extract raster values at eu_presabs points
presabs_df <- terra::extract(habitat_stack, terra::vect(eu_presabs), ID = FALSE)

# remove constant predictors
presabs_df_filtered <- presabs_df[, apply(presabs_df, 2, var) != 0]

# Compute correlation matrix
cor_matrix <- cor(presabs_df_filtered, use = "complete.obs")

# Identify highly correlated variables
drop_vars <- caret::findCorrelation(cor_matrix, cutoff = 0.7, exact = TRUE, names = TRUE)

# Subset fullstack to keep only uncorrelated predictors
fullstack <- subset(habitat_stack, !(names(habitat_stack) %in% drop_vars))


#-------------------------------------------------------------------------
#------- Extract predictor values for presences and pseudoabsences -------
#-------------------------------------------------------------------------

# Convert present absent to 1 0
eu_presabs <- eu_presabs %>%
  dplyr::mutate(species = ifelse(species == "present", 1, 0))

# Extract raster values from fullstack
occ.full.data.df <- terra::extract(fullstack, terra::vect(eu_presabs), ID = FALSE) %>%
  #dplyr::mutate(occ = eu_presabs$species) 
  bind_cols(., as.data.frame(terra::vect(eu_presabs)) %>%
              dplyr::select(species)) 

if (anyNA(occ.full.data.df)) warning("Some pseudoabsence points or occurrences fall within NA grid cells")



# - # - # - # - # - # - # - # - # - # - # - # - # - # 
#                                                   #
#                                                   #
#           MODELING - PREDICTIONS (CURRENT)        #
#                                                   #
#                                                   #
# - # - # - # - # - # - # - # - # - # - # - # - # - # 



#--------------------------------------------
#------- Run models with habitat data -------
#--------------------------------------------

# here I need to to include the raster predictors explicitly
# Get predictor names
pred_names <- names(fullstack)

# Build formula dynamically
f <- as.formula(paste("species ~", paste(pred_names, collapse = " + ")))

# Build SDM data
sdm_data <- sdm::sdmData(
  formula = f,
  train = vect(eu_presabs),
  predictors = fullstack)

methods <- c("glm", "gam", "bioclim", "brt", "rf", "glmpoly", "mars", "maxent", "fda","cart")

# run model
set.seed(2025)
model <- sdm(
  #species ~ ., 
  formula = f,
  data = sdm_data,
  methods = methods  # 10 models
)

print(model)


#--------------------------------------------
#---  Make predictions using each model  ---
#-------------------------------------------- 

# Define prevalence ratio
n1 <- nrow(eu_occ)  # presences
n0 <- nrow(global_points_sf) # pseudoabsences 
prev_ratio <- n1 / n0
nblocks <- 4

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
  e <- terra::ext(fullstack) # Europe
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
        
        block_r <- crop(fullstack, exts[[rasterblock]]) # Europe
        
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
  r0 <- fullstack[[1]]
  nr <- terra::nrow(r0)
  row_breaks <- round(seq(1, nr + 1, length.out = nblocks + 1))
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
        
        block_r <- crop(fullstack, exts[[rasterblock]]) # Europe
        
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



#-----------------------------------------------
#-------- Create ensemble using PCAm method ----
#-----------------------------------------------

# Combine into a SpatRaster stack
fav_stack <- terra::rast(modeloutput)

# Assign layer names based on model methods
names(fav_stack) <- names(modeloutput)

# a PCA needs prediction layers that contain valid, finite, non-constant information
# so I can't do it if I have all 0s or NaN in a layer

fav_stack_copy <- fav_stack

set.seed(123)
invisible(gc())

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

# make PCA: sample a representative number of pixels (in total less than 6000000 non NA cells)
set.seed(100)
pca_result <- rasterPCA(fav_stack, nSamples = 100000, spca = FALSE, maskCheck = FALSE)


#-----------------GET TOP 5 variance models----------------

# Step 1: Extract PC1 loadings from princomp object
loadings <- pca_result$model$loadings[, 1]  # Comp.1 = PC1
names(loadings) <- rownames(pca_result$model$loadings)

# Step 2: Convert raster stack to matrix (rows = pixels, cols = models)
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
cat("Top 5 models by variance along PC1:\n", paste(top5_models, collapse = ", "), "\n")

# Get model IDs
top_ids <- info$modelID[info$method %in% top5_models]

# Subset using those IDs
top5models <- model[[top_ids]]  

# Clean up
invisible(gc())

#--------Create ensemble predictions using those 5 models------

# Step 5: Subset fav_stack to top 5 layers
top5_stack <- subset(fav_stack, top5_models)

# Step 6: Compute pixel-wise median = consensus model
consensus_habitat <- app(top5_stack, median)

# Step 7: Compute pixel-wise mean
consensus_habitat_mean <- mean(top5_stack, na.rm=TRUE)

# Step 8: Compute pixel-wise population standard deviation
consensus_habitat_sd <- stdev(top5_stack, pop=TRUE)

# Step 9: Crop to extent of country if relevant
if(tolower(country_of_interest)=="europe"){
  ensemble_habitat_suitability<-consensus_habitat
  ensemble_habitat_sd <- consensus_habitat_sd
  ensemble_habitat_mean<- consensus_habitat_mean 
}else{
  ensemble_habitat_suitability<- consensus_habitat%>%
    terra::crop(country_boundary)%>%
    terra::mask(country_boundary)
  
  ensemble_habitat_sd <- consensus_habitat_sd%>%
    terra::crop(country_boundary)%>%
    terra::mask(country_boundary)
  
  ensemble_habitat_mean <- consensus_habitat_mean%>%
    terra::crop(country_boundary)%>%
    terra::mask(country_boundary)
}



#--------------------------------------------------
#-- Create map with ensemble habitat suitability --
#--------------------------------------------------

# Define name of files
base_file <- paste0(basefile, "current_ensemble")

# Export PDFs with and without occurrences plotted
for (occs in list(NULL, eu_occ)){
  filename <- ifelse(is.null(occs), base_file, paste0(base_file, "_occ"))
  
  exportPDF(predictions = ensemble_habitat_suitability,
            dataType = "Suit",
            period = "Current",
            returnPredictions = FALSE,
            returnPNG = FALSE,
            occ_data=occs,
            exportPNG=TRUE,
            PDF_title = PDF_title,
            PNG_folder=file.path(base_dir, "Habitat", "Current", "Predictions", "PNGs"),
            PDF_folder=file.path(base_dir, "Habitat", "Current", "Predictions","PDFs"),
            filename = filename)
}

# Export ensemble raster (favorability) 
current_habitat_folder <- file.path(base_dir, "Habitat", "Current", "Predictions", "Rasters")
habitat_ensemble_file <- file.path(current_habitat_folder, paste0(base_file,".tif"))
terra::writeRaster(ensemble_habitat_suitability, filename = habitat_ensemble_file, overwrite = TRUE)



#--------------------------------------------------
#---------- Create map with ensemble SD -----------
#--------------------------------------------------

# Define name of files
filename <- paste0(basefile, "current_ensemble_SD")

# Export PDFs with and without occurrences plotted
exportPDF(predictions = ensemble_habitat_sd,
          dataType = "Stdev",
          period = "Current",
          returnPredictions = FALSE,
          returnPNG = FALSE,
          occ_data=NULL,
          exportPNG=TRUE,
          PDF_title = PDF_title,
          PNG_folder=file.path(base_dir, "Habitat", "Current", "Diagnostics","Confidence_maps", "PNGs"),
          PDF_folder=file.path(base_dir, "Habitat", "Current", "Diagnostics","Confidence_maps", "PDFs"),
          filename = filename)

# Export ensemble raster (favorability) 
current_sd_habitat_folder <- file.path(base_dir, "Habitat", "Current", "Diagnostics", "Confidence_maps", "Rasters")
habitat_sd_ensemble_file <- file.path(current_sd_habitat_folder, paste0(filename,".tif"))
terra::writeRaster(ensemble_habitat_sd, filename = habitat_sd_ensemble_file, overwrite = TRUE)


#------------------------------------------
#------------ Create binary map -----------
#-----------------------------------------

# Get predictor values at occurrence points
predictors_only <- occ.full.data.df%>%
  #dplyr::filter(occ=="present")%>%
  dplyr::filter(species=="1")%>%
  #dplyr::select(-occ) 
  dplyr::select(-species)

# Predict for top 5 models
pred_vals <- list()
for (method in top5_models) {
  pred_vals[[method]] <- predict(model, 
                                 newdata = predictors_only, 
                                 method = tolower(method))
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
binary_maps_habitat_current <- list()

raster_folder <- file.path(base_dir, "Habitat","Current", "Predictions", "Rasters")

for (probs in mtp_probabilities){

  # Define mtp_pct and mtp_value
  mtp_value <- probs*100
  mtp_pct <- paste0(mtp_value, "%")
  mtp_text <- paste0(mtp_value,"pct_habitat_threshold")
  
  # Obtain threshold
  to_omit <- floor(probs * nrow(fav_vals)) #Define how many of lowest ranked occs to omit based on mtp threshold
  thr <- sort(fav_vals$median)[to_omit + 1]
  cat(paste0("Mean ",mtp_pct," minimum training presence threshold habitat model: ", round(thr, 4), "\n"))
  
  # Create binary raster using MTP threshold
  binary_map_pct <- ensemble_habitat_suitability >= thr  
  binary_map_pct <- as.factor( binary_map_pct*1) #Convert TRUE/FALSE to 1/0 and then to Present/Absent
  levels( binary_map_pct) <- data.frame(ID = c(0, 1),
                                        class = c("Absent", "Present"))
  
  # Store raster
  binary_file <- file.path (raster_folder, paste0(basefile,"current_binary",mtp_value,"pct.tif"))
  terra::writeRaster(binary_map_pct, filename = binary_file, overwrite = TRUE)

  # export as PDF and PNG with and without occurrences plotted
  base_file<- paste0(basefile, "current_binary",mtp_value,"pct")
  
  for (occs in list(NULL, eu_occ)){
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
              PNG_folder=file.path(base_dir, "Habitat","Current", "Predictions", "PNGs"),
              PDF_folder=file.path(base_dir,"Habitat" ,"Current", "Predictions", "PDFs"),
              filename = filename)
  }
  
  #assign(paste0(mtp_value,"pct_habitat_threshold"), thr)
  
  binary_maps_habitat_current[[mtp_pct]] <- list(binary_raster=binary_map_pct,
                                 mean_MTP= thr)
  
  rm(binary_map_pct, binary_file, thr)
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
    
    print(paste("[FUTURE] Projecting:", period, scenario))
    
    # Get future habitat data for specific period and scenario
    future_rast <- terra::rast(future_paths[[paste0(period, "_", scenario)]])
    # get %
    future_rast <- future_rast/10000
    
    # Keep relevant predictors in the raster stack
    if(length(drop_vars) > 0){
      
      future_selection <- subset(future_rast, !(names(future_rast) %in% drop_vars))
      
    }else{future_selection <- future_rast}
    
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
    future_folder <- file.path(base_dir, "Habitat", period, scenario, "Predictions", "Rasters")
    ensemble_file <- file.path(future_folder, paste0(basefile, period,"_",scenario,"_ensemble.tif"))
    terra::writeRaster(future_consensus_median, filename = ensemble_file, overwrite = TRUE)
    
    # Export future sd raster 
    future_sd_folder <- file.path(base_dir, "Habitat", period, scenario, "Diagnostics", "Confidence_maps", "Rasters")
    ensemble_sd_file <- file.path(future_sd_folder, paste0(basefile, period,"_",scenario,"_ensemble_SD.tif"))
    terra::writeRaster(future_consensus_sd, filename = ensemble_sd_file, overwrite = TRUE)
    
    # Export future mean raster 
    future_mean_folder <- file.path(base_dir, "Habitat", "Current", "Interim")
    ensemble_mean_file <- file.path(future_mean_folder, paste0(basefile, period,"_",scenario,"_ensemble_mean.tif"))
    terra::writeRaster(future_consensus_mean, filename = ensemble_mean_file, overwrite = TRUE)
    
    # Export ensemble predictions as PDF and PNG with and without occurrences
    base_file <- paste0(basefile, scenario,"_", period,"_ensemble")
    
    for (occs in list(NULL, eu_occ)){
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
                PNG_folder=file.path(base_dir, "Habitat", period, scenario, "Predictions", "PNGs"),
                PDF_folder=file.path(base_dir, "Habitat", period, scenario, "Predictions", "PDFs"),
                filename = filename)
    }
    
    # Export ensemble SD predictions as PDF and PNG 
    filename <- paste0(basefile, scenario,"_", period,"_ensemble_SD")
    
    exportPDF(predictions = future_consensus_sd,
              dataType = "Stdev",
              period = period,
              scenario = scenario,
              returnPredictions = FALSE,
              returnPNG = TRUE,
              occ_data=NULL,
              exportPNG=TRUE,
              PDF_title=PDF_title,
              PNG_folder=file.path(base_dir, "Habitat", period, scenario, "Diagnostics", "Confidence_maps", "PNGs"),
              PDF_folder=file.path(base_dir, "Habitat", period, scenario, "Diagnostics", "Confidence_maps", "PDFs"),
              filename = filename)
    
    # Create binarized ensemble predictions for future
    for(probs in mtp_probabilities){
      
      # Define mtp_pct and mtp_value
      mtp_value <- probs*100
      mtp_pct <- paste0(mtp_value, "%")
      mtp_text <- paste0(mtp_value,"pct_habitat_threshold")
      
      # Get threshold value and apply to consensus predictions
      threshold <- binary_maps_habitat_current[[mtp_pct]]$mean_MTP
      binary_map_future <- future_consensus_median  >= threshold
      binary_map_future <- as.factor( binary_map_future*1) #Convert TRUE/FALSE to 1/0 and then to Present/Absent
      levels( binary_map_future) <- data.frame(ID = c(0, 1),
                                               class = c("Absent", "Present"))
      
      #Store raster
      binary_file <- file.path(future_folder,
                               paste0(basefile, period,"_",scenario,"_binary",mtp_value,".tif"))
      terra::writeRaster(binary_map_future, filename = binary_file, overwrite = TRUE)

      # Export binarized ensemble predictions as PDF and PNG with and without occurrences
      base_file <- paste0(basefile, period,"_", scenario, "_binary",mtp_value, "pct")

      for (occs in list(NULL, eu_occ)){

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
                  LabelName=paste0(mtp_pct, " MTP threshold"),
                  PDF_title=PDF_title,
                  PNG_folder=file.path(base_dir,"Habitat", period, scenario, "Predictions", "PNGs"),
                  PDF_folder=file.path(base_dir, "Habitat",period, scenario, "Predictions", "PDFs"),
                  filename=filename)
      }
      rm(mtp_pct, mtp_text, threshold, binary_map_future)
    }
  }
}




#----------------------------------------------------
#------ Get response curves of 5 selected models ----
#----------------------------------------------------

response_list <- list()
varimp_list <- list()

for(topmethod in top5_models){
  
  # Get model id
  id <- info$modelID[info$method == topmethod]
  
  # Get response curve
  response_curves <- sdm::getResponseCurve(model,id)@response
  
  # Get variable importance
  varimp <- tryCatch({
    
    sdm::getVarImp(model,id)@varImportance
    
  }, error = function(e){
    
    message("Error when computing variable importance for ", topmethod, ": ", e$message)
    message("Skipping the algorithm...")
    return(NA)
    
  })
  
  # Store
  response_list[[topmethod]] <- response_curves
  varimp_list[[topmethod]] <- varimp
}

# Remove NAs
varimp_list <- varimp_list[!is.na(varimp_list)]

# Convert list to a dataframe
response_df <- purrr::imap_dfr(response_list, function(model_list, model_name) {
  imap_dfr(model_list, function(df, var_name) {
    response_df <- df %>%
      setNames(c("Predictor_value", "Response"))%>%
      mutate( Algorithm = model_name,
              Predictor = var_name)})}) %>%
  dplyr::select(Algorithm,Predictor, Predictor_value, Response)


varimp_df <- imap_dfr(varimp_list, function(df, model_name) {
  df %>%
    setNames(c("Predictor", "corTest" , "AUCtest"))%>%
    dplyr::mutate(Algorithm = model_name)})%>%
  dplyr::select(Algorithm,Predictor, corTest, AUCtest)


# Plot response curves
response_plot <- ggplot(response_df, aes(x = Predictor_value,
                                         y = Response, 
                                         color = Algorithm)) +
  geom_line(size=0.8) +
  facet_wrap(~ Predictor, scales = "free_x")+
  labs(title= "Habitat response curves" ,x= "Predictor value")+
  theme_bw()

# Plot variable importance 
varimp_plot <- ggplot(varimp_df, aes(x = Predictor, y = corTest)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  #horizontal bars
  facet_wrap(~ Algorithm) +  
  geom_hline(yintercept = 0, color = "black") + 
  labs( x = "Variable",
        y = "Importance",
        title = "Variable importance per model") +
  theme_bw()

#Save plot
PNG_folder <- file.path(base_dir, "Habitat", "Current", "Diagnostics")

ggplot2::ggsave(filename = paste0(basefile, "variable_importance.png"), plot = varimp_plot ,  device = "png", width =8.27 , height = 5.845, path= file.path(PNG_folder, "Variable_importance"))
ggplot2::ggsave(filename = paste0(basefile, "response_curves.png"), plot = response_plot,  device = "png", width =8.27 , height = 5.845, path=  file.path(PNG_folder, "Response_curves"))



# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # 
#                                                               #
#                                                               #
#             ENSEMBLE - CLIMATE AND HABITAT (CURRENT)          #
#                                                               #
#                                                               #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # 



#-------------------------------------------------------------------   
#------ Create final predictions combining habitat and climate -----
#-------------------------------------------------------------------

if( file.exists(file.path(climate_raster_folder, paste0(speciesName,"_Climate_current_ensemble.tif"))) ){
  
  if(tolower(country_of_interest)=="europe"){
    consensus_climate<-terra::rast( file.path(climate_raster_folder,
                                              paste0(speciesName,"_Climate_current_ensemble.tif")))%>%
      terra::project(consensus_habitat)
  }else{
    consensus_climate<-terra::rast(file.path( climate_raster_folder,
                                              paste0(speciesName, "_Climate_current_ensemble_Europe.tif")))%>%
      terra::project(consensus_habitat)
  }
  
}else{warning("No climate ensemble found!")}

# Combine suitability predictions by global model (climate) and EU habitat model
clim_hab <- sqrt(consensus_habitat * consensus_climate)



#-----------------------------------------------------------
#------ Export maps with final suitability predictions -----
#-----------------------------------------------------------

# Crop to extent of country if relevant
if(tolower(country_of_interest)=="europe"){
  ensemble_combined_suitability<-clim_hab
}else{
  ensemble_combined_suitability<- clim_hab%>%
    terra::crop(country_boundary)%>%
    terra::mask(country_boundary)
}

# Define name of files
base_file <- paste0(combined_basefile, "current_ensemble")

# Export raster file
# Export continuous suitability raster
clim_hab_file <- file.path(base_dir, "Combined", "Current", "Predictions", "Rasters",
                           paste0(base_file,".tif"))

terra::writeRaster(ensemble_combined_suitability, filename = clim_hab_file, overwrite = T)

# Export PDFs with and without occurrences plotted
for (occs in list(NULL, eu_occ)){
  filename <- ifelse(is.null(occs), base_file, paste0(base_file, "_occ"))
  
  exportPDF(predictions = ensemble_combined_suitability,
            dataType = "Suit",
            period = "Current",
            returnPredictions = FALSE,
            returnPNG = FALSE,
            occ_data=occs,
            exportPNG=TRUE,
            PDF_title = PDF_title,
            PNG_folder=file.path(base_dir, "Combined", "Current", "Predictions", "PNGs"),
            PDF_folder=file.path(base_dir, "Combined", "Current", "Predictions","PDFs"),
            filename = filename)
}



#--------------------------------------------------
#----- Create maps with final SD predictions ------
#--------------------------------------------------

# Load climate layers
mean_climate_path<- file.path( base_dir,"Climate", "Current", "Interim",
                               paste0(global_basefile, "current_ensemble_mean.tif"))
sd_climate_path <- file.path( base_dir,"Climate", "Current","Diagnostics", "Confidence_maps", "Rasters",
                              paste0(global_basefile, "current_ensemble_SD.tif"))
consensus_climate_mean <- terra::rast(mean_climate_path)
consensus_climate_sd <- terra::rast(sd_climate_path)

# reproject mean climate to mean habitat crs if needed
if( crs(consensus_climate_mean, proj = T) != crs(ensemble_habitat_mean, proj = T) ){
  
  message("Reprojecting mean climate to mean habitat CRS...")
  consensus_climate_mean <- terra::project(consensus_climate_mean,
                                           ensemble_habitat_mean,
                                           method = "bilinear")
  consensus_climate_sd <- terra::project(consensus_climate_sd,
                                         ensemble_habitat_mean,
                                         method = "bilinear")
}

# small floor to avoid division by zero
eps <- 1e-6    

# the rasters may have a slight different extent, so I may need to resample them
if ( ext(consensus_climate_mean) != ext(ensemble_habitat_mean) ){
  
  message("Climate and habitat rasters have different extents! Resampling the climate raster...")
  
  consensus_climate_mean <- terra::resample(consensus_climate_mean, ensemble_habitat_mean, method = "bilinear")
  consensus_climate_sd <- terra::resample(consensus_climate_sd, ensemble_habitat_mean, method = "bilinear")
  
}

# compute geometric mean to get combined suitability
S <- tryCatch({
  
  sqrt(consensus_climate_mean * ensemble_habitat_mean)
  
}, error = function(e){
  
  message("Geometric mean computantion failed with error: ", e$message)
  message("Attempting again by resampling the habitat raster...")
  
  consensus_climate_mean_res <- terra::resample(consensus_climate_mean, ensemble_habitat_mean, method = "bilinear")
  return(sqrt(consensus_climate_mean_res * ensemble_habitat_mean))
  
})

# compute relative SDs 
sd_climate <- consensus_climate_sd / (consensus_climate_mean + eps)
sd_habitat <- ensemble_habitat_sd / (ensemble_habitat_mean + eps)

# combined relative uncertainty 
sd_comb <- sqrt(sd_climate^2 + sd_habitat^2)

# final sd of geometric mean
# it shows where the combined suitability is most uncertain due to disagreement among models
Final_SD <- 0.5 * S * sd_comb

names(Final_SD) <- "sd_geometric_mean"

# Define name of files
filename <- paste0(combined_basefile, "current_ensemble_SD")

# Export raster file
clim_comb_sd_file <- file.path(base_dir, "Combined", "Current", "Diagnostics", "Confidence_maps", "Rasters",
                               paste0(filename,".tif"))

terra::writeRaster(Final_SD, filename = clim_comb_sd_file, overwrite = T)

# Export PDFs and PNGs
exportPDF(predictions = Final_SD,
          dataType = "Stdev",
          period = "Current",
          returnPredictions = FALSE,
          returnPNG = FALSE,
          occ_data=NULL,
          exportPNG=TRUE,
          PDF_title = PDF_title,
          PNG_folder=file.path(base_dir, "Combined", "Current", "Diagnostics", "Confidence_maps", "PNGs"),
          PDF_folder=file.path(base_dir, "Combined", "Current", "Diagnostics", "Confidence_maps", "PDFs"),
          filename = filename)



#------------------------------------------
#------------ Create binary map -----------
#------------------------------------------

# Get predicted values at occurrence points
vals_occ <- terra::extract(clim_hab, terra::vect(eu_occ), ID=FALSE)

# Create binary maps
binary_maps_ensemble_climate_habitat_current <- list()

for (probs in mtp_probabilities){

  # Define mtp_pct and mtp_value
  mtp_value <- probs*100
  mtp_pct <- paste0(mtp_value, "%")
  mtp_text <- paste0(mtp_value,"pct_habitat_threshold")
  
  # Obtain threshold
  to_omit <- floor(probs * nrow(vals_occ)) #Define how many of lowest ranked occs to omit based on mtp threshold
  thr <- sort(vals_occ[[1]])[to_omit + 1]
  cat(paste0("Mean ",mtp_pct," minimum training presence threshold combined model: ", round(thr, 4), "\n"))
  
  # Create binary raster using MTP threshold
  binary_map_pct <- ensemble_combined_suitability >= thr 
  binary_map_pct <- as.factor( binary_map_pct*1) #Convert TRUE/FALSE to 1/0 and then to Present/Absent
  levels( binary_map_pct) <- data.frame(ID = c(0, 1),
                                        class = c("Absent", "Present"))
  
  #Store raster
  raster_folder <- file.path(base_dir, "Combined","Current", "Predictions", "Rasters")
  binary_file <- file.path (raster_folder, paste0(combined_basefile,"current_binary",mtp_value,"pct.tif"))
  terra::writeRaster(binary_map_pct, filename = binary_file, overwrite = TRUE)
  
  # export as PDF and PNG with and without occurrences plotted 
  base_file <- paste0(combined_basefile, "current_binary",mtp_value,"pct")
  
  for (occs in list(NULL, eu_occ)){
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
              PNG_folder=file.path(base_dir, "Combined","Current", "Predictions", "PNGs"),
              PDF_folder=file.path(base_dir,"Combined" ,"Current", "Predictions", "PDFs"),
              filename = filename)
  }
  
  #assign(paste0(mtp_value,"pct"), thr)
  
  binary_maps_ensemble_climate_habitat_current[[mtp_pct]] <- list(binary_raster=binary_map_pct,
                                 mean_MTP= thr)
  
  rm(binary_map_pct, binary_file, thr)
}



# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # 
#                                                               #
#                                                               #
#              ENSEMBLE - CLIMATE AND HABITAT (FUTURE)          #
#                                                               #
#                                                               #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # 



#---------------------------------------------------
#------- Create maps with future projections -------
#---------------------------------------------------

for (period in c("2041-2070","2071-2100")){
  for(scenario in c("ssp126", "ssp370", "ssp585")){

    
    #--------------------------------
    #--- Create suitability maps ----
    #--------------------------------
    
    print(paste("[FUTURE] Projecting climate and habitat:", period, scenario))
    
    # Get ensemble climate data for specific period and scenario
    future_folder <- file.path(base_dir, "Climate", period, scenario, "Predictions", "Rasters")
    ensemble_file <- file.path(future_folder, paste0(global_basefile, period,"_",scenario,"_ensemble.tif"))
    future_climate <- terra::rast(ensemble_file) %>%
      terra::project(ensemble_habitat_suitability)
    
    # sanity check
    if(! (grepl("Climate", ensemble_file) && grepl(period, ensemble_file) && grepl(scenario, ensemble_file)) ){
      warning("You're loading the wrong file for the ensembled favorability! Here you need the global climate ensemble for ", period, " ", scenario)
    }
    
    # Get ensemble habitat data for specific period and scenario
    future_habitat_folder <- file.path(base_dir, "Habitat", period, scenario, "Predictions", "Rasters")
    ensemble_habitat_file <- file.path(future_habitat_folder, paste0(basefile, period,"_",scenario,"_ensemble.tif"))
    future_habitat <- terra::rast(ensemble_habitat_file) %>%
      terra::project(ensemble_habitat_suitability)
    
    # sanity check
    if(! (grepl("Habitat", ensemble_habitat_file) && grepl(period, ensemble_habitat_file) && grepl(scenario, ensemble_habitat_file)) ){
      warning("You're loading the wrong file for the ensembled favorability! Here you need the EU habitat ensemble for ", period, " ", scenario)
    }
    
    # Final ensemble predictions between ensemble climate data for specific period and scenario 
    # and habitat data for specific period and scenario 
    final_ensemble <- sqrt(future_habitat * future_climate)
    
    # Export future ensemble raster (favorability) 
    future_folder <- file.path(base_dir, "Combined", period, scenario, "Predictions", "Rasters")
    ensemble_file <- file.path(future_folder, paste0(combined_basefile, period,"_",scenario,"_ensemble.tif"))
    terra::writeRaster(final_ensemble, filename = ensemble_file, overwrite = TRUE)
    
    # Export ensemble predictions as PDF and PNG with and without occurrences
    base_file <- paste0(combined_basefile, scenario,"_", period,"_ensemble")
    
    for (occs in list(NULL, eu_occ)){
      filename <- ifelse(is.null(occs), base_file, paste0(base_file, "_occ"))
      
      exportPDF(predictions = final_ensemble,
                dataType = "Suit",
                period = period,
                scenario = scenario,
                returnPredictions = FALSE,
                returnPNG = TRUE,
                occ_data=occs,
                exportPNG=TRUE,
                PDF_title=PDF_title,
                PNG_folder=file.path(base_dir, "Combined", period, scenario, "Predictions", "PNGs"),
                PDF_folder=file.path(base_dir, "Combined", period, scenario, "Predictions", "PDFs"),
                filename = filename)
    }
    
    
    # Create binarized ensemble predictions for future
    for(probs in mtp_probabilities){
      
      # Define mtp_pct and mtp_value
      mtp_value <- probs*100
      mtp_pct <- paste0(mtp_value, "%")
      mtp_text <- paste0(mtp_value,"pct_habitat_threshold")
      
      # Get threshold value and apply to consensus predictions
      threshold <- binary_maps_ensemble_climate_habitat_current[[mtp_pct]]$mean_MTP
      binary_map_future <- final_ensemble  >= threshold
      binary_map_future <- as.factor( binary_map_future*1) #Convert TRUE/FALSE to 1/0 and then to Present/Absent
      levels( binary_map_future) <- data.frame(ID = c(0, 1),
                                               class = c("Absent", "Present"))
      
      # Store raster
      future_combined_folder <- file.path(base_dir, "Combined", period, scenario, "Predictions", "Rasters")
      binary_file <- file.path(future_combined_folder, paste0(combined_basefile, period,"_",scenario,"_binary",mtp_value,".tif"))
      terra::writeRaster(binary_map_future, filename = binary_file, overwrite = TRUE)
      
      # Export binarized ensemble predictions as PDF and PNG with and without occurrences 
      base_file <- paste0(combined_basefile, period,"_", scenario, "_binary", mtp_value)
      
      for (occs in list(NULL, eu_occ)){
        
        filename <- ifelse(is.null(occs), base_file, paste0(base_file, "_occ"))
        exportPDF(predictions = binary_map_future,
                  dataType = "Binary",
                  period = period,
                  scenario = scenario,
                  occ_data = occs,
                  returnPredictions = FALSE,
                  returnPNG = FALSE,
                  exportPNG = TRUE,
                  LabelValue= round(threshold,2),
                  LabelName=paste0(mtp_pct, " MTP threshold"),
                  PDF_title=PDF_title,
                  PNG_folder=file.path(base_dir, "Combined", period, scenario, "Predictions", "PNGs"),
                  PDF_folder=file.path(base_dir, "Combined", period, scenario, "Predictions", "PDFs"),
                  filename=filename)
      }
    }
    
    #--------------------------------
    #---- Create confidence maps ----
    #--------------------------------
    
    # Define file paths for future climate SD and mean files
    # NB here the rasters for the climate ensembled mean for period and scenario are stored in /Climate/Current/Interim
    future_sd_folder <- file.path(base_dir, "Climate", period, scenario, "Diagnostics", "Confidence_maps", "Rasters")
    sd_future_climate_path <-  file.path(future_sd_folder, paste0(global_basefile, period,"_",scenario,"_ensemble_SD.tif"))
    future_mean_folder <- file.path(base_dir, "Climate", "Current", "Interim")
    mean_future_climate_path <- file.path(future_mean_folder, paste0(global_basefile, period,"_",scenario,"_ensemble_mean.tif"))
    
    # Load future climate SD and mean files
    consensus_future_climate_mean <- terra::rast(mean_future_climate_path)
    consensus_future_climate_sd <- terra::rast(sd_future_climate_path)
    
    # sanity check
    if(! (grepl("Climate", mean_future_climate_path) && grepl(period, mean_future_climate_path) && grepl(scenario, mean_future_climate_path)) ){
      warning("You're loading the wrong file for the confidence maps! Here you need the global climate ensemble for ", period, " ", scenario)
    }
    
    # Define file paths for future habitat SD and mean files
    # NB here the rasters for the habitat ensembled mean for period and scenario are stored in /Habitat/Current/Interim
    future_habitat_sd_folder <- file.path(base_dir, "Habitat", period, scenario, "Diagnostics", "Confidence_maps", "Rasters")
    sd_future_habitat_path <-  file.path(future_habitat_sd_folder, paste0(basefile, period,"_",scenario,"_ensemble_SD.tif"))
    future_habitat_mean_folder <- file.path(base_dir, "Habitat", "Current", "Interim")
    mean_future_habitat_path <- file.path(future_habitat_mean_folder, paste0(basefile, period,"_",scenario,"_ensemble_mean.tif"))
    
    # Load future habitat SD and mean files
    consensus_future_habitat_mean <- terra::rast(mean_future_habitat_path)
    consensus_future_habitat_sd <- terra::rast(sd_future_habitat_path)
    
    # reproject mean future_climate to mean habitat crs
    if( crs(consensus_future_climate_mean, proj = T) != crs(consensus_future_habitat_mean, proj = T) ){
      
      message("Reprojecting future mean climate to mean habitat CRS...")
      
      consensus_future_climate_mean <- terra::project(consensus_future_climate_mean,
                                                      consensus_future_habitat_mean,
                                                      method = "bilinear")
      consensus_future_climate_sd <- terra::project(consensus_future_climate_sd,
                                                    consensus_future_habitat_sd,
                                                    method = "bilinear")
      
    }
    
    # sanity check
    if(! (grepl("Habitat", mean_future_habitat_path) && grepl(period, mean_future_habitat_path) && grepl(scenario, mean_future_habitat_path)) ){
      warning("You're loading the wrong file for the confidence maps! Here you need the EU habitat ensemble for ", period, " ", scenario)
    }
    
    # small floor to avoid division by zero; adjust if needed
    eps <- 1e-6    
    
    # compute geometric mean
    S <- sqrt(consensus_future_climate_mean * consensus_future_habitat_mean)
    
    # compute relative SDs safely
    sd_future_climate <- consensus_future_climate_sd / (consensus_future_climate_mean + eps)
    sd_future_habitat <- consensus_future_habitat_sd / (consensus_future_habitat_mean + eps)
    
    # combined relative uncertainty (root-sum-of-squares)
    sd_comb <- sqrt(sd_future_climate^2 + sd_future_habitat^2)
    
    # final sd of geometric mean
    Final_future_SD <- 0.5 * S * sd_comb
    
    names(Final_future_SD) <- "sd_geometric_mean"
    
    # Define name of files
    filename <- paste0(combined_basefile, period, "_",scenario,"_ensemble_SD")
    
    # Export raster file
    future_sd_file <- file.path(base_dir, "Combined", period, scenario, "Diagnostics", "Confidence_maps", "Rasters",
                                paste0(filename,".tif"))
    
    terra::writeRaster(Final_future_SD, filename = future_sd_file, overwrite = T)
    
    #Export PDFs and PNGs
    exportPDF(predictions = Final_future_SD,
              dataType = "Stdev",
              period = period,
              scenario = scenario,
              returnPredictions = FALSE,
              returnPNG = FALSE,
              occ_data=NULL,
              exportPNG=TRUE,
              PDF_title = PDF_title,
              PNG_folder=file.path(base_dir, "Combined", period, scenario, "Diagnostics", "Confidence_maps", "PNGs"),
              PDF_folder=file.path(base_dir, "Combined", period, scenario, "Diagnostics", "Confidence_maps", "PDFs"),
              filename = filename)
    
    rm(S, consensus_future_climate_mean, consensus_future_climate_sd, consensus_future_habitat_mean,
       consensus_future_habitat_sd, sd_future_climate, sd_future_habitat, sd_comb, Final_future_SD, future_sd_file)
  }
}


#------------------------------------------------------------------
#--------- Save best model, european occurrences, and layers ------
#------------------------------------------------------------------

habitatmodel <- list(species = species,
                     taxonkey = taxonkey,
                     eu_occ = eu_occ,  # sf of filtered EU occurrences
                     eu_presabs = eu_presabs,    # sf of presence + pseudoabsence data
                     occ_full_df = occ.full.data.df, # presabs data and their habitat values
                     prevalence_ratio = prev_ratio, # used for favourability scaling
                     habitat_5pct_threshold = binary_maps_habitat_current[["5%"]]$mean_MTP,# 5% mtp threshold habitat model
                     habitat_1pct_threshold = binary_maps_habitat_current[["1%"]]$mean_MTP,# 1% mtp threshold habitat model
                     climhab_5pct_threshold = binary_maps_ensemble_climate_habitat_current[["5%"]]$mean_MTP, # 5% min training presence threshold ensemble model
                     climhab_1pct_threshold = binary_maps_ensemble_climate_habitat_current[["1%"]]$mean_MTP, # 1% min training presence threshold ensemble model
                     response_df = response_df,
                     varimp_df = varimp_df,
                     selected_predictors = names(fullstack),
                     top5models = top5models, #model object holding selected models
                     top5_models = top5_models
)

# Save eumodel as .qs file
qs2::qs_save(habitatmodel, 
          file.path(base_dir,"Habitat", paste0("Habitat_model_",speciesName,"_",taxonkey,".qs"))) 



#--------------------------------------------
#-------- End of loop -----------------------
#--------------------------------------------
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units="hours")
cat("Success! Habitat and ensemble models with pseudo-presences have been created for", sp, "in", round(elapsed, 2), "hours\n\n")
rm(list = ls())

# Clean terra tempfiles
terra::tmpFiles(remove = TRUE)

