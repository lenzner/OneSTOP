#--------------------------------------------
#-------  Define wiSDM configurations -------
#--------------------------------------------

# Specify project name and version for the input data
project_version <- "v02"

# Specify project name and version for the current run
projectname <- "pseudopresences"
project <- "pseudopresences" 

# Define parameters
occurrence_thinning_method <- "random" #"kmeans_clustering" #either "random" or "kmeans_clustering"

pseudoabsence_thinning_method <- "random" # "kmeans_clustering" #either "random" or "kmeans_clustering"

mtp_probabilities <- c(0.01, 0.05) #Define MTP thresholds (0.01 = 1%; 0.05 = 5%,...)

country_of_interest <-"Europe"

update_files <- "ask" #either "ask", "yes", "no"

workflow <-"single_step" #either single_step or two_step

project_projection <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # either "EPSG:4326" or "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

pseudo_absences_background <- "biomes" # either "continents" or "wwf_ecoregions" or "biomes

latitudinal_cutting <- "row_indices" # either "row_indices" or "wiSDM"

boyce_background_size <- 50000 # Number of non NA pixels in Europe to be selected for Boyce index calculation