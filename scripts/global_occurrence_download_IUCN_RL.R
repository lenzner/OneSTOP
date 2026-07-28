###############################################################################
#################### GBIF OCCURRENCES DOWNLOAD AND CLEANING ###################
###############################################################################

# Started on 18.02.2026
# finalized on 28.07.2026
# by LT

# this is a script to download GBIF occurrences for the IUCN RL IAS-threatened native species 
# using the new wiSDM version
# based on oneSTOP Task5.1 scripts
# and wiSDM_v02

# Setting up the R environment
rm(list=ls())
setwd("/lisc/data/scratch/botany/tedeschi/Work/oneSTOP") # adjust as needed
getwd()

# Load required packages
pacman::p_load(rgbif, dplyr, purrr, assertthat, readr,
               gh, jsonlite, readxl)

# Install correct version of qs
req_qs_version <- "0.27.3"

installed_packages <- installed.packages() |>
  as.data.frame()

if (!"qs" %in% installed_packages$Package){
  warning("qs is not installed => installing")
  remotes::install_version("qs", version = req_qs_version)
}else{
  qs_version <- installed_packages |>
    dplyr::filter(Package == "qs") |>
    dplyr::pull(Version)
  
  if(qs_version != req_qs_version){
    warning(paste("Version", qs_version, "of qs is installed, while", req_qs_version, 
                  "is required => installing correct version"))
    remotes::install_version("qs", version = req_qs_version)
  }else{
    print("Correct version of qs installed")
  }
}

library(qs)
library(qs2)

sessionInfo()

# R and Package versions used in this script: 
# R version 4.4.2 
# readxl_1.4.3     
# jsonlite_1.8.9   
# gh_1.4.1         
# qs_0.27.3        
# readr_2.1.5      
# assertthat_0.2.1 
# purrr_1.0.2     
# dplyr_1.1.4      
# rgbif_3.8.1 

# Define paths
source_path <- paste0(getwd(), "/scripts")
database_path <- normalizePath("../databases", winslash = "/")
output_path <- paste0(getwd(), "/outputs")

# Load functions
source(paste0(source_path, "/task5.1/aux_funs.R"))
source(paste0(source_path, "/task5.1/helper_functions.R"))
source(paste0(source_path, "/wiSDM_v02/helper_functions.R"))

# Specify project name and version
projectname <- "onestop_GBIF_v02"

gbif_raw_folder <- paste0("./data/original/GBIF/IUCN_RL_sp/", projectname)
gbif_proc_folder <- paste0("./data/processed/GBIF/IUCN_RL_sp/", projectname)



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#             1. LOAD DATA          #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 



## 1.1 Load species list
sp_list <- read.csv("./outputs/IUCN_RL/IUCN_check/sp_general_info_clean.csv")
head(sp_list)
length(sp_list$scientific_name) # 592 sp




## 1.2 Create folders

# create custom function
create_folder <- function(path, name) {
  
  full_path <- file.path(path, name)
  
  if (!dir.exists(full_path)) {
    dir.create(full_path, recursive = TRUE)
    message("Created folder: ", full_path)
  } else {
    message("Folder already exists: ", full_path)
  }
}

# Define the folder paths
folder_paths <- list(list("path" = file.path(output_path),
                          "name" = projectname), # create a folder called "projectname" within "output_path"
                     list("path" = paste0(getwd(), "/data/original/GBIF/IUCN_RL_sp"),
                          "name"= projectname),
                     list("path" = paste0(getwd(), "/data/processed/GBIF/IUCN_RL_sp"),
                          "name"= projectname))  # create a folder called "IUCN_test_sp" within "paste0(getwd(), "/data/original/GBIF")"

# Check and create each folder if necessary
lapply(folder_paths, function(folder){
  
  create_folder(folder$path, folder$name)
  
})




# - # - # - # - # - # - # - # - # - # - # - # - # 
#                                               #
#                                               #
#              2. GBIF TAXONOMY                 #
#                                               #
#                                               #
# - # - # - # - # - # - # - # - # - # - # - # - # 



## 2.1 Retrieve GBIF taxonkeys

# Match species names with the GBIF backbone, retrieve taxon keys from GBIF when a match is found
taxon_df <- as.data.frame(sp_list)
colnames(taxon_df)[which(colnames(taxon_df) == "scientific_name")] <- "species"

mapped_taxa <- purrr::map_dfr(
  taxon_df$species,
  ~ {
    tryCatch(
      {
        # Add a small delay to avoid API misses
        Sys.sleep(0.2)
        
        data <- rgbif::name_backbone(name = .x,
                                     curlopts=list(http_version=2))
        if (length(data) == 0) {
          stop("No match with the GBIF backbone found")
        }
        data
      },
      error = function(e) {
        NULL
      }
    )
  }
)

mapped_taxa
unique(mapped_taxa$rank)

# Make sure that only species info is stored as it is possible that genus information is captured 
# when the species part of the name is not clear

mapped_taxa <- mapped_taxa %>%
  dplyr::filter(rank == "SPECIES" | rank == "SUBSPECIES")

# Make sure that all species were mapped to the GBIF backbone, 
# if not an error will appear indicating which species are missing

assertthat::assert_that(
  nrow(mapped_taxa) == length(taxon_df$species),
  msg = paste0("The following species could not be found in the GBIF backbone taxonomy: ",
               paste(taxon_df$species[!sapply(taxon_df$species, function(x) any(grepl(x, mapped_taxa$scientificName)))], collapse = ", "))
)

# Error: The following species could not be found in the GBIF backbone taxonomy: 
# Isoetes tenuissima, 
# Sorbus pseudolatifolia, 
# Pararge xiphia, 
# Sciurus meridionalis

# those species do not exist on GBIF
# e.g.,
name_backbone(name = "Isoetes tenuissima") # only exist as a genus
name_backbone(name = "Sciurus meridionalis") # only exist as a phylum

not_accepted <- mapped_taxa %>%
  dplyr::filter(status !="ACCEPTED")

if (nrow(not_accepted)!= 0) {
  warning(paste0("The following species do not have an accepted taxonomic status in the GBIF backbone: ",paste(unique(not_accepted$scientificName), collapse=", "),". Their corresponding accepted species names will be used for downloading occurrence data.")
  )
} else {
  paste0("All species are accepted taxa in the GBIF backbone 🎉")
}

# Extract taxonkeys ("usageKey") of each ACCEPTED species
accepted_taxonkeys <- mapped_taxa %>%
  dplyr::filter(status == "ACCEPTED")%>%
  dplyr::pull(usageKey)

length(accepted_taxonkeys) # 517 sp have an ACCEPTED status

# for synonyms, the "acceptedUsageKey" is stored

if(nrow(not_accepted != 0)){
  
  synonym_taxonkeys <- mapped_taxa %>%
    dplyr::filter(status != "ACCEPTED")%>%
    dplyr::pull(acceptedUsageKey)
  
  # merge SYNONYMS with accepted ones
  accepted_taxonkeys <- c(accepted_taxonkeys, synonym_taxonkeys)
  
}

length(synonym_taxonkeys) # 71 species have a SYNONYM status

# Keep unique accepted taxonkeys
accepted_taxonkeys <- unique(accepted_taxonkeys)

length(accepted_taxonkeys) # 587
# which species is missing? 

# check if all the taxonkeys stored are either among the usageKey (for accepted species) or acceptedUsageKey (for synonyms)
(accepted_taxonkeys[!accepted_taxonkeys %in% (mapped_taxa$usageKey)]) %in% mapped_taxa$acceptedUsageKey

# save
write.csv(mapped_taxa,
          paste0(gbif_raw_folder, "/mapped_IUCN_RL_sp_taxa.csv"),
          row.names = F)

write.table(accepted_taxonkeys,
            paste0(gbif_raw_folder, "/mapped_IUCN_RL_sp_accepted_taxonkeys.txt"),
            row.names = F,
            col.names = F,
            quote = F,
            sep = ",")



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#         3. GBIF DOWNLOAD          #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 



## 3.1 Define download settings
# All basis of record types, except `FOSSIL SPECIMEN` and `LIVING SPECIMEN`, 
# which can have misleading location information (e.g. location of captive animal).

basis_of_record <- c(
  "OBSERVATION", 
  "HUMAN_OBSERVATION",
  "MATERIAL_SAMPLE",
  "PRESERVED_SPECIMEN", 
  "UNKNOWN", 
  "MACHINE_OBSERVATION",
  "OCCURRENCE")

# Time period
year_begin <- 1971            # should be 1981? --> I can always filter afterwards
year_end <- 2025

# Only georeferenced points
hasCoordinate <- TRUE




## 3.2 Split the species in different chunks

# chunks of ca. 100 species
accepted_taxonkeys_chunks <- split(accepted_taxonkeys, ceiling(seq_along(accepted_taxonkeys)/100))



## 3.3 Download

# adjust as needed

user <- ""
email <- ""
pwd <- ""

for(i in 1:length(accepted_taxonkeys_chunks)){
  
  #--------------------------------------------
  #------------- Download data ----------------
  #--------------------------------------------
  
  # Put the download into the GBIF query
  # !!very important here to use pred_in!!
  
  gbif_download_key <- rgbif::occ_download(
    pred_in("taxonKey", accepted_taxonkeys_chunks[[i]]),
    pred_in("basisOfRecord", basis_of_record),
    pred_gte("year", year_begin),
    pred_lte("year", year_end),
    pred("hasCoordinate", hasCoordinate),
    pred("occurrenceStatus", "PRESENT"),
    pred("hasGeospatialIssue", FALSE), # Remove default geospatial issues
    user = user,
    pwd  = pwd,
    email = email,
    curlopts = list(http_version = 2))
  
  # once the GBIF query runs, check the status of the query (running/succeded) 
  gbif_download_metadata <- occ_download_wait(gbif_download_key) 
  
  # returns info on the download and also the DOI and how to cite it
  message(paste("============ GBIF download info for set n.", i))
  # gbif_download_key 
  
  # After it finishes, download your occurrences from GBIF into your PC (with occ_download_get)
  message(paste("============ GBIF import into R for set n.", i))
  gbif_occurrences <- rgbif::occ_download_get(gbif_download_key, 
                                              path = gbif_raw_folder, 
                                              overwrite = TRUE)
  
  metadata <- rgbif::occ_download_meta(key = gbif_download_key)
  gbif_download_key <- metadata$key
  
  # Extract_GBIF_occurrence
  raw.path <- paste0(gbif_raw_folder, "/", gbif_download_key)
  unzip(paste0(raw.path, ".zip"), exdir = raw.path, overwrite = TRUE)
  
  # load into R
  global <- as.data.frame(data.table::fread(paste0(raw.path, "/occurrence.txt"), header = TRUE))
  
  invisible(gc())
  
  global <- dplyr::select(global,
                          c(speciesKey, acceptedTaxonKey, species, acceptedScientificName, year, decimalLatitude, decimalLongitude, kingdom, phylum, class, genus, coordinateUncertaintyInMeters, identificationVerificationStatus))
  
  
  
  #--------------------------------------------
  #--------- Process ambiguous synonyms -------
  #--------------------------------------------
  
  # Get unique taxonkeys that are not part of the accepted taxonkeys (ambiguous keys)
  ambiguous <- global %>%
    dplyr::filter(!acceptedTaxonKey %in% accepted_taxonkeys) %>%
    dplyr::select(acceptedTaxonKey, acceptedScientificName) %>%
    dplyr::distinct()
  
  if (nrow(ambiguous) > 0) {
    
    # Map these with the GBIF backbone
    mapped_ambiguous<- purrr::map_dfr(
      ambiguous$acceptedScientificName,
      ~ {
        tryCatch(
          {
            # Add a small delay to avoid API misses
            Sys.sleep(0.2)
            
            data <- rgbif::name_backbone(name = .x)
            if (length(data) == 0) {
              stop("No match with the GBIF backbone found")
            }
            data
          },
          error = function(e) {
            NULL
          }
        )
      }
    )
    
    # Keep original acceptedScientificName and the species it was mapped to
    mapped_ambiguous <- mapped_ambiguous %>% 
      dplyr::select(verbatim_name, species)
    
    # Map the species-level against the GBIF backbone
    mapped_ambiguous_species <- purrr::map_dfr(
      mapped_ambiguous$species,
      ~ {
        tryCatch(
          {
            # Add a small delay to avoid API misses
            Sys.sleep(0.2)
            
            data <- rgbif::name_backbone(name = .x)
            if (length(data) == 0) {
              stop("No match with the GBIF backbone found")
            }
            data
          },
          error = function(e) {
            NULL
          }
        )
      }
    )
    
    # Create a df with the following columns:
    # verbatim_name = original acceptedScientificName in df 'global'
    # usageKey = taxonKey of the mapped species
    # scientificName = scientific name of the mapped species
    
    mapped <- mapped_ambiguous %>%
      dplyr::select(species, verbatim_name) %>%
      left_join(mapped_ambiguous_species, 
                by = c("species" = "verbatim_name")) %>%
      dplyr::select(verbatim_name, usageKey, scientificName)
    
    # Add this info to df 'global'
    global <- left_join(global, mapped, 
                         by = c("acceptedScientificName" = "verbatim_name"))
    
    # Overwrite acceptedScientificName and acceptedTaxonKey if necessary
    global <- global %>%
      dplyr::mutate(
        acceptedScientificName = ifelse(!acceptedTaxonKey %in% accepted_taxonkeys,
                                        scientificName, 
                                        acceptedScientificName),
        acceptedTaxonKey = ifelse(!acceptedTaxonKey %in% accepted_taxonkeys, 
                                  usageKey,
                                  acceptedTaxonKey)) %>%
      dplyr::select(-c(usageKey, scientificName)) %>%
      dplyr::filter(acceptedTaxonKey %in% accepted_taxonkeys) 
    
  }
  
  #nrow(global) 
  
  # Save occurrence data as .qs file  
  qs::qsave(global, 
            paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_", i, ".qs"))
  
  # Save taxa info as .csv
  taxa_info <- unique(global[, c("speciesKey", "species")])
  taxa_info$gbif_download_key <- gbif_download_key
  taxa_info$year_begin <- metadata[["request"]][["predicate"]][["predicates"]][[3]][["value"]]
  taxa_info$year_end <- metadata[["request"]][["predicate"]][["predicates"]][[4]][["value"]]
  taxa_info$gbif_download_created <- format(
    strptime(metadata$created, "%Y-%m-%dT%H:%M:%S"),
    "%Y-%m-%d %H:%M:%S")
  taxa_info$gbif_set <- i
  taxa_info$gbif_set_doi <- gbif_download_metadata$doi
  taxa_info$project <- projectname
  row.names(taxa_info) <- NULL
 
  write.csv2(taxa_info, 
             paste0(gbif_proc_folder, "/IUCN_RL_sp_taxa_info_", i, ".csv"), 
             row.names = FALSE)
  
}



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#         4. CLEAN OCCURRENCES      #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 



## 4.1 Load global occurrences downloaded through GBIF and taxa info for each chunk

# list the .qs files
gbif_raw_folder <- paste0("./data/original/GBIF/IUCN_RL_sp/", project_version)
gbif_proc_folder <- paste0("./data/processed/GBIF/IUCN_RL_sp/", project_version)

x <- list.files(gbif_proc_folder)[grepl(".qs", list.files(gbif_proc_folder))]

# get their numbers
x <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x))

global <- c()
taxa_info <- c()
accepted_taxonkeys <- c()

for(i in x){
  
  global_i <- qs::qread(paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_", i, ".qs"))
  taxa_info_i <- read.csv2(paste0(gbif_proc_folder, "/IUCN_RL_sp_taxa_info_", i, ".csv"))
  accepted_taxonkeys_i <- taxa_info_i %>%
    dplyr::pull(speciesKey) %>%
    unique()
  
  global <- bind_rows(global, global_i)
  taxa_info <- bind_rows(taxa_info, taxa_info_i)
  accepted_taxonkeys <- c(accepted_taxonkeys, accepted_taxonkeys_i)
  
}

rm(global_i, taxa_info_i)

if( (length(unique(global$species)) != length(unique(taxa_info$acceptedScientificName)) ) |
    (length(unique(global$species)) != length(accepted_taxonkeys) )){
  warning("Number of species do not match!")
}

# Save global raw occurrence data as .qs file  
qs::qsave(global, 
          paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_raw.qs"))




## 4.2 Remove unverified records
identificationVerificationStatus_to_discard <- c( "unverified",
                                                  "unvalidated",
                                                  "not validated",
                                                  "under validation",
                                                  "not able to validate",
                                                  "control could not be conclusive due to insufficient knowledge",
                                                  "1",
                                                  "uncertain",
                                                  "unconfirmed",
                                                  "douteux",
                                                  "invalide",
                                                  "non r\u00E9alisable",
                                                  "verification needed" ,
                                                  "probable",
                                                  "unconfirmed - not reviewed",
                                                  "validation requested",
                                                  "unconfirmed - plausible")



# Coordinate uncertainty
nrow(global) # 30,221,022 (31,033,748 in v02)
length(unique(global$species)) # 502 sp (501 in v02)

# Enter value for max coordinate uncertainty in meters, default = 1000
global.occ <- global %>%
  dplyr::filter(speciesKey %in% accepted_taxonkeys) %>%
  dplyr::filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%             # or 5k?
  dplyr::filter(!str_to_lower(identificationVerificationStatus) %in% identificationVerificationStatus_to_discard)

nrow(global.occ) # 20,229,363 (20,661,326 in v02)
length(unique(global.occ$species)) # 484 sp (481 in v02)

# Remove coordinates that for both lon and lat values, have less than 4 decimal places
global.occ$lon_dplaces <- sapply(global.occ$decimalLongitude, function(x) decimalplaces(x))
global.occ$lat_dplaces <- sapply(global.occ$decimalLatitude, function(x) decimalplaces(x))
rows_to_na <- which(global.occ$lon_dplaces < 4 & global.occ$lat_dplaces < 4)
global.occ[rows_to_na, ] <- NA
global.occ <- global.occ[ which(!is.na(global.occ$lon_dplaces)),]
global.occ <- within(global.occ, rm("lon_dplaces","lat_dplaces"))

nrow(global.occ) # 18,834,488 (19,312,446 in v02)
length(unique(global.occ$species)) # 458  sp (456 in v02)



## 4.3 Rename sp groups

# this is needed to get the correct biasgrid
# available biasgrids are: Molluscs, Mammals, Insects, Birds, Amphibians, Reptiles, Plants.

colnames(global.occ)

sort(unique(global.occ$kingdom))
sort(unique(global.occ$class))
sort(unique(global.occ$phylum))

global.occ <- global.occ %>%
  dplyr::mutate(Group = case_when(kingdom == "Plantae" ~ "Plants",
                                  phylum == "Mollusca" ~ "Molluscs",
                                  class == "Insecta" ~ "Insects",
                                  class == "Aves" ~ "Birds",
                                  class == "Amphibia" ~ "Amphibians",
                                  class == "Mammalia" ~ "Mammals",
                                  class %in% c("Crocodylia", "Testudines", "Sphenodontia", "Squamata") ~ "Reptiles",
                                  class == "Squamata" ~ "Reptiles",
                                  TRUE ~ NA_character_))

sort(unique(global.occ$Group))



## 4.4 Keep only columns of interest

global.occ.LL <- global.occ %>%
  dplyr::select(decimalLongitude, decimalLatitude, species, acceptedTaxonKey, speciesKey, Group, coordinateUncertaintyInMeters) 

message("Global number of raw occurrences: ", nrow(global.occ.LL))

# also keep occurrences after 1981 (CHELSA year)
global.occ.LL.1981 <- global.occ %>%
  filter(year > 1980) %>%
  dplyr::select(decimalLongitude, decimalLatitude, species, acceptedTaxonKey, speciesKey, Group, coordinateUncertaintyInMeters) 

message("Global number of raw occurrences after 1981: ", nrow(global.occ.LL.1981))

rm(global.occ, global)



## 4.5 Coordinate cleaning
# Clean coordinates based on their proximity to country centroids, capitals, 
# biodiversity institutions, GBIF headquarters, and the 0/0 point

cleaned <- global.occ.LL %>%
  CoordinateCleaner::cc_cen(buffer = 100) %>% # remove points within a buffer of 100m around country centroids, default 1km
  CoordinateCleaner::cc_cap(buffer = 100) %>% # remove capitals centroids (buffer 100m), default 10km
  CoordinateCleaner::cc_inst(buffer = 100) %>% # remove zoo and herbaria records buffer of 100 m around biodiversity institutes, default 100m
  CoordinateCleaner::cc_gbif(buffer = 100) %>% # remove around GBIF headquarters in Copenhagen (buffer 100m), default 100m
  CoordinateCleaner::cc_zero() # Remove around the 0/0 point (buffer 0.5 degrees)

message("Global number of cleaned occurrences: ", nrow(cleaned)) # 19,301,404 (v02)
length(unique(cleaned$species)) # 454 sp (v02)

# Save global cleaned occurrence data as .qs file
qs::qsave(cleaned,
          paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_cleaned.qs"))

# do the same for those after 1981
cleaned.1981 <- global.occ.LL.1981 %>%
  CoordinateCleaner::cc_cen(buffer = 100) %>% # remove points within a buffer of 100m around country centroids, default 1km
  CoordinateCleaner::cc_cap(buffer = 100) %>% # remove capitals centroids (buffer 100m), default 10km
  CoordinateCleaner::cc_inst(buffer = 100) %>% # remove zoo and herbaria records buffer of 100 m around biodiversity institutes, default 100m
  CoordinateCleaner::cc_gbif(buffer = 100) %>% # remove around GBIF headquarters in Copenhagen (buffer 100m), default 100m
  CoordinateCleaner::cc_zero() # Remove around the 0/0 point (buffer 0.5 degrees)

message("Global number of cleaned occurrences: ", nrow(cleaned.1981)) # 19,055,841 (v02)
length(unique(cleaned.1981$species)) # 456 sp (v02)

# Save global cleaned occurrence data as .qs file
qs::qsave(cleaned.1981,
          paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_cleaned.1981.qs"))



# - # - # - # - # - # - # - # - # - # - # - # - # 
#                                               #
#                                               #
#           5. REPROJECT OCCURRENCES            #
#                                               #
#                                               #
# - # - # - # - # - # - # - # - # - # - # - # - # 



## 5.1 Reproject all occurrences to project projection

cleaned <- qs::qread(paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_cleaned.qs"))
head(cleaned)
nrow(cleaned) # 19,301,404

cleaned_vect <- vect(cleaned, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
nrow(cleaned_vect) == nrow(cleaned)
cleaned_vect <- terra::project(cleaned_vect, project_projection)
nrow(cleaned_vect) == nrow(cleaned)
cleaned_vect <- terra::makeValid(cleaned_vect)
nrow(cleaned_vect) == nrow(cleaned)

# extract points coordinate for CEA WGS84
p_coord <- terra::geom(cleaned_vect, df = T)[,c("geom", "x", "y")]

colnames(p_coord) <- c("geom", "decimalLongitude", "decimalLatitude")

# add GBIF info
head(p_coord)

cleaned.cea.wgs84 <- p_coord %>%
  dplyr::select(-geom) %>%
  bind_cols(., cleaned %>%
              dplyr::select(-c("decimalLongitude", "decimalLatitude")))

# Save global cleaned occurrence data as .qs file
qs::qsave(cleaned.cea.wgs84,
          paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_cleaned.cea.wgs84.qs"))



## 5.2 Reproject all occurrences > 1981 to project projection

cleaned.1981 <- qs::qread(paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_cleaned.1981.qs"))
head(cleaned.1981)
nrow(cleaned.1981) # 19,055,841

cleaned_vect <- vect(cleaned.1981, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
nrow(cleaned_vect) == nrow(cleaned.1981)
cleaned_vect <- terra::project(cleaned_vect, project_projection)
nrow(cleaned_vect) == nrow(cleaned.1981)
cleaned_vect <- terra::makeValid(cleaned_vect)
nrow(cleaned_vect) == nrow(cleaned.1981)

# extract points coordinate for CEA WGS84
p_coord <- terra::geom(cleaned_vect, df = T)[,c("geom", "x", "y")]

colnames(p_coord) <- c("geom", "decimalLongitude", "decimalLatitude")

# add GBIF info
head(p_coord)

cleaned.1981.cea.wgs84 <- p_coord %>%
  dplyr::select(-geom) %>%
  bind_cols(., cleaned.1981 %>%
              dplyr::select(-c("decimalLongitude", "decimalLatitude")))

# Save global cleaned occurrence data as .qs file
qs::qsave(cleaned.1981.cea.wgs84,
          paste0(gbif_proc_folder, "/IUCN_RL_sp_occurrences_cleaned.1981.cea.wgs84.qs"))

