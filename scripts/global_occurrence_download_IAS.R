##################################################################
#################### GBIF OCCURRENCES DOWNLOAD ###################
##################################################################

# Started on 25.11.2025
# modified on 13.07.2026
# finalized on 05.11.2026
# by LT

# this is a script to download GBIF occurrences for IAS
# based on oneSTOP Task5.1 scripts

# Setting up the R environment
rm(list=ls())
setwd("/home/biodiversity/tedeschil/Work/oneSTOP") # adjust as needed
getwd()

# Load required packages
pacman::p_load(rgbif, dplyr, purrr, assertthat, readr, qs,
               gh, jsonlite, readxl, tidyverse, taxify)

sessionInfo()

# R and Package versions used in this script: 
# R version 4.4.2 

# Define paths
source_path <- paste0(getwd(), "/scripts")
database_path <- normalizePath("../databases", winslash = "/")
output_path <- paste0(getwd(), "/outputs")

# Load functions
source(paste0(source_path, "/task5.1/aux_funs.R"))
source(paste0(source_path, "/task5.1/helper_functions.R"))
source(paste0(source_path, "/wiSDM_v02/helper_functions.R"))

# Specify project name and version
project_version <- "v02"
projectname <- "onestop_IAS_v02"
project_projection <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # either "EPSG:4326" or "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

gbif_raw_folder <- paste0("./data/original/GBIF/IAS/", project_version)
gbif_proc_folder <- paste0("./data/processed/GBIF/IAS/", project_version)



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#             1. LOAD DATA          #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 

## 1.1 Load list of IAS 
# this comes from the "named" IAS threatening the IUCN RL species
# plus the IAS identified with Natalia Joelson from JRC

ias_list <- read.csv("./outputs/IAS_NJ/IAS_from_IUCN_and_GIDIAS_final.csv")
head(ias_list)
nrow(ias_list) # 1118

ias_names <- sort(unique(ias_list %>%
  # keep only those that we are going to model
  dplyr::filter(remove_from_task5.4 == "no") %>%
  # get species names
  dplyr::pull(threat_ias)))

ias_names




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
                     list("path" = paste0(getwd(), "/data/original/GBIF/IAS"),
                          "name"= project_version))  # create a folder called "v02" within "paste0(getwd(), "/data/original/GBIF/IAS")"

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
taxon_df <- as.data.frame(ias_names)
colnames(taxon_df) <- "species"

mapped_taxa <- purrr::map_dfr(
  taxon_df$species,
  ~ {
    tryCatch(
      {
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

mapped_taxa
unique(mapped_taxa$rank)

# Keep only accepted species
# Make sure that only species info is stored as it is possible that genus information is captured 
# when the species part of the name is not clear

mapped_taxa <- mapped_taxa %>%
  dplyr::filter(rank =="SPECIES" | rank == "SUBSPECIES") 



## 2.2 Check GBIF backbone
# Make sure that all species were mapped to the GBIF backbone, 
# if not an error will appear indicating which species are missing

not_found_ias_keys <- c()

assertthat::assert_that(
  nrow(mapped_taxa) == length(taxon_df$species),
  msg = paste0("The following species could not be found in the GBIF backbone taxonomy: ",
               paste(taxon_df$species[!sapply(taxon_df$species, function(x) any(grepl(x, mapped_taxa$scientificName)))], collapse = ", "))
)

# Error: The following species could not be found in the GBIF backbone taxonomy: 

# Campylopus interflexus: is Campylopus introflexus in GBIF
taxify("Campylopus interflexus", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Campylopus introflexus") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_found_ias_keys <- c(key, not_found_ias_keys)

# Leptotes pirithous ssp. pirithous: is Leptotes pirithous in GBIF
taxify("Leptotes pirithous", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Leptotes pirithous") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_found_ias_keys <- c(key, not_found_ias_keys)

# Neogale vison: is Neovison vison in GBIF
taxify("Neogale vison", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Neovison vison") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_found_ias_keys <- c(key, not_found_ias_keys)

# Rhodeus amareus: is Rhodeus amarus in GBIF 
taxify("Rhodeus amareus", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Rhodeus amarus") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_found_ias_keys <- c(key, not_found_ias_keys)
not_found_ias_keys
length(not_found_ias_keys) # 4 sp



## 2.3 Check for not accepted keys

not_accepted_ias_keys <- c()

not_accepted <- mapped_taxa %>%
  dplyr::filter(status !="ACCEPTED")

if (nrow(not_accepted)!= 0) {
  warning(paste0("The following species do not have an accepted taxonomic status in the GBIF backbone: ",paste(unique(not_accepted$scientificName), collapse=", "),". Their corresponding accepted species names will be used for downloading occurrence data.")
  )
} else {
  paste0("All species are accepted taxa in the GBIF backbone 🎉")
}

# The following species do not have an accepted taxonomic status in the GBIF backbone: 
# Alexandrinus krameri (Scopoli, 1769), 
taxify("Alexandrinus krameri", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Psittacula krameri") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_accepted_ias_keys <- c(key, not_accepted_ias_keys)
  
# Crassostrea gigas (Thunberg, 1793), 
taxify("Crassostrea gigas", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Crassostrea gigas") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_accepted_ias_keys <- c(key, not_accepted_ias_keys)

# Pennisetum setaceum (Forssk.) Chiov., 
taxify("Pennisetum setaceum", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Pennisetum setaceum") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_accepted_ias_keys <- c(key, not_accepted_ias_keys)

# Rana perezi López-Seoane, 1885, 
taxify("Rana perezi", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Rana perezi") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_accepted_ias_keys <- c(key, not_accepted_ias_keys)

# Robinia pseudacacia L., 
taxify("Robinia pseudacacia", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Robinia pseudacacia") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_accepted_ias_keys <- c(key, not_accepted_ias_keys)

# Salariopsis fluviatilis (Asso, 1801), 
taxify("Salariopsis fluviatilis", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Salariopsis fluviatilis") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_accepted_ias_keys <- c(key, not_accepted_ias_keys)

# Salmo gairdnerii Richardson, 1836. 
taxify("Salmo gairdnerii", backend = c("wfo", "col", "gbif"))
# get GBIF backbone
bb <- name_backbone("Salmo gairdnerii") 
# get key 
key <- if ("acceptedUsageKey" %in% names(bb)) {
  bb %>% dplyr::pull(acceptedUsageKey)
} else {
  bb %>% dplyr::pull(usageKey)
}
not_accepted_ias_keys <- c(key, not_accepted_ias_keys)

not_accepted_ias_keys
length(not_accepted_ias_keys) # 7 sp



## 2.4 Extract taxonkeys 
# Extract taxonkeys of each species, for synonyms the acceptedUsageKey is stored
accepted_taxonkeys <- mapped_taxa %>%
  dplyr::filter(status == "ACCEPTED")%>%
  dplyr::pull(usageKey)

length(accepted_taxonkeys) # 88

if(nrow(not_accepted != 0)){
  synonym_taxonkeys <- mapped_taxa %>%
    dplyr::filter(status != "ACCEPTED")%>%
    dplyr::pull(acceptedUsageKey)
  
  accepted_taxonkeys <- c(accepted_taxonkeys, synonym_taxonkeys)

  }

# Keep unique accepted taxonkeys
accepted_taxonkeys <- unique(accepted_taxonkeys)

length(accepted_taxonkeys) # 94
not_found_ias_keys %in% accepted_taxonkeys # 2 not present 
accepted_taxonkeys <- c(not_found_ias_keys[!not_found_ias_keys %in% accepted_taxonkeys], accepted_taxonkeys)
length(accepted_taxonkeys) # 96

not_accepted_ias_keys %in% accepted_taxonkeys # all present 

# save
write.csv(mapped_taxa,
          paste0(output_path, "/", projectname, "/mapped_IAS_taxa.csv"),
          row.names = F)



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#         3. GBIF DOWNLOAD          #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 



## 3.1 Define download settings
# All basis of record types, except `FOSSIL SPECIMEN` and `LIVING SPECIMEN`, 
#which can have misleading location information (e.g. location of captive animal).

basis_of_record <- c(
  "OBSERVATION", 
  "HUMAN_OBSERVATION",
  "MATERIAL_SAMPLE",
  "PRESERVED_SPECIMEN", 
  "UNKNOWN", 
  "MACHINE_OBSERVATION",
  "OCCURRENCE")

# Time period
year_begin <- 1971
year_end <- 2025

# Only georeferenced points
hasCoordinate <- TRUE



## 3.2 Split the species in different chunks
# N. vison should be alone since it has so many occurrences
# this is its key: 5218823

############ actually the problem is Branta canadensis!

# chunks of ca. 20 species
accepted_taxonkeys_chunks <- split(accepted_taxonkeys, ceiling(seq_along(accepted_taxonkeys)/20))

# Convert the first one (where N. vison is) to tibble
accepted_taxonkeys_chunks[[1]] <- tibble(value = accepted_taxonkeys_chunks[[1]])
# Remove the key
accepted_taxonkeys_chunks[[1]] <- accepted_taxonkeys_chunks[[1]] %>%
  dplyr::filter(!value %in% c(5218823))
# Extract the updated vector
accepted_taxonkeys_chunks[[1]] <- pull(accepted_taxonkeys_chunks[[1]], value)
# Print the updated vector
print(accepted_taxonkeys_chunks[[1]])

# add back Neovison vison 
accepted_taxonkeys_chunks[["6"]] <- 5218823L
accepted_taxonkeys_chunks



## 3.2 Download

user <- "lisatedeschi"
email <- "lisatedeschi2@gmail.com"
pwd <- "Gbilisa17?"

for(i in 1:length(accepted_taxonkeys_chunks)){
  
  
 # Note that GBIF credentials are required
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
  email = email)

 # once the GBIF query runs, check the status of the query (running/succeded) 
 gbif_download_metadata <- occ_download_wait(gbif_download_key) 

 # returns info on the download and also the DOI and how to cite it
 message(paste("============ GBIF download info for set n.", i))
 # gbif_download_key



 ## 3.3 Retrieve download
 # After it finishes, download your occurrences from GBIF into your PC (with occ_download_get)
 message(paste("============ GBIF import into R for set n.", i))
 gbif_occurrences <- rgbif::occ_download_get(gbif_download_key, 
                        path = paste0("./data/original/GBIF/IAS/", project_version), 
                        overwrite = TRUE)

 metadata <- rgbif::occ_download_meta(key = gbif_download_key)
 gbif_download_key <- metadata$key



 ## 3.4 Extract_GBIF_occurrence
 raw.path <- paste0("./data/original/GBIF/IAS/", project_version, "/", gbif_download_key)
 unzip(paste0(raw.path, ".zip"), exdir = raw.path, overwrite = TRUE)
  
 # load into R
 global <- as.data.frame(data.table::fread(paste0(raw.path, "/occurrence.txt"), header = TRUE))
 object.size(global) # 46 GB
 global <- dplyr::select(global, 
                        c(speciesKey,acceptedTaxonKey, species, acceptedScientificName, year, decimalLatitude, decimalLongitude, kingdom, phylum, class, genus, coordinateUncertaintyInMeters, identificationVerificationStatus))
 nrow(global)
 object.size(global) # 2.3 GB

 gc()



 ## 3.5 Process ambiguous synonyms 

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



 ## 3.5 Save data

 # Save occurrence data as .qs file
 qs::qsave(global,
            paste0(gbif_proc_folder, "/IAS_occurrences_", i, ".qs"))

 # Save taxa info as .csv
 taxa_info <- unique(global[, c("speciesKey", "species")])
 taxa_info$gbif_download_key <- gbif_download_key
 taxa_info$year_begin <- metadata[["request"]][["predicate"]][["predicates"]][[3]][["value"]]
 taxa_info$year_end <- metadata[["request"]][["predicate"]][["predicates"]][[4]][["value"]]
 taxa_info$gbif_download_created <- format(
    strptime(metadata$created, "%Y-%m-%dT%H:%M:%S"),
    "%Y-%m-%d %H:%M:%S")
 taxa_info$gbif_set <- i
 taxa_info$gbif_doi <- gbif_download_metadata$doi
 taxa_info$project <- projectname
 row.names(taxa_info) <- NULL
 taxa_info

 write.csv2(taxa_info,
             paste0(gbif_proc_folder, "/IAS_taxa_info_", i, ".csv"),
             row.names = FALSE)
 
 # # Remove the zipped folder
 # suppressWarnings(file.remove(paste0(raw.path, ".zip"), full.names = TRUE))
 # 
 # # Remove the unzipped folder 
 # unlink(raw.path, recursive = TRUE)
 
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
gbif_raw_folder <- paste0("./data/original/GBIF/IAS/", project_version)
gbif_proc_folder <- paste0("./data/processed/GBIF/IAS/", project_version)

x <- list.files(gbif_proc_folder)[grepl(".qs", list.files(gbif_proc_folder))]

# get their numbers
x <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x))

global <- c()
taxa_info <- c()
accepted_taxonkeys <- c()

for(i in x){
  
  global_i <- qs::qread(paste0(gbif_proc_folder, "/IAS_occurrences_", i, ".qs"))
  taxa_info_i <- read.csv2(paste0(gbif_proc_folder, "/IAS_taxa_info_", i, ".csv"))
  accepted_taxonkeys_i <- taxa_info_i %>%
    dplyr::pull(speciesKey) %>%
    unique()
  
  global <- bind_rows(global, global_i)
  taxa_info <- bind_rows(taxa_info, taxa_info_i)
  accepted_taxonkeys <- c(accepted_taxonkeys, accepted_taxonkeys_i)
  
}

rm(global_i, taxa_info_i)

if( (length(unique(global$species)) != length(unique(taxa_info$species)) ) |
    (length(unique(global$species)) != length(accepted_taxonkeys) )){
  warning("Number of species do not match!")
}

# Save global raw occurrence data as .qs file  
qs::qsave(global, 
          paste0(gbif_proc_folder, "/IAS_occurrences_raw.qs"))




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
nrow(global) # 36,567,906
length(unique(global$species)) # 92 sp

# Enter value for max coordinate uncertainty in meters, default = 1000
global.occ <- global %>%
  dplyr::filter((speciesKey %in% accepted_taxonkeys) | (acceptedTaxonKey %in% accepted_taxonkeys)) %>%
  dplyr::filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%             # or 5k?
  dplyr::filter(!str_to_lower(identificationVerificationStatus) %in% identificationVerificationStatus_to_discard)

nrow(global.occ) # 30,138,830
length(unique(global.occ$species)) # 91 sp

# Remove coordinates that for both lon and lat values, have less than 4 decimal places
global.occ$lon_dplaces <- sapply(global.occ$decimalLongitude, function(x) decimalplaces(x))
global.occ$lat_dplaces <- sapply(global.occ$decimalLatitude, function(x) decimalplaces(x))
rows_to_na <- which(global.occ$lon_dplaces < 4 & global.occ$lat_dplaces < 4)
global.occ[rows_to_na, ] <- NA
global.occ <- global.occ[ which(!is.na(global.occ$lon_dplaces)),]
global.occ <- within(global.occ, rm("lon_dplaces","lat_dplaces"))

nrow(global.occ) # 29,397,238
length(unique(global.occ$species)) # 91 sp



## 4.3 Rename sp groups

# this is needed to get the correct biasgrid
# available biasgrids are: Molluscs, Mammals, Insects, Birds, Amphibians, Reptiles, Plants.

colnames(global.occ)

sort(unique(global.occ$kingdom))
sort(unique(global.occ$phylum))
sort(unique(global.occ$class))

# "Amphibia"        
# "Aves"            
# "Bivalvia"        
# "Bryopsida"       
# "Clitellata"     
# "Gastropoda"      
# "Insecta"         
# "Leotiomycetes"   
# "Liliopsida"      
# "Magnoliopsida"   
# "Mammalia"       
# "Pinopsida"       
# "Sordariomycetes" 
# "Squamata"  

nrow(global.occ %>%
  dplyr::filter(class == ""))

unique(global.occ %>%
  dplyr::filter(class == "") %>%
  dplyr::pull(genus))

# "Rhodeus": Actinopterygii      
# "Salvelinus": Actinopterygii   
# "Oncorhynchus" : Actinopterygii 
# "Salaria" : Actinopterygii 

global.occ[global.occ$genus %in% c("Rhodeus","Salvelinus","Oncorhynchus","Salaria"),]$class <- "Actinopterygii"

# remove fishes
global.occ <- global.occ %>%
  dplyr::filter(!class == "Actinopterygii")

nrow(global.occ) # 29,090,862
length(unique(global.occ$species)) # 87 sp

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
  dplyr::select(decimalLongitude, decimalLatitude, species, acceptedTaxonKey, 
                speciesKey, Group, coordinateUncertaintyInMeters) 

message("Global number of raw occurrences: ", nrow(global.occ.LL))

# also keep occurrences after 1981 (CHELSA year)
global.occ.LL.1981 <- global.occ %>%
  filter(year > 1980) %>%
  dplyr::select(decimalLongitude, decimalLatitude, species, acceptedTaxonKey, 
                speciesKey, Group, coordinateUncertaintyInMeters) 

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

message("Global number of cleaned occurrences: ", nrow(cleaned)) # 29,013,415
length(unique(cleaned$species)) # 87 sp

# Save global cleaned occurrence data as .qs file
qs::qsave(cleaned,
          paste0(gbif_proc_folder, "/IAS_occurrences_cleaned.qs"))

# do the same for those after 1981
cleaned.1981 <- global.occ.LL.1981 %>%
  CoordinateCleaner::cc_cen(buffer = 100) %>% # remove points within a buffer of 100m around country centroids, default 1km
  CoordinateCleaner::cc_cap(buffer = 100) %>% # remove capitals centroids (buffer 100m), default 10km
  CoordinateCleaner::cc_inst(buffer = 100) %>% # remove zoo and herbaria records buffer of 100 m around biodiversity institutes, default 100m
  CoordinateCleaner::cc_gbif(buffer = 100) %>% # remove around GBIF headquarters in Copenhagen (buffer 100m), default 100m
  CoordinateCleaner::cc_zero() # Remove around the 0/0 point (buffer 0.5 degrees)

message("Global number of cleaned occurrences: ", nrow(cleaned.1981)) # 28,928,910
length(unique(cleaned.1981$species)) # 87 sp 

# Save global cleaned occurrence data as .qs file
qs::qsave(cleaned.1981,
          paste0(gbif_proc_folder, "/IAS_occurrences_cleaned.1981.qs"))



# - # - # - # - # - # - # - # - # - # - # - # - # 
#                                               #
#                                               #
#           5. REPROJECT OCCURRENCES            #
#                                               #
#                                               #
# - # - # - # - # - # - # - # - # - # - # - # - # 



## 5.1 Reproject all occurrences to project projection

cleaned <- qs::qread(paste0(gbif_proc_folder, "/IAS_occurrences_cleaned.qs"))
head(cleaned)
nrow(cleaned) 

# create a vector
cleaned_vect <- terra::vect(cleaned, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
nrow(cleaned_vect) == nrow(cleaned)

# reproject
cleaned_vect <- terra::project(cleaned_vect, project_projection)
nrow(cleaned_vect) == nrow(cleaned)

# make valid
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
          paste0(gbif_proc_folder, "/IAS_occurrences_cleaned.cea.wgs84.qs"))



## 5.2 Reproject all occurrences > 1981 to project projection

cleaned.1981 <- qs::qread(paste0(gbif_proc_folder, "/IAS_occurrences_cleaned.1981.qs"))
head(cleaned.1981)
nrow(cleaned.1981) # 28,928,910

# create a vector
cleaned_vect <- terra::vect(cleaned.1981, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
nrow(cleaned_vect) == nrow(cleaned.1981)

# reproject
cleaned_vect <- terra::project(cleaned_vect, project_projection)
nrow(cleaned_vect) == nrow(cleaned.1981)

# make valid
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
          paste0(gbif_proc_folder, "/IAS_occurrences_cleaned.1981.cea.wgs84.qs"))



# - # - # - # - # - # - # - # - # - # - # - # - # 
#                                               #
#                                               #
#       6. CREATE ONE TAXA INFO FILE           #
#                                               #
#                                               #
# - # - # - # - # - # - # - # - # - # - # - # - # 


## 6.1 Load

x <- list.files(gbif_proc_folder)[grepl("taxa", list.files(gbif_proc_folder))]
taxa_info_all <- data.frame()

for(i in 1:length(x)){
  
  xxx <- read.csv(paste0(gbif_proc_folder, "/IAS_taxa_info_", i, ".csv"), sep=",")
  taxa_info_all <- bind_rows(xxx, taxa_info_all)
  
}

taxa_info_all



## 6.2 Remove fishes

taxa_info_all <- taxa_info_all %>% 
  dplyr::filter(!species %in% c("Oncorhynchus mykiss", "Rhodeus amarus", "Salaria fluviatilis", "Salvelinus fontinalis")) %>% 
  dplyr::arrange(species)



## 6.3 Save

write.csv2(taxa_info_all,
           paste0(gbif_proc_folder, "/IAS_taxa_info.csv"),
           row.names = FALSE)



