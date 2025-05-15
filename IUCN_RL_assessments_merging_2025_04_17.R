###################################################################
#################### IUCN RL ASSESSMENTS MERGING ##################
###################################################################

# Started on 17.04.2025
# by LT

# this is a script to merge the IUCN RL assessments downloaded with the new API
# for IUCN RL IAS-threatened species occurring also in terrestrial habitats

# Setting up the R environment
rm(list=ls())
setwd("D:/Work/oneSTOP")
#setwd("/home/biodiversity/tedeschil/R/range_fit")
getwd()

# Load required packages
pacman::p_load(viridis, rredlist, tidyverse)
sessionInfo()

# Package versions used in this script: 
# viridis_0.6.5
# rredlist_1.0.0    
# tidyverse_2.0.0  



# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # 
#                                                           #
#                                                           #
#          1. COLLATE RED LIST ASSESSMENTS INFORMATION      #
#                                                           #
#                                                           #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # 

# Define categories and base path 
categories <- c("CR", "EN", "VU")
base_path <- "./outputs/IUCN_RL/rredlist_assessments"

# Function to read one type of file for one category
read_species_files <- function(cat, file_type){
  
  # list species in the folder for category "cat"
  sp_list <- list.files(file.path(base_path, cat))
  
  # map applies a function to each element of a list or vector, returning an object of the same length
  sp_data <- map(sp_list, function(sp){
    
    # here, "file_type" will be assigned later as one of the "general_info", "habitats", etc
    file_path <- file.path(base_path, cat, sp, paste0(sp, "_", file_type, ".csv"))
    
    if( file.exists(file_path) ){
      
      # read the file "file_type" for species "sp" in category "cat"
      df <- read.csv(file_path, sep = ",", header = TRUE)
      
      # change code column to character to allow merging
      if( "habitats_code" %in% colnames(df) ){
        
        df$habitats_code <- as.character(df$habitats_code)
        
      }
      
      if( "threats_code" %in% colnames(df) ){
        
        df$threats_code <- as.character(df$threats_code)
        
      }
      
      return(df)
      
    }else{
      
      warning(paste("File not found:", file_path))
      return(NULL)
      
    }
  })
  
  # Combine all into one data frame
  bind_rows(sp_data)
  
}

# Loop through categories and combine everything for each file type
# map_dfr returns dataframeS
# here I am saying: apply the function read_species_files that take as inputs "cat" and "file_type"
# to each element of "categories" (.x), for "file_type" (defined below)
file_type <- "general_info"
sp_all <- map_dfr(categories, ~ read_species_files(.x, file_type))
write.csv(sp_all, file.path(base_path, "sp_general_info.csv"), row.names = FALSE)

file_type <- "habitats"
sp_all <- map_dfr(categories, ~ read_species_files(.x, file_type))
write.csv(sp_all, file.path(base_path, "sp_habitats.csv"), row.names = FALSE)

file_type <- "locations"
sp_all <- map_dfr(categories, ~ read_species_files(.x, file_type))
write.csv(sp_all, file.path(base_path, "sp_locations.csv"), row.names = FALSE)

file_type <- "realms"
sp_all <- map_dfr(categories, ~ read_species_files(.x, file_type))
write.csv(sp_all, file.path(base_path, "sp_realms.csv"), row.names = FALSE)

file_type <- "threats"
sp_all <- map_dfr(categories, ~ read_species_files(.x, file_type))
write.csv(sp_all, file.path(base_path, "sp_threats.csv"), row.names = FALSE)









