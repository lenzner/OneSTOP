###################################################################
#################### IUCN RL IAS-THREATENED SPECIES ###############
###################################################################

# Started on 02.04.2025
# by LT

# this is a script to download IUCN RL assessment with the new API
# create ad hoc functions to format the downloaded information
# and extract the IUCN RL species that are threatened by IAS
# all terrestrial species + freshwater/those we are not sure if they’re ours or GuardIAS

# Setting up the R environment
rm(list=ls())
setwd("D:/Work/oneSTOP")
#setwd("/home/biodiversity/tedeschil/R/range_fit")
getwd()

# Load required packages
pacman::p_load(viridis, rredlist, tidyverse)
sessionInfo()

# R and Package versions used in this script: 
# R version 4.4.2 
# viridis_0.6.5
# rredlist_1.0.0    
# tidyverse_2.0.0   



# - # - # - # - # - # - # - # - # - # - # - # - # 
#                                               #
#                                               #
#       1. DOWNLOAD RED LIST ASSESSMENTS        #
#                                               #
#                                               #
# - # - # - # - # - # - # - # - # - # - # - # - #

## NB IUCN suggests a 2-second delay between your calls if you plan to make a lot of calls
# so in a for loop you'll need Sys.sleep(2)

# IUCN RL contains also information for species that are introduced, so I need to remove: 
# presence 5 (extinct), 6 (uncertain) 
# origin 3 (introduced), 4 (vagrant), 5 (uncertain), 6 (assisted colonization)

# IUCN_REDLIST_KEY = "c937c514b0c78581769255e3d7352f1f86ee493dce2e59b948dcb11398930697" # old version API v3
IUCN_REDLIST_KEY = "P4DKTzW7G9LUdae4NPifBzkbcnN51w9YSV17"



## 1.1 Get the assessment IDs for the species in Europe categorized as CR
rl_CR_assessments <- rl_categories(code = "CR",
                                    latest = T,
                                    scope_code = 2,
                                    key = IUCN_REDLIST_KEY)$assessments

length(rl_CR_assessments$assessment_id) 
# 1011 assessments in Europe for species with CR category if i don't keep only the latest ones
# 768 assessments in Europe for species with CR category if i keep only the latest ones



## 1.2 Get the assessment IDs for the species in Europe categorized as EN
rl_EN_assessments <- rl_categories(code = "EN",
                                    latest = T,
                                    scope_code = 2,
                                    key = IUCN_REDLIST_KEY)$assessments

length(rl_EN_assessments$assessment_id) 
# 1800 assessments in Europe for species with EN category if i don't keep only the latest ones
# 1455assessments in Europe for species with EN category if i keep only the latest ones



## 1.3 Get the assessment IDs for the species in Europe categorized as VU
rl_VU_assessments <- rl_categories(code = "VU",
                                    latest = T,
                                    scope_code = 2,
                                    key = IUCN_REDLIST_KEY)$assessments

length(rl_VU_assessments$assessment_id) 
# 2089 assessments in Europe for species with VU category if i don't keep only the latest ones
# 1514 assessments in Europe for species with CR category if i keep only the latest ones



# - # - # - # - # - # - # - # - # - # 
#                                   #
#                                   #
#       2. CREATE FUNCTIONS         #
#                                   #
#                                   #
# - # - # - # - # - # - # - # - # - # 


# Create functions needed for the data extraction and cleaning


## 2.1 General info

assessment_general_info <- function(x) {
  
  # Extract relevant data with safety for missing fields in a tibble
  tibble(
    
    # assessment info
    year_published = x$year_published %||% NA_character_,
    latest = x$latest %||% NA_character_,
    possibly_extinct = x$possibly_extinct %||% NA_character_,
    possibly_extinct_in_the_wild = x$possibly_extinct_in_the_wild %||% NA_character_,
    citation = x$citation %||% NA_character_,
    assessment_id = x$assessment_id %||% NA_character_,
    
    # taxon info
    sis_id = x$taxon$sis_id %||% NA_character_,
    scientific_name = x$taxon$scientific_name %||% NA_character_,
    kingdom_name = x$taxon$kingdom_name %||% NA_character_,
    phylum_name = x$taxon$phylum_name %||% NA_character_,
    class_name = x$taxon$class_name %||% NA_character_,
    order_name = x$taxon$order_name %||% NA_character_,
    family_name = x$taxon$family_name %||% NA_character_,   
    genus_name = x$taxon$genus_name %||% NA_character_,
    species_name = x$taxon$species_name %||% NA_character_,   
    infra_name = x$taxon$infra_name %||% NA_character_,   
    
    # RL category info
    rl_version = x$red_list_category$version %||% NA_character_,   
    rl_category = x$red_list_category$code %||% NA_character_,

    # documentation threats 
    # here i need to add a specification that it gets the content of "documentation$threats" only until the 32750 character
    # because some species have a too long description and they get cut
    documentation_threats = paste0(str_sub(x$documentation$threats, 1, 32750), collapse = "; ") %||% NA_character_
  ) 
}



## 2.2 Biogeographical realms

assessment_realms <- function(x) {
  
  # Extract relevant data with safety for missing fields in a tibble
  if ( !is.null(x$biogeographical_realms) && is.data.frame(x$biogeographical_realms) ) {
      tibble::tibble(
        
        # add info on the species
        assessment_id	= x$assessment_id %||% NA_character_,
        sis_id = x$taxon$sis_id %||% NA_character_,
        scientific_name = x$taxon$scientific_name %||% NA_character_,
        kingdom_name = x$taxon$kingdom_name %||% NA_character_,
        phylum_name = x$taxon$phylum_name %||% NA_character_,
        class_name = x$taxon$class_name %||% NA_character_,
        order_name = x$taxon$order_name %||% NA_character_,
        family_name = x$taxon$family_name %||% NA_character_,   
        genus_name = x$taxon$genus_name %||% NA_character_,
        species_name = x$taxon$species_name %||% NA_character_,   
        rl_category = x$red_list_category$code %||% NA_character_,
        
        biogeographical_realms = unlist(x$biogeographical_realms$description[[1]]) #[[1]], collapse = "; ")
        
      ) 
    } else {NA_character_}
}




## 2.3 Locations

assessment_locations <- function(x) {
  
  # Extract relevant data with safety for missing fields in a tibble
    if ( !is.null(x$locations) && is.data.frame(x$locations) ) {
      tibble::tibble(
        
        # add info on the species
        assessment_id	= x$assessment_id %||% NA_character_,
        sis_id = x$taxon$sis_id %||% NA_character_,
        scientific_name = x$taxon$scientific_name %||% NA_character_,
        kingdom_name = x$taxon$kingdom_name %||% NA_character_,
        phylum_name = x$taxon$phylum_name %||% NA_character_,
        class_name = x$taxon$class_name %||% NA_character_,
        order_name = x$taxon$order_name %||% NA_character_,
        family_name = x$taxon$family_name %||% NA_character_,   
        genus_name = x$taxon$genus_name %||% NA_character_,
        species_name = x$taxon$species_name %||% NA_character_,   
        rl_category = x$red_list_category$code %||% NA_character_,
        
        location_endemism = unlist(x$locations$is_endemic),
        location_origin = unlist(x$locations$origin),
        location_presence = unlist(x$locations$presence),
        
        # some elements of the list may be NULL, so we use map_chr to handle it
        # it means “Go through each element of x$locations$seasonality. If the element is NULL, return NA_character_, otherwise return the element itself. Collect all results into a single character vector.”
        location_seasonality = map_chr(x$locations$seasonality, ~ if (is.null(.x)) NA_character_ else paste(.x, collapse = "; ")),
        
        locations_text = unname(map_chr(x$locations$description$en, ~ if (is.null(.x)) NA_character_ else .x)),
        location_code = unlist(x$locations$code)
       
      )
    } else {
      tibble::tibble(
        
        # add info on the species
        assessment_id	= x$assessment_id %||% NA_character_,
        sis_id = x$taxon$sis_id %||% NA_character_,
        scientific_name = x$taxon$scientific_name %||% NA_character_,
        kingdom_name = x$taxon$kingdom_name %||% NA_character_,
        phylum_name = x$taxon$phylum_name %||% NA_character_,
        class_name = x$taxon$class_name %||% NA_character_,
        order_name = x$taxon$order_name %||% NA_character_,
        family_name = x$taxon$family_name %||% NA_character_,   
        genus_name = x$taxon$genus_name %||% NA_character_,
        species_name = x$taxon$species_name %||% NA_character_,   
        rl_category = x$red_list_category$code %||% NA_character_,
        
        location_origin = NA_character_,
        location_presence = NA_character_,
        location_seasonality = NA_character_,
        locations_text = NA_character_,
        location_code = NA_character_
        
      )
    }
    
}



## 2.4 Threats

assessment_threats <- function(x){
  
  # Extract relevant data with safety for missing fields in a tibble 
  if (!is.null(x$threats) && is.data.frame(x$threats)) {
    tibble::tibble(
      
      # add info on the species
      assessment_id	= x$assessment_id %||% NA_character_,
      sis_id = x$taxon$sis_id %||% NA_character_,
      scientific_name = x$taxon$scientific_name %||% NA_character_,
      kingdom_name = x$taxon$kingdom_name %||% NA_character_,
      phylum_name = x$taxon$phylum_name %||% NA_character_,
      class_name = x$taxon$class_name %||% NA_character_,
      order_name = x$taxon$order_name %||% NA_character_,
      family_name = x$taxon$family_name %||% NA_character_,   
      genus_name = x$taxon$genus_name %||% NA_character_,
      species_name = x$taxon$species_name %||% NA_character_,   
      rl_category = x$red_list_category$code %||% NA_character_,
      
      threat_scope = x$threats$scope,
      threat_timing = x$threats$timing,
      threat_score = x$threats$score,
      threat_severity = x$threats$severity,
      threat_ias = x$threats$ias,
      threat_text = x$threats$text,
      threat_code = x$threats$code
      
    )
  } else {
    tibble::tibble(
      
      # add info on the species
      assessment_id	= x$assessment_id %||% NA_character_,
      sis_id = x$taxon$sis_id %||% NA_character_,
      scientific_name = x$taxon$scientific_name %||% NA_character_,
      kingdom_name = x$taxon$kingdom_name %||% NA_character_,
      phylum_name = x$taxon$phylum_name %||% NA_character_,
      class_name = x$taxon$class_name %||% NA_character_,
      order_name = x$taxon$order_name %||% NA_character_,
      family_name = x$taxon$family_name %||% NA_character_,   
      genus_name = x$taxon$genus_name %||% NA_character_,
      species_name = x$taxon$species_name %||% NA_character_,   
      rl_category = x$red_list_category$code %||% NA_character_,
      
      threat_scope = NA_character_,
      threat_timing = NA_character_,
      threat_score = NA_character_,
      threat_severity = NA_character_,
      threat_ias = NA_character_,
      threat_text = NA_character_,
      threat_code = NA_character_
      
    )
  }
}



## 2.5 Habitat
# this is needed also to filter out the marine species
assessment_habitats <- function(x){
  
  # Extract relevant data with safety for missing fields in a tibble 
  if (!is.null(x$habitats) && is.data.frame(x$habitats)) {
    tibble::tibble(
      
      # add info on the species
      assessment_id	= x$assessment_id %||% NA_character_,
      sis_id = x$taxon$sis_id %||% NA_character_,
      scientific_name = x$taxon$scientific_name %||% NA_character_,
      kingdom_name = x$taxon$kingdom_name %||% NA_character_,
      phylum_name = x$taxon$phylum_name %||% NA_character_,
      class_name = x$taxon$class_name %||% NA_character_,
      order_name = x$taxon$order_name %||% NA_character_,
      family_name = x$taxon$family_name %||% NA_character_,   
      genus_name = x$taxon$genus_name %||% NA_character_,
      species_name = x$taxon$species_name %||% NA_character_,   
      rl_category = x$red_list_category$code %||% NA_character_,
      
      habitats_season = x$habitats$season, 
      habitats_suitability = x$habitats$suitability,
      
      # the description of the habitats is a dataframe within the dataframe within the list, so we need to handle it
      # this means “Go through each element of x$habitats$description. If the element is NULL, return NA_character_, otherwise return the element itself. Collect all results into a single character vector and remove its name (en).”
      #habitats_text = unname(map_chr(x$habitats$description, ~ if (is.null(.x)) NA_character_ else paste(.x, collapse = "; "))),
      habitats_text = unname(map_chr(x$habitats$description$en, ~ if (is.null(.x)) NA_character_ else .x)),
      habitats_code = x$habitats$code
      
    )
  } else {
    tibble::tibble(
      
      # add info on the species
      assessment_id	= x$assessment_id %||% NA_character_,
      sis_id = x$taxon$sis_id %||% NA_character_,
      scientific_name = x$taxon$scientific_name %||% NA_character_,
      kingdom_name = x$taxon$kingdom_name %||% NA_character_,
      phylum_name = x$taxon$phylum_name %||% NA_character_,
      class_name = x$taxon$class_name %||% NA_character_,
      order_name = x$taxon$order_name %||% NA_character_,
      family_name = x$taxon$family_name %||% NA_character_,   
      genus_name = x$taxon$genus_name %||% NA_character_,
      species_name = x$taxon$species_name %||% NA_character_,   
      rl_category = x$red_list_category$code %||% NA_character_,
      
      habitats_season = NA_character_, 
      habitats_suitability = NA_character_,
      habitats_text = NA_character_,
      habitats_code = NA_character_
      
    )
  }
}




## 2.6 Put everything together

# create a list of codes because I need to keep only IUCN RL species threatened by IAS
ias_codes <- c("8", "8_1", "8_1_1", "8_1_2")

# function to extract the info needed from the assessments
clean_assessments <- function(x) {  
  
  # extract the species only if it's threatened by IAS
  # this "if" statement says: if the species is threatened by IAS, then its code is among the ias_codes (the 8_ I have defined above)
  # which means there will be at least one TRUE here: ias_codes %in% x$threats$code
  # so sum them and, if if() is >= 1, then continue with the function
  if( sum(ias_codes %in% x$threats$code) ){
    
    # extract species name
    sp <- sub(" ", "_", unique(x$taxon$scientific_name)) 
      
      # create directory for species sp, if it doesn't exist
      if( !dir.exists(paste0("./outputs/IUCN_RL/rredlist_assessments/", cat, "/", sp)) ){
        
        dir.create(paste0("./outputs/IUCN_RL/rredlist_assessments/", cat, "/", sp))
        
      }
      
      # save general info of the species sp
      write.csv(assessment_general_info(x), 
                paste0("./outputs/IUCN_RL/rredlist_assessments/", cat, "/", sp, "/", sp, "_general_info.csv"),
                row.names = F)
      
      # save biogeographical realm info
      write.csv(assessment_realms(x), 
                paste0("./outputs/IUCN_RL/rredlist_assessments/", cat, "/", sp, "/", sp, "_realms.csv"),
                row.names = F)
      
      # save habitats info 
      write.csv(assessment_habitats(x), 
                paste0("./outputs/IUCN_RL/rredlist_assessments/", cat, "/", sp, "/", sp, "_habitats.csv"),
                row.names = F)
      
      # save location info
      write.csv(assessment_locations(x), 
                paste0("./outputs/IUCN_RL/rredlist_assessments/", cat, "/", sp, "/", sp, "_locations.csv"),
                row.names = F)
      
      # save threats info 
      write.csv(assessment_threats(x), 
                paste0("./outputs/IUCN_RL/rredlist_assessments/", cat, "/", sp, "/", sp, "_threats.csv"),
                row.names = F)
      
      gc()
    
  }else{
    
    # if the species is not threatened by IAS, skip it
    # extract species name
    sp <- unique(x$taxon$scientific_name) 
    
    print(paste0("........ Skipping species ", sp, ", since it's not threatened by IAS. (assessment ID: ", x$assessment_id, ")"))
    
  } 
  
}



# - # - # - # - # - # - # - # - # - # - # - # - # 
#                                               #
#                                               #
#       3. EXTRACT RED LIST INFORMATION         #
#                                               #
#                                               #
# - # - # - # - # - # - # - # - # - # - # - # - #



## 3.1 Extract the full assessments
# with this list of assessment IDs, I could use rl_assessment to get the full assessment
# but then keep only the information I need from the output list I get 

start <- Sys.time()

# for every category (CR, EN, VU)
for(cat in c("CR", "EN", "VU")){ 
  
  # get the obj for category "cat"
  y <- get(paste0("rl_", cat, "_assessments"))
  
  # create directory for category cat, if it doesn't exist
  if( !dir.exists(paste0("./outputs/IUCN_RL/rredlist_assessments/", cat)) ){
    
    dir.create(paste0("./outputs/IUCN_RL/rredlist_assessments/", cat))
    
  }
  
  # extract list of assessments for category "cat"
  assessm_list <- y$assessment_id
  
  # create a list ("tmp") where each element is the full assessment of each species in the category "cat" 
  # here what I'm saying is: create a list (lapply) by applying the function in brackets {} 
  # (that is rl_assessment) to each element ("x") of "assessm_list"
  tmp <- lapply(assessm_list, function(x) {Sys.sleep(0.5)
                                                rl_assessment(id = x,
                                                              key = IUCN_REDLIST_KEY)}
                )
  
  # assign name for the category "cat" so I can keep it in the environment
  assign(paste0("tmp_", cat), tmp)
  
  lapply(tmp, clean_assessments)
  
  # if and when it breaks, I can do 
  # match(60294370, assessm_list)
  
  # and then find the assessments before and after that index and see the species
  
  rm(tmp)
  gc()
  
}

print(paste0("Time used: ", Sys.time() - start))

# check if the marine species have been correctly removed   
new_files <- list.files("./outputs/IUCN_RL/rredlist_assessments/CR")  
old_files <- list.files("./outputs/IUCN_RL/rredlist_assessments/CR_old")
old_files[which(!old_files %in% new_files)]
length(new_files) + length(old_files[which(!old_files %in% new_files)]) == length(old_files)

  
  