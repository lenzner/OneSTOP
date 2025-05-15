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

# Package versions used in this script: 
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
                                    #latest = T,
                                    scope_code = 2,
                                    key = IUCN_REDLIST_KEY)$assessments

length(rl_CR_assessments$assessment_id) # 1011 assessments in Europe for species with CR category if i don't keep only the latest ones
length(rl_CR_assessments$assessment_id) # 768 assessments in Europe for species with CR category if i keep only the latest ones



## 1.2 Get the assessment IDs for the species in Europe categorized as EN
rl_EN_assessments <- rl_categories(code = "EN",
                                    #latest = T,
                                    scope_code = 2,
                                    key = IUCN_REDLIST_KEY)$assessments

length(rl_EN_assessments$assessment_id) # 1800 assessments in Europe for species with EN category if i don't keep only the latest ones



## 1.3 Get the assessment IDs for the species in Europe categorized as VU
rl_VU_assessments <- rl_categories(code = "VU",
                                    #latest = T,
                                    scope_code = 2,
                                    key = IUCN_REDLIST_KEY)$assessments

length(rl_VU_assessments$assessment_id) # 2089 assessments in Europe for species with VU category if i don't keep only the latest ones



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
ias_codes <- c("8", 
               "8_1", "8_2", "8_3", "8_4", "8_5", "8_6",
               "8_1_1", "8_1_2",
               "8_2_1", "8_2_2",
               "8_4_1", "8_4_2",
               "8_5_1", "8_5_2")

# and list the habitats we want to remove (wetlands # 5, marine # 6) - TO DISCUSS: amphibians in wetlands?
habitats_code <- c("5",
                   "5_1", "5_2", "5_3", "5_4", "5_5", "5_6", "5_7", "5_8", "5_9",
                   "5_10", "5_11", "5_12", "5_13", "5_14", "5_15", "5_16", "5_17", "5_18",
                   "9",
                   "9_1", "9_2", "9_3", "9_4", "9_5", "9_6", "9_7", "9_8", "9_9", "9_10",
                   "9_8_1","9_8_2","9_8_3","9_8_4","9_8_5", "9_8_6",
                   "10_1", "10_2", "10_3", "10_4",
                   "11_1", "11_2", "11_3", "11_4", "11_5", "11_6",
                   "11_1_1", "11_1_2")


clean_assessments <- function(x) {  
  
  # extract the species only if it's threatened by IAS
  # this "if" statement says: if the species is threatened by IAS, then its code is among the ias_codes (the 8_ I have defined above)
  # which means there will be at least one TRUE here: ias_codes %in% x$threats$code
  # so sum them and, if if() is >= 1, then continue with the function
  if( sum(ias_codes %in% x$threats$code) ){
    
      # extract species name
      sp <- sub(" ", "_", unique(x$taxon$scientific_name)) 
    
      # extract the species only if it's also terrestrial  
      # here the number is the same only is the species has all habitats listed among the ones we don't want to keep
      # meaning: first I sum how many of the habitats of the species are among the ones I don't want to keep: sum(habitats_code %in% x$habitats$code)
      # then, if this number is the same as the total habitats of the species (== length(x$habitats$code)), it means the species is only present in the habitats we don't want
      # so in that case I need to skip it
      if ( sum(habitats_code %in% x$habitats$code) == length(x$habitats$code) ) {        

        # extract species name
        sp <- unique(x$taxon$scientific_name) 
        
        print(paste0("........ Skipping species ", sp, ", since it's only freshwater/marine. (assessment ID: ", x$assessment_id, ")"))
        
        }else if ( isFALSE(sum(habitats_code %in% x$habitats$code) == length(x$habitats$code)) ){
    
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
    
      }
    
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

  
  
  
  
  
  
  


  
  


















# for every category (CR, EN, VU)
for(cat in c("CR", "EN", "VU")){ 
  
  # get the obj for category "cat"
  y <- get(paste0("rl_", cat, "_assessments"))
  
  # extract list of assessments for category "cat"
  assessm_list <- y$assessment_id
  
  # initialize the loop 
  i = 1
  myList <- list()
  
  # loop through the assessments
  for(assessment in assessm_list){
  #for(assessment in assessm_list[1482:length(assessments_cat$assessment_id)]){  
    
    # extract the full assessment
    x <- rl_assessment(id = 212995510,
                       key = IUCN_REDLIST_KEY)
    Sys.sleep(1)
    #212995510
    
    # keep the species only if it's threatened by IAS
    if( sum(ias_codes %in% x$threats$code) ){
    
    # I create an empty df where I will store the information for each assessment
    df <- data.frame("year_published" = 0,
               "latest"= 0,
               "possibly_extinct"= 0,
               "possibly_extinct_in_the_wild"= 0,
               "assessment_id"= 0,
               "sis_id"= 0,
               "scientific_name" = 0,
               "kingdom_name" = 0,
               "phylum_name" = 0,
               "class_name" = 0,
               "order_name" = 0,
               "family_name" = 0,
               "genus_name" = 0,
               "species_name" = 0,
               "infra_name" = 0,
               "rl_version" = 0,
               "rl_category" = 0,
               "documentation_threats" = 0,
               "biogeographical_realms" = 0,
               "presence" = 0,
               "origin" = 0,
               "threat_scope" = 0,             
               "threat_timing" = 0,             
               "threat_internationalTrade" = 0, 
               "threat_score" = 0,              
               "threat_severity" = 0,           
               "threat_ancestry" = 0,           
               "threat_virus" = 0,              
               "threat_ias" = 0,               
               "threat_text" = 0,             
               "threat_description" = 0,       
               "threat_code" = 0)
    df
    
    # then i start filling in the info for each species
    df$year_published <- x$year_published
    df$latest <- x$latest
    df$possibly_extinct <- x$possibly_extinct
    df$possibly_extinct_in_the_wild <- x$possibly_extinct_in_the_wild
    df$assessment_id <- x$assessment_id
    df$sis_id <- x$taxon$sis_id
    df$scientific_name <- x$taxon$scientific_name
    df$kingdom_name <- x$taxon$kingdom_name
    df$phylum_name <- x$taxon$phylum_name
    df$class_name <- x$taxon$class_name
    df$order_name <- x$taxon$order_name
    df$family_name <- x$taxon$family_name       
    df$genus_name <- x$taxon$genus_name
    df$species_name <- x$taxon$species_name   
    df$infra_name <- x$taxon$infra_name   
    df$rl_version <- x$red_list_category$version   
    df$rl_category <- x$red_list_category$code
    df$documentation_threats <- x$documentation$threats
    
    df
    
    # biogeographical_realms can be empty 
    if( length(unique(x$biogeographical_realms$description[[1]])) == 0){
      
      df$biogeographical_realms <- "NA"
      
    }else if( (length(unique(x$biogeographical_realms$description[[1]])) > 0) & (nrow(df) == length(unique(x$biogeographical_realms$description[[1]])) ) ){ # biogeographical_realms can be more than one row, so I duplicate the first row of df as many times as the biogeographical_realms are
      
      df$biogeographical_realms <- x$biogeographical_realms$description[[1]]
      
    }else if( (length(unique(x$biogeographical_realms$description[[1]])) > 0) & (nrow(df) != length(unique(x$biogeographical_realms$description[[1]])) ) ){ # biogeographical_realms can be more than one row, so I duplicate the first row of df as many times as the biogeographical_realms are
      
      df <- df %>% slice(rep(1, length(unique(x$biogeographical_realms$description[[1]]))))
      
    }
    
    # presences can be more than one row, so I duplicate the first row of df as many times as the presences are
    if(nrow(df) != length(unique(x$locations$presence))){
      
      df <- df %>% slice(rep(1, length(unique(x$locations$presence))))
      
    }
    
    df$presence  <- unique(x$locations$presence)
    
    
    
    
    
    
    ############## here I need to add the countries, because based on the countries there may be a different presence status
    # eg try to downlooad the 212995510 assessment and run x$locations
    
    

    
    
    # origin can be more than one row, so I duplicate the first row of df as many times as the origin are
    if(nrow(df) != length(unique(x$locations$origin))){
      
      df <- df %>% slice(rep(1, length(unique(x$locations$origin))))
      
    }
    
    df$origin <- unique(x$locations$origin)
    
    # threats can be more than one row, so I duplicate the first roiw of df as many times as the rows of x$threats are
    if(nrow(df) != nrow(x$threats)){
      
      df <- df %>% slice(rep(1, nrow(x$threats)))
      
    }
    
    # then I have to add in the threat information 
    df$threat_scope <- x$threats$scope              
    df$threat_timing <- x$threats$timing             
    df$threat_internationalTrade <- x$threats$internationalTrade 
    df$threat_score <- x$threats$score              
    df$threat_severity <- x$threats$severity           
    df$threat_ancestry <- x$threats$ancestry           
    df$threat_virus <- x$threats$virus              
    df$threat_ias <- x$threats$ias                
    df$threat_text <- x$threats$text               
    df$threat_description <- x$threats$description$en       
    df$threat_code <- x$threats$code  
    
    df
    
    # keep species name
    sp <- sub(" ", "_", unique(df$scientific_name)) 
    
    # save output for species sp 
    write.csv(df, 
              paste0("./outputs/IUCN_RL/rredlist_assessments/", sp, ".csv"),
              row.names = F)
    
    # save the output in the list
    myList[[i]] <- df
    
    i = i + 1 
    
    gc()
    
    }else{
      
      # keep species name
      sp <- unique(x$taxon$scientific_name) 
      
      print(paste0("........ Skipping species ", sp, ", since it's not threatened by IAS."))
      
      i = i + 1
      next
      
    }
  }
  
  # save full output for category "cat"
  
  tmp <- lapply(myList, function(df){
    
    df$presence <- as.character(df$presence)
    df$origin <- as.character(df$origin)
    df$biogeographical_realms <- as.character(df$biogeographical_realms)
    df
  
  })
  
  write.csv(bind_rows(tmp),
  paste0("./outputs/IUCN_RL/", cat, "_assessment.csv"),
  row.names = F)
  
}




clean_assessment <- function(x) {
  
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
    
    # realms 
    biogeographical_realms = if (!is.null(x$biogeographical_realms$description)) {
      
      paste0(x$biogeographical_realms$description[[1]], collapse = "; ")
      
    } else {NA_character_},
    
    # locations
    if (!is.null(x$locations) && is.data.frame(x$locations)) {
      tibble::tibble(
        
        location_origin = x$locations$origin,
        location_presence = x$locations$presence,
        location_seasonality = x$locations$seasonality,
        location_code = x$locations$code
        
      )
    } else {
      tibble::tibble(
        
        location_origin = NA_character_,
        location_presence = NA_character_,
        location_seasonality = NA_character_,
        location_code = NA_character_
        
      )
    },
    
    # threats 
    if (!is.null(x$threats) && is.data.frame(x$threats)) {
      tibble::tibble(
        
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
        
        threat_scope = NA_character_,
        threat_timing = NA_character_,
        threat_score = NA_character_,
        threat_severity = NA_character_,
        threat_ias = NA_character_,
        threat_text = NA_character_,
        threat_code = NA_character_
        
      )
    },
    
    # documentation threats 
    documentation_threats = paste0(x$documentation$threats, collapse = "; ") %||% NA_character_

  ) 
  
 
}

df <- clean_assessment(y)

cleaned_assessments <- lapply(assessment_list, clean_assessment)

df
View(df)


result <- purrr::map_dfr(list_of_assessments, clean_assessment)
result




t <- list()
t$biogeographical_realms <- structure(list(description = structure(list(en = "Palearctic"), class = "data.frame", row.names = 1L), 
                     code = "7"), class = "data.frame", row.names = 1L)
t$locations <- structure(list(is_endemic = c(FALSE, FALSE, FALSE, FALSE, FALSE, 
                                             FALSE), formerlyBred = c(NA, NA, NA, NA, NA, NA), origin = c("Native", 
                                                                                                          "Native", "Native", "Native", "Native", "Native"), presence = c("Extant", 
                                                                                                                                                                          "Extant", "Extant", "Extant", "Extinct Post-1500", "Extant"), 
                              seasonality = list("Resident", "Resident", "Resident", "Resident", 
                                                 "Resident", "Resident"), description = structure(list(
                                                   en = c("Spain", "Russian Federation", "United Kingdom", 
                                                          "Spain (mainland)", "Lithuania", "Great Britain")), row.names = c(NA, 
                                                                                                                            6L), class = "data.frame"), code = c("ES", "RU", "GB", "SPA-SP", 
                                                                                                                                                                 "LT", "GRB-OO")), row.names = c(NA, 6L), class = "data.frame")
t
