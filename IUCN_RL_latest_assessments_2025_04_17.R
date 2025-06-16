###################################################################
#################### IUCN RL LATEST ASSESSMENTS ###################
###################################################################

# Started on 23.05.2025
# by LT

# this is a script to explore the issue of IUCN RL assessments downloaded with the latest_assessments == T
# to see which species are kept and which are not based on the last (or not) assessment
# for IUCN RL IAS-threatened species

# Setting up the R environment
rm(list=ls())
setwd("D:/Work/oneSTOP")
getwd()

# Load required packages
pacman::p_load(viridis, rredlist, tidyverse)
sessionInfo()

# R and Package versions used in this script: 
# R version 4.4.2 
# viridis_0.6.5
# rredlist_1.0.0    
# tidyverse_2.0.0  



# - # - # - # - # - # - # - # - # 
#                               #
#                               #
#         1. LOAD DATA          #
#                               #
#                               #
# - # - # - # - # - # - # - # - # 



## 1.1 Load data

# WITHOUT latest == T
sp_general_info_no_lat <- read.csv("D:/Work/oneSTOP/outputs/IUCN_RL/rredlist_assessments_no_latest/sp_general_info.csv", h = T, sep = ",")
head(sp_general_info_no_lat)
nrow(sp_general_info_no_lat) # 736

# with latest == T
sp_general_info <- read.csv("D:/Work/oneSTOP/outputs/IUCN_RL/rredlist_assessments/sp_general_info_clean.csv", h = T, sep = ",")
head(sp_general_info)
nrow(sp_general_info) # 607



## 1.2 Explore
length(unique(sp_general_info_no_lat$scientific_name)) # 446 sp
length(unique(sp_general_info$scientific_name)) # 419 sp



# let's see which ones are left out
tmp <- anti_join(sp_general_info_no_lat, sp_general_info, by = "scientific_name")
nrow(tmp) # 27
colnames(tmp)

tmp[,1:18]

# this output needs to be checked manually, because latest == T excludes somehow some species that are still threatened and still classified as threatened



# - # - # - # - # - # - # - # - # 
#                               #
#                               #
#        2. CHECK THE DATA      #
#                               #
#                               #
# - # - # - # - # - # - # - # - # 

# these are the species that have NOT been downloaded with the option of latest_assessment == T
# but that are present in the output of the website
sort(unique(tmp$scientific_name))



# "Abies nebrodensis" - TO ADD TO THE LIST        
sp_general_info %>% filter(scientific_name == "Abies nebrodensis") # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == "Abies nebrodensis") # but present in the old one as CR
rl_species(genus = "Abies", species = "nebrodensis", key = IUCN_REDLIST_KEY) # but latest assessment of 2017 classifies it as CR in Europe

# "Aethionema retsina" - TO ADD TO THE LIST        
sp_general_info %>% filter(scientific_name == "Aethionema retsina") # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == "Aethionema retsina") # but present in the old one as CR
rl_species(genus = "Aethionema", species = "retsina", key = IUCN_REDLIST_KEY) # but latest assessment of 2024 classifies it as CR in Europe

# "Arvicola sapidus"
sp_general_info %>% filter(scientific_name == "Arvicola sapidus") # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == "Arvicola sapidus") # but present in the old one as VU
rl_species(genus = "Arvicola", species = "sapidus", key = IUCN_REDLIST_KEY) # latest assessment of 2024 classifies it as NT in Europe

# "Avellara fistulosa" - TO ADD TO THE LIST    
sp <- "Avellara fistulosa"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as CR in Europe

# "Aythya marila"
sp <- "Aythya marila"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2021 classifies it as LC in Europe

# # "Atriplex lanfrancoi" - TO ADD TO THE LIST          
# sp_general_info %>% filter(scientific_name == "Atriplex lanfrancoi") # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == "Atriplex lanfrancoi") # but present in the old one as CR
# rl_species(genus = "Atriplex", species = "lanfrancoi", key = IUCN_REDLIST_KEY) # latest assessment of 2017 classifies it as EN in Europe
# 
# "Bombina pachypus"
sp_general_info %>% filter(scientific_name == "Bombina pachypus") # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == "Bombina pachypus") # but present in the old one as EN
rl_species(genus = "Bombina", species = "pachypus", key = IUCN_REDLIST_KEY) # no result
# when checking the IUCN RL page, we can see that it has been taxonomically re-assigned and it's now Bombina variegata
# and it's LC
rl_species(genus = "Bombina", species = "variegata", key = IUCN_REDLIST_KEY) # latest assessment of 2024 classifies it as LC in Europe

# "Centaurea akamantis" - TO ADD TO THE LIST   
sp_general_info %>% filter(scientific_name == "Centaurea akamantis") # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == "Centaurea akamantis") # but present in the old one as CR
rl_species(genus = "Centaurea", species = "akamantis", key = IUCN_REDLIST_KEY) # latest assessment of 2017 classifies it as EN in Europe

# # "Centranthus trinervis" - TO ADD TO THE LIST    
# sp <- "Centranthus trinervis"  
# gen <- strsplit(sp, " ")[[1]][1]
# spsp <- strsplit(sp, " ")[[1]][2]
# sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
# rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2011 classifies it as EN in Europe

# "Chalcides simonyi"
sp <- "Chalcides simonyi"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2024 classifies it as DD in Europe

# "Charadrius leschenaultii"
sp <- "Charadrius leschenaultii"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2021 classifies it as NT in Europe

# "Cherleria dirphya" - TO ADD TO THE LIST    
sp <- "Cherleria dirphya"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as CR in Europe

# "Coenonympha oedippus"
sp <- "Coenonympha oedippus"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as NT in Europe

# "Coenonympha orientalis"
sp <- "Coenonympha orientalis"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest (but it's latest == FALSE) assessment of 2010 classifies it as VU in Europe
# but on the webpage there is an assessment of 2025 classifying it as NT

# "Colias chrysotheme" - TO ADD TO THE LIST
sp <- "Colias chrysotheme"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as EN in Europe

# "Colias myrmidone" - TO ADD TO THE LIST
sp <- "Colias myrmidone"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as VU in Europe

# "Delphinium caseyi" - TO ADD TO THE LIST     
sp <- "Delphinium caseyi"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2018 classifies it as CR in Europe

# "Gallotia auaritae"
sp <- "Gallotia auaritae"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # not found
# the species is probably now as a subspecies of G. simonyi, which is downloaded

# # "Gallotia intermedia" - TO ADD TO THE LIST       
# sp <- "Gallotia intermedia"  
# gen <- strsplit(sp, " ")[[1]][1]
# spsp <- strsplit(sp, " ")[[1]][2]
# sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
# rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2024 classifies it as EN in Europe
# 
# # "Gallotia simonyi" - TO ADD TO THE LIST           
# sp <- "Gallotia simonyi"  
# gen <- strsplit(sp, " ")[[1]][1]
# spsp <- strsplit(sp, " ")[[1]][2]
# sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
# rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2024 classifies it as VU in Europe

# "Gypaetus barbatus"
sp <- "Gypaetus barbatus"
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2021 classifies it as NT in Europe

# "Horstrissea dolinicola" - TO ADD TO THE LIST        
sp <- "Horstrissea dolinicola"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as CR in Europe

# # "Juniperus brevifolia" - TO ADD TO THE LIST          
# sp <- "Juniperus brevifolia"  
# gen <- strsplit(sp, " ")[[1]][1]
# spsp <- strsplit(sp, " ")[[1]][2]
# sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
# rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2011 classifies it as VU in Europe
# 
# # "Linaria pseudolaxiflora" - TO ADD TO THE LIST      
# sp <- "Linaria pseudolaxiflora"  
# gen <- strsplit(sp, " ")[[1]][1]
# spsp <- strsplit(sp, " ")[[1]][2]
# sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
# rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2019 classifies it as VU in Europe

# "Mauremys leprosa"
sp <- "Mauremys leprosa"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2024 classifies it as NT in Europe

# # "Mustela lutreola" - TO ADD TO THE LIST              
# sp <- "Mustela lutreola"  
# gen <- strsplit(sp, " ")[[1]][1]
# spsp <- strsplit(sp, " ")[[1]][2]
# sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
# rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as CR in Europe

# "Pararge xiphia"
sp <- "Pararge xiphia"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as NT in Europe

# # "Pelophylax shqipericus" - TO ADD TO THE LIST         
# sp <- "Pelophylax shqipericus"  
# gen <- strsplit(sp, " ")[[1]][1]
# spsp <- strsplit(sp, " ")[[1]][2]
# sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
# rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2024 classifies it as VU in Europe

# "Picconia azorica"        
sp <- "Picconia azorica"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2017 classifies it as LC in Europe

# "Pieris wollastoni"        
sp <- "Pieris wollastoni"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as EX in Europe

# "Podarcis lilfordi"   
sp <- "Podarcis lilfordi"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2024 classifies it as NT in Europe

# # "Podarcis raffonei" - TO ADD TO THE LIST            
# sp_general_info %>% filter(scientific_name == "Podarcis raffonei") # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == "Podarcis raffonei") # but present in the old one 
# rl_species(genus = "Podarcis", species = "raffonei", key = IUCN_REDLIST_KEY) # but latest assessment of 2024 classifies it as EN in Europe

# "Pseudochazara cingovskii" - TO ADD TO THE LIST       
sp <- "Pseudochazara cingovskii"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2025 classifies it as CR in Europe

# # "Rhacocleis buchichii" - TO ADD TO THE LIST         
# sp <- "Rhacocleis buchichii"  
# gen <- strsplit(sp, " ")[[1]][1]
# spsp <- strsplit(sp, " ")[[1]][2]
# sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
# sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as EN
# rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2016 classifies it as EN in Europe

# "Sorbus eminens" - TO ADD TO THE LIST       
sp <- "Sorbus eminens"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2017 classifies it as VU in Europe

# "Sorbus vexans" - TO ADD TO THE LIST             
sp <- "Sorbus vexans"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as VU
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2017 classifies it as CR in Europe

# "Turnix sylvaticus" 
sp <- "Turnix sylvaticus"  
gen <- strsplit(sp, " ")[[1]][1]
spsp <- strsplit(sp, " ")[[1]][2]
sp_general_info %>% filter(scientific_name == sp) # not present when downloaded with latest == T
sp_general_info_no_lat %>% filter(scientific_name == sp) # but present in the old one as CR
rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY) # latest assessment of 2021 classifies it as RE in Europe



# - # - # - # - # - # - # - # - # 
#                               #
#                               #
#         3. SAVE THE SP        #
#                               #
#                               #
# - # - # - # - # - # - # - # - # 



## 3.1 Write the list of species to add
sp_to_add <- data.frame("species" = c("Abies nebrodensis",      
                                      "Aethionema retsina", 
                                      "Avellara fistulosa", 
                                      "Centaurea akamantis", 
                                      "Cherleria dirphya", 
                                      "Colias chrysotheme", 
                                      "Colias myrmidone", 
                                      "Delphinium caseyi",  
                                      "Horstrissea dolinicola",     
                                      "Pseudochazara cingovskii",      
                                      "Sorbus eminens",     
                                      "Sorbus vexans") )

sp_to_add

write.csv(sp_to_add, "./outputs/IUCN_RL/rredlist_assessments/latest_ass_species_missing.csv", row.names = F)



# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # 
#                                                                       #
#                                                                       #
#          4. EXTRACT THE ASSESSMENTS FOR THE MISSING SPECIES           #
#                                                                       #
#                                                                       #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # 

# create empty vector
add_assessm_list <- c()

nrow(sp_to_add) # 12 sp

for(sp in sp_to_add$species){

  gen <- strsplit(sp, " ")[[1]][1]
  spsp <- strsplit(sp, " ")[[1]][2]
  
  # retrieve assessment for species sp
  y <- rl_species(genus = gen, species = spsp, key = IUCN_REDLIST_KEY)$assessments
  
  # now keep only the latest == T 
  y <- y %>% filter(latest == TRUE)
  
  # keep the European assessment
  # scopes is a list of dataframes
  x <- unique(y[sapply(y$scopes, function(scope) grepl("Europe", scope$description$en, fixed = TRUE)), ]$assessment_id)    
  
  # remove NAs
  x <- x[!is.na(x)]
  
  # x is the assessment number 
  
  # bind
  add_assessm_list <- c(add_assessm_list, x)
  
}

length(add_assessm_list) # 12


# extract assessments
tmp <- lapply(add_assessm_list, function(x) {Sys.sleep(0.5)
  rl_assessment(id = x,
                key = IUCN_REDLIST_KEY)}
)



# function to clean the assessments
# NB here we add a folder for this new assessments: outputs/IUCN_RL/rredlist_assessments/latest_ass
add_clean_assessments <- function(x) {  
  
  # extract the species only if it's threatened by IAS
  # this "if" statement says: if the species is threatened by IAS, then its code is among the ias_codes (the 8_ I have defined above)
  # which means there will be at least one TRUE here: ias_codes %in% x$threats$code
  # so sum them and, if if() is >= 1, then continue with the function
  if( sum(ias_codes %in% x$threats$code) ){
    
    # extract species name
    sp <- sub(" ", "_", unique(x$taxon$scientific_name)) 
    
    # extract category 
    cat <- x$red_list_category$code
    
    # create directory for species sp, if it doesn't exist
    if( !dir.exists(paste0("./outputs/IUCN_RL/rredlist_assessments/latest_ass/", cat, "/", sp)) ){
      
      dir.create(paste0("./outputs/IUCN_RL/rredlist_assessments/latest_ass/", cat, "/", sp))
      
    }
    
    # save general info of the species sp
    write.csv(assessment_general_info(x), 
              paste0("./outputs/IUCN_RL/rredlist_assessments/latest_ass/", cat, "/", sp, "/", sp, "_general_info.csv"),
              row.names = F)
    
    # save biogeographical realm info
    write.csv(assessment_realms(x), 
              paste0("./outputs/IUCN_RL/rredlist_assessments/latest_ass/", cat, "/", sp, "/", sp, "_realms.csv"),
              row.names = F)
    
    # save habitats info 
    write.csv(assessment_habitats(x), 
              paste0("./outputs/IUCN_RL/rredlist_assessments/latest_ass/", cat, "/", sp, "/", sp, "_habitats.csv"),
              row.names = F)
    
    # save location info
    write.csv(assessment_locations(x), 
              paste0("./outputs/IUCN_RL/rredlist_assessments/latest_ass/", cat, "/", sp, "/", sp, "_locations.csv"),
              row.names = F)
    
    # save threats info 
    write.csv(assessment_threats(x), 
              paste0("./outputs/IUCN_RL/rredlist_assessments/latest_ass/", cat, "/", sp, "/", sp, "_threats.csv"),
              row.names = F)
    
    gc()
    
  }else{
    
    # if the species is not threatened by IAS, skip it
    # extract species name
    sp <- unique(x$taxon$scientific_name) 
    
    print(paste0("........ Skipping species ", sp, ", since it's not threatened by IAS. (assessment ID: ", x$assessment_id, ")"))
    
  } 
  
}

lapply(tmp, add_clean_assessments)

# turned out they are NOT threatened by IAS, no need to add them 










