##########################################################################
#################### IUCN RL ASSESSMENTS COUNTRIES CLEANING ##############
##########################################################################

# Started on 16.05.2025
# by LT

# this is a script to explore the IUCN RL assessments downloaded with the new API
# for IUCN RL IAS-threatened species
# dealing with countries of occurrence
# and cleaning the output removing countries we don't need

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
#          1. LOAD DATA         #
#                               #
#                               #
# - # - # - # - # - # - # - # - # 

sp_locations <- read.csv("./outputs/IUCN_RL/rredlist_assessments/sp_locations_clean.csv", h = T, sep = ",")
head(sp_locations)
length(unique(sp_locations$scientific_name)) # 419 sp
nrow(sp_locations) # 2490



## 1.1 Remove countries where sp have been introduced 

unique(sp_locations$location_origin)

sp_locations <- sp_locations %>% filter(!location_origin == "Introduced")
nrow(sp_locations) # 2462
length(unique(sp_locations$scientific_name)) # 419 sp - no sp kicked out, good



## 1.2 Remove countries where sp are extinct after 1500

unique(sp_locations$location_presence)

sp_locations <- sp_locations %>% filter(!location_presence == "Extinct Post-1500")
nrow(sp_locations) # 2411
length(unique(sp_locations$scientific_name)) # 419 sp - no sp kicked out, good



## 1.3 Remove countries where sp are passage and uncertain?




## 2.4 Remove extra-UE countries

sort(unique(sp_locations$locations_text))

# define countries to remove: of these, I'm pretty sure
countries_to_rm <- c( "Afghanistan", "Algeria", "Armenia", "Azerbaijan", 
                      "Bahrain", "Bangladesh", "Belarus",
                      "Cabo Verde","Central Asian Russia","Central European Russia", "China","Côte d'Ivoire",
                      "Djibouti",
                      "East European Russia","Eastern Asian Russia","Egypt","Eritrea","Ethiopia","European Russia",
                      "Gambia","Georgia", "Ghana", "Guinea","Guinea-Bissau", "Greenland", 
                      "India", "Iraq", "Iran, Islamic Republic of", "Israel",
                      "Japan", "Jordan",                                
                      "Kaliningrad", "Kazakhstan", "Kenya", "Korea, Democratic People's Republic of", "Korea, Republic of","Kuwait", "Kyrgyzstan", 
                      "Libya",
                      "Mauritania", "Myanmar","Morocco",
                      "Nigeria", "North European Russia", "Northwest European Russia",
                      "Oman",
                      "Pakistan", "Palestine, State of",
                      "Qatar",
                      "Russian Federation",
                      "Saudi Arabia", "Senegal", "Sierra Leone","Somalia","Spanish North African Territories","South European Russia","Sri Lanka", "Sudan","Syrian Arab Republic",    
                      "Taiwan, Province of China", "Tajikistan","Tanzania, United Republic of","Tunisia","Türkiye", "Türkiye-in-Asia", "Türkiye-in-Europe","Turkmenistan", 
                      "Ukraine", "Ukraine (main part)","United Arab Emirates","Uzbekistan",                            
                      "Western Sahara",
                      "Yemen")

sp_locations <- sp_locations %>% filter(!locations_text %in% countries_to_rm)
nrow(sp_locations) # 2101
length(unique(sp_locations$scientific_name)) # 417 sp 

# we lose two species: 
# Spermophilus major, which occurs at the border between southern Russia and Kazakhistan
# Desmana moschata



## 1.4 Isolate islands (lol)

length(unique(sp_locations$locations_text)) # we have 110 countries
sort(table(sp_locations$locations_text))

# define islands to isolate - TO DISCUSS
islands <- c("Sicilia", "Madeira", "Kriti", "Isle of Man", "Ireland", "Iceland",                          
             "Malta", "Guernsey", "Cyprus", "Faroe Islands",                    
             "Jersey", "Corsica", "Sardegna", "East Aegean Is.", "Baleares",
             "Great Britain", "Selvagens", "Canary Is.", "Azores", "Greenland",                        
             "Svalbard and Jan Mayen", "Åland Islands")

x <- sp_locations %>% filter(locations_text %in% islands)
nrow(x) # 767
length(unique(x$scientific_name)) # 414 sp are present also/only on islands

# let's see which ones
sp_locations[which(!x$scientific_name %in% sp_locations$scientific_name),]

