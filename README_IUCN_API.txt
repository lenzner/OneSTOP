Created on 15.05.2025 by Lisa Tedeschi
Last update: 16.06.2025
This README file contains the description of the process followed so far for downloading IUCN RL assessment for IAS-threatened native species in Europe.


# - # - # - # - # - # - # - # - # - # - # - # 
#                               			#
#                               			#
#           1. RREDLIST DOWNLOAD         	#
#                               			#
#                               			#
# - # - # - # - # - # - # - # - # - # - # - # 

It couples with the R script "IUCN_RL_2025_04_02.R" and "IUCN_RL_assessments_merging_2025_04_17.R", which can be downloaded in the OneSTOP Task 5.4 Github repository: https://github.com/lenzner/OneSTOP 
The version of rredlist R package used in the script is rredlist_1.0.0  

First you need to request a IUCN RL v.4 API here: https://api.iucnredlist.org/ 

## CATEGORIES
I downloaded the assessments for all threatened species of CR, EN, and VU categories through rredlist::rl_categories()

## GEOGRAPHICAL SCOPE 
I limited the download to the European scope. Note that even with this limitation some polygons of species outside Europe (e.g., Afghanistan) are retrieved. 
The boundaries of Europe that we are going to use are in concordance with other tasks of OneSTOP (e.g., Lien Reyserhove's) and GuardIAS.
For instance, countries that can have been removed are Armenia, Azerbaijan, Georgia, Greenland, and the Spanish North African Territories.
The cleaning tnd filtering process is here: IUCN_RL_countries_exploration_2025_05_16.R
Then I also removed countries where the species are marked as "Introduced", and countries where the species are marked as "Extinct Post-1500"

## LATEST ASSESSMENTS
Since some species have multiple assessments (due to being re-assessed in different categories), I downloaded only the latest assessments.
However, there may be an inconsistency between the assessments downloaded with latest == T and those without.
Here I explored the issue: "IUCN_RL_latest_assessments_2025_04_17.R"

## THREATS
I limited the download to species threatened by IAS (category 8.1 and sub-categories).



The new API gives an output difficult to handle, because it's a list with different lists/dataframes/vectors inside.
I created a set of custom functions to retrieve the information I needed from the assessments. These are tailored to my needs but can be adjusted based on needs.
Then I filtered the assessments to keep only IAS-threatened native species.

Finally, I merge all the custom functions into one big function that: takes the species assessment, retain the species only if it's IAS-threatened, applies each custom function to extract the information I need and create a .csv out of it.
Each species gets its own folder within its threat category.
Namely I store (each in a different .csv): general information of the species and of the assessment, biogeographical realm, habitats, location (countries), threats. 
Example structure:

CR
|
|
|---Mustela_lutreola
	|
	|
	|---Mustela_lutreola_general_info.csv
	|---Mustela_lutreola_habitats.csv
	|---Mustela_lutreola_locations.csv
	|---Mustela_lutreola_realms.csv
	|---Mustela_lutreola_threats.csv	

Lastly, I merge all the species .csv ("IUCN_RL_assessments_merging_2025_04_17.R") for each piece of information (e.g., one for the habitats, one for the threats) into a single .csv, which will therefore contain all the IAS-threatened native species of the 3 threat categories considered (e.g., ""sp_general_info.csv""). 
From the .csv of threats, I keep the named IAS and their frequencies (i.e., how many times it is indicated that they threaten a species), to see the most common ones. This is "named_IAS.csv".



# - # - # - # - # - # - # - # - # - # - # 
#                               		#
#                               		#
#       	2. WEBSITE DOWNLOAD         #
#                               		#
#                               		#
# - # - # - # - # - # - # - # - # - # - # 

Then I also want to comapre the output of rredlist to the output of the website. I modified my profile to be able to download the information regarding threats, habitats, etc. 
Then I performed an advanced search using the following filters:
- Taxonomy: Animalia, Fungi, Plantae
- Geographical scope: Europe
- RL Category: CR, EN, VU
- Threats: 8.1
- Include: species 

I don't specify anything in the field "Land Regions", because if I select only "Europe", some species are left out (e.g., sp occurring in Crimea or Cyprus)
The comparison between the two outputs is done in "IUCN_RL_rredlist_vs_website_2025_05_26.R"
