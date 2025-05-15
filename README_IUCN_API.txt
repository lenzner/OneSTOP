Created on 15.05.2025 by Lisa Tedeschi
This README file contains the description of the process followed so far for downloading IUCN RL assessment for IAS-threatened native species in Europe.
It couples with the R script "IUCN_RL_2025_04_02.R" and "IUCN_RL_assessments_merging_2025_04_17.R", which can be downloaded in the OneSTOP Task 5.4 Github repository: https://github.com/lenzner/OneSTOP 
The version of rredlist R package used in the script is rredlist_1.0.0  

First you need to request a IUCN RL v.4 API here: https://api.iucnredlist.org/ 

I downloaded the assessments for all threatened species of CR, EN, and VU categories through rredlist::rl_categories()
I limited the donwload to the European scope. Note that even with this limitation some polygons of species outside Europe (e.g., Afghanistan) are retrieved. I am in contact with Lien Reyserhove who's going to discuss the issue of European boundaries with GRIIS.
I downloaded all assessments, not only the latest ones.

The new API gives an output difficult to handle, because it's a list with different lists/dataframes/vectors inside.
I created a set of custom functions to retrieve the information I needed from the assessments. These are tailored to my needs but can be adjusted based on needs.
Then I filtered the assessments to keep only IAS-threatened native species, i.e., code 8 and sub-categories.
I also filtered by habitat, and if a species is present only in the habitats I have excluded, then the species is dropped. But if the species is present also in different habitats, it is kept. 
So far I have excluded habitat 5 (and sub-categories), 9 (and sub-categories), 10 (and sub-categories), 11 (and sub-categories), and some sub-categories of 15. 
Probably more habitats needs to be removed, because I still get some species that are clearly only aquatic (e.g., fishes). But this can be a post-hoc procedure where I just remove fishes.

Finally, I merge all the custom functions into one big function that: takes the species assessment, retain the species only if it's IAS-threatened and if it's not only aquatic, applies each custom function to extract the information I need and create a .csv out of it.
Each species gets its own folder within the threat category.
Namely I store (each in a different .csv): general information of the species and of the assessment, biogeographical realm, habitats, location (coutnries), threats. 
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

Lastly, I merge all the species .csv for each piece of information (e.g., one for the habitats, one for the threats) into a single .csv, which will therefore contain all the IAS-threatened native species of the 3 threat categories considered (e.g., "sp_general_info.csv"). 
From the .csv of threats, I keep the named IAS and their frequencies (i.e., how many times it is indicated that they threaten a species), to see the most common ones. This is "named_IAS.csv". 