# OneSTOP UNIVIE Repository

This GitHub repository is associated with the Horizon Europe Project OneSTOP #101180559. The project aims at enhancing biosecurity and managing terrestrial invasive alien species (IAS). This repository stores code and workflows developed by the project team (Task 5.4) at the University of Vienna (UNIVIE). The team is composed by Anna Schertler and [Bernd Lenzner](https://lenzner.github.io/) (task leader), and formerly [Lisa Tedeschi](https://lisa-tedeschi.github.io/).

## About OneSTOP

OneSTOP (OneBiosecurity Systems and Technology for People, Places and Pathways) is a Horizon Europe project (Grant Agreement #101180559) dedicated to pioneering an integrated approach to biosecurity for terrestrial IAS. The project focuses on:

-   **Detection**: Developing innovative methods such as environmental DNA analysis, computer vision, and citizen science initiatives for early IAS detection.

-   **Prioritization**: Implementing data-driven systems to inform management strategies and resource allocation.

-   **Dissemination**: Ensuring that data and tools are accessible to stakeholders for effective decision-making.

-   **Socio-political action**: Enhancing public engagement, policy development, and governance related to IAS management.

For more information, visit the [OneSTOP official website](https://onestop-project.eu/).

## OneSTOP Task 5.4

According to the International Union for Conservation of Nature (IUCN), nearly 3000 terrestrial species are threatened in Europe, of which more than 500 are threatened specifically by IAS. The EU Biodiversity Strategy for 2030 aims at reducing the pressure from IAS on IUCN Red List threatened species. OneSTOP Task 5.4 aims at evaluating the impact of IAS on threatened native species currently and under future land-use and climate change scenarios, and hence determine the priority actions required to reduce the impact of IAS on IUCN Red List species by 50%.

In the OneSTOP project, the UNIVIE team will develop prioritisation maps for IUCN RL species and IAS under different scenarios. 

## Content of this repository

This repository mostly contains the R scripts used to perform SDMs for European native threatened species and their IAS. The underlying data (list of species, GBIF presences) is stored in a Zenodo repository that will be published along with the Task 5.4 Milestone (latest March 2027). 

For native species, models were performed using three different sets of input data (GBIF, GBIF + pseudopresences, pseudopresences). For the IAS, the input data was sourced only from GBIF. The whole modeling framework consists of a global climate model (e.g., script global_model_GBIF.R), a European habitat model (e.g., script habitat_model_GBIF.R), and spatial cross-block validation (e.g., script CV_GBIF.R).

R scripts are renamed accordingly: the suffixes "_GBIF", "_GBIF_pseudopresences", and "_pseudopresences" represent the input data used in the script. The additional suffix "_IAS" identifies the script for IAS SDMs. If no suffix is used, the script refers to native species.   

For details regarding the modeling process, please refer to OneSTOP Task5.4 Milestone (to be published latest in March 2027), [OneSTOP Task5.1 Deliverable](https://onestop-project.eu/library/deliverables-milestones/search/d5.1), and [wiSDM2.0](https://github.com/trias-project/risk-modelling-and-mapping).

------------------------------------------------------------------------

![](https://github.com/lenzner/OneSTOP/blob/main/pictures/eu_funded_en.png)

OneSTOP receives funding from the European Union Horizon Europe Research and Innovation Programme (ID No 101180559). Views and opinions expressed are those of the author(s) only and do not necessarily reflect those of the European Union or the European Research Executive Agency (REA). Neither the EU nor REA can be held responsible for them.
