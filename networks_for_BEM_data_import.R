## networks for BEM presentation
## Created by: Joey Burant
## Last updated: 13 July 2022

## This script generates the datasets (and many other derivative products)
## that are imported at the start of "networks_for_BEM.R", and has been
## subsetted from the larger "JEKB_foodwebs_scripts.R" which is itself
## a combination of several different scripts created by Jarrett

## load required packages
library(googlesheets4)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(rglobi)
library(worrms)

## set Google Sheets login/authorization
gs4_auth()

# subtidal data -----------------------------------------------------------

## import subtidal data from Byrnes Lab (most of the data)
sub_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1f4a8mYrov0DiRBOoP1lbuEh_kxhQP6t7Ay6fH5q5gME/", 
                      sheet = "Working Interaction Data", 
                      col_types = "cccccccccccc") %>% 
  select(-`Ref ID`)

## import rest of subtidal data (from working group)
sub_dat_2 <- read_sheet("https://docs.google.com/spreadsheets/d/16PwO_TI_YnSktYBolK6MPuuvyhEu7xZy7j0jplmz5sg/edit#gid=1883520352",
                        sheet = "Interaction Data", 
                        col_types = "cccccccccccc") %>% 
  select(-"...12")

## bind the subtidal data together
sub_dat <- bind_rows(sub_dat, sub_dat_2) %>%
  ## manipulate variables
  mutate(#`Ref ID` = map_chr(`Ref ID`, ~ifelse(is.null(.x), "", .x)),
    `Paper ID` = map_chr(`Paper ID`, ~ifelse(is.null(.x), "", .x)),
    observationDateTime = map_chr(observationDateTime, ~ifelse(is.null(.x), "", .x)))

## export subtidal data
write_csv(sub_dat, "./data/entered_sub_data.csv")

## drop rows where the focal species (left-hand), interaction type, or target
## species (right-hand) are missing
sub_dat <- sub_dat %>% 
  filter(!is.na(.$sourceTaxonName) & 
           !is.na(.$interactionTypeName) & 
           !is.na(.$targetTaxonName))


# pull subtidal species list from interactions database
sp_data <- read_sheet("https://docs.google.com/spreadsheets/d/1f4a8mYrov0DiRBOoP1lbuEh_kxhQP6t7Ay6fH5q5gME/", 
                      sheet = "Species List") %>% 
  select(-c("...14":"...18"))

sp_data2 <- read_sheet("https://docs.google.com/spreadsheets/d/16PwO_TI_YnSktYBolK6MPuuvyhEu7xZy7j0jplmz5sg/edit#gid=19019573", 
                       sheet = "Species List") %>%
  mutate(`Done = 1` = map_dbl(`Done = 1`, ~ifelse(is.null(.x), NA, .x) %>% as.numeric)) %>% 
  mutate(`Added as new agents` = unlist(`Added as new agents`))

sp_data <- bind_rows(sp_data, sp_data2)

write_csv(sp_data, "data/entered_species_data_sub.csv")



# intertidal data ---------------------------------------------------------

## import intertidal interaction data from working group data
int_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1ELUVTUnV1fUMc3nBC6EFgCG4jwhcWCfoewATIK-BiLw/", 
                      sheet = "Interaction Data", 
                      col_types = "cccccccccc") %>% 
  mutate(`Reference DOI` = as.character(NA), .after = `Paper ID`) %>% 
  mutate(`Reference ISBN` = as.character(NA), .before = Notes) %>% 
  select(-c("...10"))

## import other intertidal interaction data from Laura Dee et al.
## LOL NOT!

## pull intertidal species list 
sp_data_int <- read_sheet("https://docs.google.com/spreadsheets/d/1ELUVTUnV1fUMc3nBC6EFgCG4jwhcWCfoewATIK-BiLw/", 
                          sheet = "Species List") %>% 
  mutate(`Done = 1` = map_dbl(`Done = 1`, ~ifelse(is.null(.x), NA, .x) %>% as.numeric)) %>% 
  mutate(`Added as new agents` = unlist(`Added as new agents`))

## export intertidal species list
write_csv(sp_data_int, "./data/entered_species_data_int.csv")


## bind all species lists together
## (need to select common columns first, and do some name formatting)
sp_data_simple <- sp_data %>% 
  select(sourceTaxonName, `Taxonomic Level`, `EOL ID`, 
         `WORMS ID`, `Common Name`, `Done = 1`, `Added as new agents`, 
         `Aggregated taxa`, `Notes`, `search term`, `who is searching?`)

sp_data_int_simple <- sp_data_int %>% 
  select(sourceTaxonName, `Taxonomic Level`, `EOL ID`, 
         `WORMS ID`, `Common Name`, `Done = 1`, `Added as new agents`, 
         `Aggregated taxa`, `Notes`, `Search term`, `Who is searching?`) %>% 
  rename(`who is searching?` = `Who is searching?`, 
         `search term` = `Search term`)

names(sp_data_simple) == names(sp_data_int_simple)

sp_data_all <- bind_rows(sp_data_simple, sp_data_int_simple)

## how many unique species are included in this list?
n_distinct(sp_data_all$sourceTaxonName) # [1] 2364

## how many are duplicates? LOTS
sp_data_all %>% 
  group_by(sourceTaxonName) %>% 
  summarise(n = n()) %>% 
  group_by(n) %>% 
  summarise(n_species_with_x_rows = n())

#       n n_species_with_x_rows
# 1     1                  1996
# 2     2                   268
# 3     3                    34
# 4     4                     4

## COMMENT:
## lots of duplicates (and some triplicates, quadruplets). 
## What to do with these? Could filter to a single row per species
## (see next chunk of code)
## WILL ALSO WANT TO CHECK THAT MULTIPLE SYNONYMS AREN'T BEING USED
## FOR SPECIES WITH ONLY ONE ROW

## filter the complete species list to distinct species
sp_data_all <- sp_data_all %>% 
  group_by(sourceTaxonName) %>% 
  slice(1)

## import list of species found at Appledore
# sp_zone <- read_csv("data/species_list_by_zone.csv")
## use the updated version of this list that includes the synonymized/
## searched species names
sp_zone <- read_sheet("https://docs.google.com/spreadsheets/d/1L92amK19NyS2KrAj7_kbQM0Gg_p9MduL08h2FyYEVHU/", 
                      sheet = "species_list_by_zone") %>% 
  mutate(DB_name = ifelse(is.na(DB_name), organism, DB_name))


## add column identifying Appledore Island species in full species list
## (i.e. left join sp_zone to sp_data_all), and identify where those species
## are found (intertidal, subtidal, or both?)

sp_data_all <- sp_data_all %>% 
  mutate(appledore_sp = as.factor(if_else(
    sourceTaxonName %in% sp_zone$DB_name, 1, 0))) 

sp_data_all %>% 
  group_by(appledore_sp) %>% 
  summarise(n_sp = n_distinct(sourceTaxonName)) ## [1] 156 (up from 121)
## Tianna thinks this looks good

## COMMENT:
## only 121 and of the 144 species in the Island list are currently 
## included in the species list for interaction data... why could this be? 
## Maybe because some aggregate/common names in our island list 
## (e.g. Amphipoda, Barnacle, etc.) are being included in the interactions
## data as set of relevant species (e.g., Balanus balanoides, etc.)

sp_data_all <- sp_data_all %>% 
  left_join(., sp_zone, by = c("sourceTaxonName" = "DB_name"))


## which species in the island list aren't in the interactions DB lists?
sp_data_all %>%
  filter(appledore_sp == 1)
sp_zone %>% 
  filter(!DB_name %in% unique(
    sp_data_all$sourceTaxonName[
      sp_data_all$appledore_sp == 1])) %>% 
  knitr::kable()

## COMMENT:
## currently 23 species included in the island species list that haven't
## been matched to species in our interactions databases
## some of these are because the island list includes generic/aggregate
## names (e.g., "barnacle" instead of "Balanoides sp." or 
## "myoxycephalus" instead of "Myoxycephalus scorpius"), others are 
## because of synonyms (e.g., we have "TTectura testinalis" in our list, but
## the accepted name is "Testudinalia testudinalis")


# combine all interaction sheets ------------------------------------------

## bind all datasets together
interactions <- bind_rows(sub_dat, int_dat)
write_csv(interactions, "data/intertidal_subtidal_interactions_DB.csv")

## add a column indicating zone for the focal species 
## (intertidal, subtidal, both) -- will need to resolve species names
## in this list first before joining!!
# interactions <- interactions %>% 
#   left_join(., sp_zone, by = c("SourceTaxonID" == "DB_name"))

## COMMENT:
## come back to this later after we've pulled the interaction data
## from Globi

## pull the list of unique species from the combined data
interactors <- interactions %>% 
  select(sourceTaxonName, targetTaxonName) %>% 
  pivot_longer(cols = everything(), 
               names_to = "source_target", values_to = "species") %>% 
  group_by(species) %>%
  slice(1) %>% 
  ungroup()

## assign all unique species to corresponding 'organism' in the observational
## data (see 'sp_zone' table) -- need to do this because many taxa are
## observed/documented at genus level but interactions were searched at species level
##
##

## once we have this list of species, we can use it to search for all 
## relevant interactions on globi

##
##
##
##

# get globi data (using full subtidal-intertidal interactions DB list)
# sp_data_ne_all <- sp_data_all %>%
#   filter(is.na(`Added as new agents`),
#          `Taxonomic Level`=="Species")

## COMMENT:
## This list currently include 347 species (compared to the 144 taxa
## in our island list). What's up?
## It's like that many of the higher level taxa have been disaggregated
## to the species level in our interaction species list. We will need
## create a cross table that links these disaggregated species
## back to the organism as identified in the sp_zone table


# get globi data
# sp_data_ne_subtidal <- sp_data %>%
#   filter(is.na(`Added as new agents`),
#          `Taxonomic Level`=="Species")


# get worms synonyms for getting globi data
get_valid_name <- function(.x){
  print(.x)
  
  id <- get_id(.x)
  if(is.na(id)){return(.x)}
  
  wm_record(id) %>%
    pull(valid_name) %>%
    `[`(1)
}

#deprecated
get_ids <- function(.x){
  id <- try(wm_name2id(.x))
  if(class(id) == "try-error"){return(NA)}
  id
}


get_id <- function(.x){
  
  rec <- try(wm_records_name(.x))
  if(class(rec)[1] == "try-error"){return(NA)}
  
  rec %>%
    filter(status=="accepted") %>%
    pull(AphiaID) %>%
    `[`(1)
}

get_synonyms <- function(.x){
  print(.x)
  if(is.na(.x)) return(NA)
  
  syn <- try(wm_synonyms(.x))
  if(class(syn) == "try-error"){return(.x)}
  
  syn %>% pull(scientificname)
  
}
# valid_names <- map_chr(sp_data_ne_subtidal$sourceTaxonName,
#                        get_valid_name)
# 
# ids <- map_int(valid_names, get_ids) %>%
#   na.omit
# synonyms <- map(ids, 
#                 ~wm_synonyms(.x) %>% pull(scientificname)) %>%
#   unlist()

# sp_data_ne_subtidal_2 <- sp_data_ne_subtidal %>%
#   mutate(valid_names = map_chr(sourceTaxonName, get_valid_name),
#          ids = map_int(valid_names, get_id),
#          synonyms = map(ids, ~get_synonyms(.x)) )


sp_zone2 <- sp_zone %>% 
  mutate(valid_names = map_chr(DB_name, get_valid_name), 
         ids = map_int(valid_names, get_id),
         synonyms = map(ids, ~ get_synonyms(.x)))

## matches not found for
## "Barnacle sp."
## "Blady Ulvoid"
## "Diatom Tube Mat"
## "Encrusting coralline"
## "Filamentous Green"
## "Obelia spp."
## "Red Algal Turf"
## "Tubular Ulvoid"
## "UNID Juv Laminariales"
## "UNID Juv Laminariales"
## "Unidentified Erect Coralline"
## "Unidentified Filamentous Red"
## "Unidentified Red Blade"

#save it for the future!
# saveRDS(sp_data_ne_subtidal_2, "data/sp_data_ne_subtidal_worms_resolves.rds")
#sp_data_ne_subtidal_2 <- readRDS("data/sp_data_ne_subtidal_worms_resolves.rds")
saveRDS(sp_zone2, "data/species_list_by_zone2.RDS")

#make a vector of species to search on
# sp_to_search <- c(sp_data_ne_subtidal_2$sourceTaxonName,
#                   sp_data_ne_subtidal_2$valid_names#,
#                   # sp_data_ne_subtidal_2$synonyms%>%unlist(),
#                   # sp_data_ne_subtidal_2$synonyms %>%
#                   #   unlist() %>%
#                   #   stringr::str_remove(" var\\..*") %>%
#                   #   stringr::str_remove(" f\\..*")
# ) %>%
#   unique()

sp_to_search <- c(sp_zone2$DB_name, sp_zone2$valid_names) %>% 
  unique()
length(sp_to_search) ## [1] 160

#function to be kind to the API
get_interactions_sleepy <- function(.x, pause = 1, ...){
  Sys.sleep(pause)
  get_interactions(.x, ...)
}

#get interactions for these species from GLOBI
globi_ints <- map_df(sp_to_search, get_interactions_sleepy, 
                     interaction.type = c("eats", "eatenBy", "preysOn", "preyedUponBy"),
                     pause = 0) %>%
  rename(sourceTaxonName = source_taxon_name,
         targetTaxonName = target_taxon_name,
         interactionTypeName = interaction_type)  %>%
  filter(interactionTypeName %in% c("eats", "eatenBy", "preysOn", "preyedUponBy")) %>%
  mutate(interactionTypeName = case_when(
    interactionTypeName == "eatenBy" ~ "eaten by",
    interactionTypeName == "preysOn" ~ "preys on",
    interactionTypeName == "preyedUponBy" ~ "preyed upon by",
    interactionTypeName == "eats" ~ "eats"
  ))

#should this be piped in?
#select(sourceTaxonName, targetTaxonName, interactionTypeName)

#resolve taxonomy on interaction sheet
write_csv(globi_ints, "data/globi_ints.csv")


# 2_join_int_data.R -------------------------------------------------------

#species list
# sp_data_ne_subtidal <- readRDS("data/sp_data_ne_subtidal_worms_resolves.rds")
sp_zone2 <- readRDS("data/species_list_by_zone2.RDS")

# fix misspellings (i.e., add them back in)
# sp_data_ne_subtidal <- sp_data_ne_subtidal%>%
#   mutate(sourceTaxonName = ifelse(!is.na(sourceTaxonId), sourceTaxonId, sourceTaxonName))

#load entered data, filter to what we have, and fix taxonomy with valid names
# interactions <- ... ## need to change the chunk below to work with the 
## combined interactions databases (called "interactions" above)
# int_dat <- read_csv("./data/entered_int_data.csv") %>%
#   filter(sourceTaxonName %in% sp_data_ne_subtidal$sourceTaxonName) %>%
#   filter(targetTaxonName %in% sp_data_ne_subtidal$sourceTaxonName) %>%
#   dplyr::select(sourceTaxonName, targetTaxonName, interactionTypeName) %>%
#   filter(interactionTypeName %in% c("eats", "eaten by", "preys on", "preyed upon by")) 
# 
# int_dat <- int_dat %>%
#   left_join(sp_data_ne_subtidal %>%
#               select(sourceTaxonName, sourceTaxonName_valid = valid_names))%>%
#   left_join(sp_data_ne_subtidal %>%
#               select(targetTaxonName = sourceTaxonName, targetTaxonName_valid = valid_names)) %>%
#   mutate(sourceTaxonName = sourceTaxonName_valid,
#          targetTaxonName = targetTaxonName_valid) %>%
#   select(-sourceTaxonName_valid, -targetTaxonName_valid)

#load GLobi data
globi_ints2 <- read_csv("data/globi_ints.csv") %>%
  filter(sourceTaxonName %in% sp_zone2$DB_name) %>%
  filter(targetTaxonName %in% sp_zone2$DB_name)

table(globi_ints2$interactionTypeName)
## From some date before 13 July 2022
# eaten by           eats preyed upon by       preys on 
#      410            417            178            170 

## From 13 July 2022
# eaten by           eats preyed upon by       preys on 
#      398            401            178            170 
## why has the total number of interactions decreased?????
names(globi_ints2)

globi_ints2 <- globi_ints2 %>% 
  # mutate(sourceTaxonName2 = sp_zone2$organism[sp_zone2$valid_names == .$sourceTaxonName]) %>% 
  # mutate(sourceTaxonName2 = ifelse(sourceTaxonName == sp_zone2$valid_names, 
  #                                  sp_zone2$organism, NA), 
  #        targetTaxonName2 = ifelse(targetTaxonName == sp_zone2$valid_names, 
  #                                  sp_zone2$organism, NA)) %>% 
  left_join(x = ., y = sp_zone2[c("organism", "valid_names")], 
            by = c("sourceTaxonName" = "valid_names")) %>% 
  rename(sourceTaxonName2 = organism) %>% 
  left_join(x = ., y = sp_zone2[c("organism", "valid_names")], 
            by = c("targetTaxonName" = "valid_names")) %>% 
  rename(targetTaxonName2 = organism) %>% 
  relocate(., c("sourceTaxonName2", "targetTaxonName2"), 
           .before = 1) %>% 
  left_join(x., y = sp_zone2[c("organism", "sp_zone")], 
            by = c("sourceTaxonName2" = "organism")) %>% 
  rename(sourceTaxon_zone = sp_zone) %>% 
  relocate(., "sourceTaxon_zone", .after = targetTaxonName2) %>% 
  left_join(x., y = sp_zone2[c("organism", "sp_zone")], 
            by = c("targetTaxonName2" = "organism")) %>% 
  rename(targetTaxon_zone = sp_zone) %>% 
  relocate(., "targetTaxon_zone", .after = sourceTaxon_zone) 


# write_csv(interactions_comp, "./data/cleaned_joined_int_data.csv")
interactions <- read_csv("data/intertidal_subtidal_interactions_DB.csv")

interactions2 <- interactions %>% 
  # left_join(x = ., y = sp_zone2[c("organism", "valid_names")], 
  #           by = c("sourceTaxonName" = "valid_names")) %>% 
  # rename(sourceTaxonName2 = organism) %>% 
  # left_join(x = ., y = sp_zone2[c("organism", "valid_names")], 
  #           by = c("targetTaxonName" = "valid_names")) %>% 
  # rename(targetTaxonName2 = organism) %>% 
  # relocate(., c("sourceTaxonName2", "targetTaxonName2"), 
  #          .before = 1) %>% 
  left_join(x = ., y = sp_zone2[c("organism", "valid_names")], 
            by = c("sourceTaxonName" = "valid_names")) %>% 
  rename(sourceTaxonName2 = organism) %>% 
  left_join(x = ., y = sp_zone2[c("organism", "valid_names")], 
            by = c("targetTaxonName" = "valid_names")) %>% 
  rename(targetTaxonName2 = organism) %>% 
  relocate(., c("sourceTaxonName2", "targetTaxonName2"), 
           .before = 1) %>% 
  # relocate(sourceTaxonName, .after = sourceTaxonName2) %>% 
  # relocate(targetTaxonName, .after = targetTaxonName2)
  left_join(x., y = sp_zone2[c("organism", "sp_zone")], 
            by = c("sourceTaxonName2" = "organism")) %>% 
  rename(sourceTaxon_zone = sp_zone) %>% 
  relocate(., "sourceTaxon_zone", .after = targetTaxonName2) %>% 
  left_join(x., y = sp_zone2[c("organism", "sp_zone")], 
            by = c("targetTaxonName2" = "organism")) %>% 
  rename(targetTaxon_zone = sp_zone) %>% 
  relocate(., "targetTaxon_zone", .after = sourceTaxon_zone) 

length(which(is.na(interactions2$sourceTaxonName2))) ## [1] 3409 ## [1] 4102
length(which(is.na(interactions2$targetTaxonName2))) ## [1] 4736 ## [1] 5903

## WHAT'S GOING ON HERE????? revisit with Jarrett

## drop the NAs for now
interactions2 <- interactions2 %>% 
  drop_na(sourceTaxonName2, targetTaxonName2)


# combined network --------------------------------------------------------

## read in required data
## sp_zone2
## globi_ints2
## interactions2

## what does the input data look like?
nrow(globi_ints2) ## [1] 1557 ## [1] 1477
nrow(interactions2) ## [1] 656 ## [1] 778

## combine Globi and lit search data
interactions_comb <- bind_rows(interactions2, globi_ints2)  %>%
  group_by(sourceTaxonName2, targetTaxonName2, interactionTypeName) %>%
  slice(1L)

nrow(interactions_comb) ## [1] 866 ## [1] 892
View(interactions_comb)

## need to correct one interaction that is currently NA for interaction type
interactions_comb <- interactions_comb %>% 
  ## correct one species interaction type that is currently NA but
  ## should actually be "preys on" 
  ## (it's actually auto-cannabilism by Hiatella arctica) 
  mutate(interactionTypeName = ifelse(is.na(interactionTypeName), 
                                      "preys on", 
                                      interactionTypeName))
length(which(is.na(interactions_comb$interactionTypeName))) ## [1] 0

## save combined dataset
write_csv(interactions_comb, "data/combined_litsearch_globi_interactions.csv")

## COMMENT:
## we still need to adjust the direction of these interactions and 
## remove any potential duplicates (i.e., we want all interactions to be 
## in one direction, from the resource/prey to the consumer/predator)

## read in combined dataset
# interactions_comb <- read_csv(
#   "data/combined_litsearch_globi_interactions.csv")

## split data into two subsets:
unique(interactions_comb$interactionTypeName)
# [1] "eaten by"                      
# [2] "preyed upon by"                
# [3] "has epiphyte"                  
# [4] "eats"                          
# [5] "preys on"                      
# [6] "mutualistically interacts with"
# [7] "epiphyte of"                   
# [8] "parasite of"                   
# [9] "epibiont of"                   
# [10] "competes with"                 
# [11] "visits"  

## subset 1: consuming (don't need to modify)
interactions_comb_eats <- interactions_comb %>% 
  ## filter to "consuming" interactions
  filter(interactionTypeName %in% c("eats", 
                                    "preys on", 
                                    "visits", 
                                    "epiphyte of", 
                                    "parasite of", 
                                    "epibiont of")) %>% 
  ## move the interactionTypeName column to a more convenient spot
  relocate(interactionTypeName, .after = targetTaxon_zone)
nrow(interactions_comb_eats) ## [1] 468 ## [1] 502

## subset 2: consumed (need to modify)
interactions_comb_eaten <- interactions_comb %>% 
  ## filter to "consumed" interactions
  filter(interactionTypeName %in% c("eaten by", 
                                    "preyed upon by", 
                                    "has epiphyte"))
nrow(interactions_comb_eaten) ## [1] 395 ## [1] 386

## COMMENT:
## there are 3 interactions that aren't captured by these subsets because
## they are not trophic interactions (and shouldn't be included in our
## trophic/food-web analysis). These are:
interactions_comb %>% 
  ungroup() %>% 
  filter(interactionTypeName %in% c("competes with", 
                                    "mutualistically interacts with")) %>% 
  select(sourceTaxonName2, targetTaxonName2, interactionTypeName)
#   sourceTaxonName2 targetTaxonName2 interactionTypeName        
# 1 Cancer borealis  Chondrus crispus mutualistically interacts …
# 2 Cancer irroratus Chondrus crispus mutualistically interacts …
# 3 Hiatella arctica Mytilus edulis   competes with

## now, flip the direction of the "eaten" interactions, so that the
## sourceTaxon becomes the targetTaxon and vise versa
interactions_comb_eaten2 <- interactions_comb_eaten %>% 
  ## first need to give the relevant columns new names
  rename(sourceTaxonName3  = targetTaxonName2, 
         targetTaxonName3  = sourceTaxonName2, 
         sourceTaxon_zone2 = targetTaxon_zone, 
         targetTaxon_zone2 = sourceTaxon_zone) %>% 
  ## then replace with the correct column names 
  # (so we can re-join the two subsets) 
  rename(sourceTaxonName2 = sourceTaxonName3, 
         targetTaxonName2 = targetTaxonName3, 
         sourceTaxon_zone = sourceTaxon_zone2, 
         targetTaxon_zone = targetTaxon_zone2) %>% 
  ## and reorder the columns to match the order in the other subset
  select(sourceTaxonName2, targetTaxonName2, 
         sourceTaxon_zone, targetTaxon_zone, 
         interactionTypeName, 
         `Paper ID`:sourceTaxonName, 
         targetTaxonName:study_source_citation) %>% 
  ## redefine the interaction types to reflect the change in direction
  mutate(interactionTypeName = 
           case_when(interactionTypeName == "eaten by" ~ "eats", 
                     interactionTypeName == "preyed upon by" ~ "preys on", 
                     interactionTypeName == "has epiphyte" ~ "epiphyte of"))

## join the two subsets back together and remove duplicates
interactions_comb2 <- bind_rows(interactions_comb_eats, 
                                interactions_comb_eaten2) %>% 
  group_by(sourceTaxonName2, targetTaxonName2, interactionTypeName) %>%
  slice(1L)

nrow(interactions_comb2) ## [1] 550 -- this seems low, but okay!!


## write out the cleaned combined dataset
write_csv(interactions_comb2, 
          "data/combined_litsearch_globi_interactions2.csv")
