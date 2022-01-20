## Jarrett's scripts for food web analysis
## from: https://github.com/jebyrnes/keen_one_foodwebs/tree/main/scripts


## Communities in {igraph}
## https://users.dimi.uniud.it/~massimo.franceschet/R/communities.html
## Introducing {tidygraph}
## https://www.data-imaginist.com/2017/introducing-tidygraph/

# 1_get_master_web.R ------------------------------------------------------

#'----------------------------
# Load interaction data
#'----------------------------

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
int_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1f4a8mYrov0DiRBOoP1lbuEh_kxhQP6t7Ay6fH5q5gME/", 
                      sheet = "Working Interaction Data")

## import rest of subtidal data (from working group)
int_dat_2 <- read_sheet("https://docs.google.com/spreadsheets/d/16PwO_TI_YnSktYBolK6MPuuvyhEu7xZy7j0jplmz5sg/edit#gid=1883520352",
                        sheet = "Interaction Data")

## bind the subtidal data together
int_dat <- bind_rows(int_dat, int_dat_2) %>%
  ## manipulate variables
  mutate(`Ref ID` = map_chr(`Ref ID`, ~ifelse(is.null(.x), "", .x)),
         `Paper ID` = map_chr(`Paper ID`, ~ifelse(is.null(.x), "", .x)),
         observationDateTime = map_chr(observationDateTime, ~ifelse(is.null(.x), "", .x)))

## export subtidal data
<<<<<<< HEAD

=======
>>>>>>> 55492772a27ae4e89372ff82a6ecf85d35fa9775
write_csv(int_dat, "./data/entered_int_data.csv")

# pull subtidal species list from interactions database
sp_data <- read_sheet("https://docs.google.com/spreadsheets/d/1f4a8mYrov0DiRBOoP1lbuEh_kxhQP6t7Ay6fH5q5gME/", 
                      sheet = "Species List")

sp_data2 <- read_sheet("https://docs.google.com/spreadsheets/d/16PwO_TI_YnSktYBolK6MPuuvyhEu7xZy7j0jplmz5sg/edit#gid=19019573", 
                       sheet = "Species List") %>%
  mutate(`Done = 1` = map_dbl(`Done = 1`, ~ifelse(is.null(.x), NA, .x) %>% as.numeric)) %>% 
  mutate(`Added as new agents` = unlist(`Added as new agents`))

sp_data <- bind_rows(sp_data, sp_data2)

write_csv(sp_data, "./data/entered_species_data.csv")
<<<<<<< HEAD
=======
int_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1f4a8mYrov0DiRBOoP1lbuEh_kxhQP6t7Ay6fH5q5gME/", 
                      sheet = "Working Interaction Data")
>>>>>>> 55492772a27ae4e89372ff82a6ecf85d35fa9775


# intertidal data ---------------------------------------------------------

## import intertidal interaction data from working group data
intertidal_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1ELUVTUnV1fUMc3nBC6EFgCG4jwhcWCfoewATIK-BiLw/", sheet = "Interaction Data")

## import other intertidal interaction data from Laura Dee et al.
##
##

## reformat tables to a consistent format
##
##

<<<<<<< HEAD
## pull intertidal species list 
sp_data_intertidal <- read_sheet("https://docs.google.com/spreadsheets/d/1ELUVTUnV1fUMc3nBC6EFgCG4jwhcWCfoewATIK-BiLw/", 
                      sheet = "Species List") %>% 
  mutate(`Done = 1` = map_dbl(`Done = 1`, ~ifelse(is.null(.x), NA, .x) %>% as.numeric)) %>% 
  mutate(`Added as new agents` = unlist(`Added as new agents`))

## pull intertidal species list from Laura Dee's database
sp_data2 <- read_sheet("<need to get data>", 
                       sheet = "Species List") 

sp_data <- bind_rows(sp_data, sp_data2)

write_csv(sp_data, "./data/entered_species_data.csv")


## bind all species lists together
## (need to select common columns first, and do some name formatting)
sp_data_simple <- sp_data %>% 
  select(sourceTaxonName, `Taxonomic Level`, `EOL ID`, 
         `WORMS ID`, `Common Name`, `Done = 1`, `Added as new agents`, 
         `Aggregated taxa`, `Notes`, `search term`, `who is searching?`)

sp_data_intertidal_simple <- sp_data_intertidal %>% 
  select(sourceTaxonName, `Taxonomic Level`, `EOL ID`, 
         `WORMS ID`, `Common Name`, `Done = 1`, `Added as new agents`, 
         `Aggregated taxa`, `Notes`, `Search term`, `Who is searching?`) %>% 
  rename(`who is searching?` = `Who is searching?`, 
         `search term` = `Search term`)

names(sp_data_simple) == names(sp_data_intertidal_simple)

sp_data_all <- bind_rows(sp_data_simple, sp_data_intertidal_simple)

## how many unique species are included in this list?
n_distinct(sp_data_all$sourceTaxonName) # [1] 2293

## how many are duplicates? LOTS
sp_data_all %>% 
  group_by(sourceTaxonName) %>% 
  summarise(n = n()) %>% 
  group_by(n) %>% 
  summarise(n_species_with_x_rows = n())

#       n n_species_with_x_rows
# 1     1                  1992
# 2     2                   265
# 3     3                    33
# 4     4                     3

## COMMENT:
## lots of duplicates (and some triplicates, quadruplets). 
## What to do with these? Could filter to a single row per species
## (see next chunk of code)
## WILL ALSO WANT TO CHECK THAT MULTIPLE SYNONYMS AREN'T BEING USED
## FOR SPECIES WITH ONLY ONE ROW

## filter the complete species list to distinct species
sp_data_all %>% 
  group_by(sourceTaxonName) %>% 
  slice(1) %>% 


## import list of species found at Appledore
sp_zone <- read_csv("data/species_list_by_zone.csv")

## add column identifying Appledore Island species in full species list
## (i.e. left join sp_zone to sp_data_all), and identify where those species
## are found (intertidal, subtidal, or both?)

sp_data_all %>% 
  mutate(appledore_sp = as.factor(if_else(
    sourceTaxonName %in% sp_zone$organism, 1, 0))) %>% 
  group_by(appledore_sp) %>% 
  summarise(n_sp = n_distinct(sourceTaxonName))
  
## COMMENT:
## only 121 and of the 144 species in the Island list are currently 
## included in the species list for interaction data... why could this be? 
## Maybe because some aggregate/common names in our island list 
## (e.g. Amphipoda, Barnacle, etc.) are being included in the interactions
## data as set of relevant species (e.g., Balanus balanoides, etc.)

sp_data_all <- sp_data_all %>% 
  mutate(appledore_sp = as.factor(if_else(sourceTaxonName %in% 
                                            sp_zone$organism, 1, 0))) %>% 
  left_join(., sp_zone, by = c("sourceTaxonName" = "organism"))


=======
>>>>>>> 55492772a27ae4e89372ff82a6ecf85d35fa9775
# combine all interaction sheets ------------------------------------------

## bind all datasets together
interactions <- bind_rows(int_data, intertidal_dat, ...)

## add a column indicating zone for the focal species 
## (intertidal, subtidal, both) -- will need to resolve species names
## in this list first before joining!!
interactions <- interactions %>% 
  left_join(., sp_zone, by = c("SourceTaxonID" == "organism"))


## pull the list of unique species from the combined data
## 
##

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

<<<<<<< HEAD
# get globi data (using full subtidal-intertidal interactions DB list)
sp_data_ne_all <- sp_data_all %>%
  filter(is.na(`Added as new agents`),
         `Taxonomic Level`=="Species")

## COMMENT:
## This list currently include 347 species (compared to the 144 taxa
## in our island list). What's up?
## It's like that many of the higher level taxa have been disaggregated
## to the species level in our interaction species list. We will need
## create a cross table that links these disaggregated species
## back to the organism as identified in the sp_zone table

=======
# get globi data
sp_data_ne_subtidal <- sp_data %>%
  filter(is.na(`Added as new agents`),
         `Taxonomic Level`=="Species")

>>>>>>> 55492772a27ae4e89372ff82a6ecf85d35fa9775
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

sp_data_ne_subtidal_2 <- sp_data_ne_subtidal %>%
  mutate(valid_names = map_chr(sourceTaxonName, get_valid_name),
         ids = map_int(valid_names, get_id),
         synonyms = map(ids, ~get_synonyms(.x)) )

#save it for the future!
saveRDS(sp_data_ne_subtidal_2, "data/sp_data_ne_subtidal_worms_resolves.rds")
#sp_data_ne_subtidal_2 <- readRDS("data/sp_data_ne_subtidal_worms_resolves.rds")

#make a vector of species to search on
sp_to_search <- c(sp_data_ne_subtidal_2$sourceTaxonName,
                  sp_data_ne_subtidal_2$valid_names#,
                  # sp_data_ne_subtidal_2$synonyms%>%unlist(),
                  # sp_data_ne_subtidal_2$synonyms %>%
                  #   unlist() %>%
                  #   stringr::str_remove(" var\\..*") %>%
                  #   stringr::str_remove(" f\\..*")
) %>%
  unique()

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

write_csv(globi_ints, "./data/globi_ints_unresolved.csv")

#resolve taxonomy on interaction sheet
write_csv(globi_ints, "./data/globi_ints.csv")


# 2_join_int_data.R -------------------------------------------------------



library(readr)
library(dplyr)

#species list
sp_data_ne_subtidal <- readRDS("data/sp_data_ne_subtidal_worms_resolves.rds")

# fix misspellings (i.e., add them back in)
sp_data_ne_subtidal <- sp_data_ne_subtidal%>%
  mutate(sourceTaxonName = ifelse(!is.na(sourceTaxonId), sourceTaxonId, sourceTaxonName))


#load entered data, filter to what we have, and fix taxonomy with valid names
int_dat <- read_csv("./data/entered_int_data.csv") %>%
  filter(sourceTaxonName %in% sp_data_ne_subtidal$sourceTaxonName) %>%
  filter(targetTaxonName %in% sp_data_ne_subtidal$sourceTaxonName) %>%
  dplyr::select(sourceTaxonName, targetTaxonName, interactionTypeName) %>%
  filter(interactionTypeName %in% c("eats", "eaten by", "preys on", "preyed upon by")) 

int_dat <- int_dat %>%
  left_join(sp_data_ne_subtidal %>%
              select(sourceTaxonName, sourceTaxonName_valid = valid_names))%>%
  left_join(sp_data_ne_subtidal %>%
              select(targetTaxonName = sourceTaxonName, targetTaxonName_valid = valid_names)) %>%
  mutate(sourceTaxonName = sourceTaxonName_valid,
         targetTaxonName = targetTaxonName_valid) %>%
  select(-sourceTaxonName_valid, -targetTaxonName_valid)

#load GLobi data
globi_ints <- read_csv("./data/globi_ints.csv")%>%
  filter(sourceTaxonName %in% sp_data_ne_subtidal$sourceTaxonName) %>%
  filter(targetTaxonName %in% sp_data_ne_subtidal$sourceTaxonName)  %>%
  mutate(interactionTypeName = case_when(
    interactionTypeName == "eatenBy" ~ "eaten by",
    interactionTypeName == "preysOn" ~ "preys on",
    interactionTypeName == "preyedUponBy" ~ "preyed upon by",
    interactionTypeName == "eats" ~ "eats"
  ))


int_dat <- bind_rows(int_dat, globi_ints)  %>%
  group_by(sourceTaxonName, targetTaxonName, interactionTypeName) %>%
  slice(1L)

write_csv(int_dat, "./data/cleaned_joined_int_data.csv")


# 3_get_species_by_site.R -------------------------------------------------

#'---------------------------------------
#' Get Species at each site at each year
#'---------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
keen_dir <- "~/Dropbox (Byrnes Lab)/Byrnes Lab Shared Folder/KEEN Data Processing/Monitoring/cleaned_data/"

# load in quads, swath, fish, pointcount
quads <- read_csv(glue("{keen_dir}/keen_quads.csv"))

get_sp_df_transect <- function(a_df){
  a_df %>% 
    filter(GROUP %in% c("Algae","Fish","Invertebrate")) %>%
    group_by(SITE, YEAR, TRANSECT,
             GROUP,SPECIES) %>%
    slice(1L) %>%
    ungroup() %>%
    select(SITE, YEAR, TRANSECT, GROUP:SPECIES) %>%
    select(-SIZE)
}

files <- paste0(keen_dir, 
                c("keen_quads.csv",
                  "keen_swath.csv",
                  "keen_fish.csv",
                  "keen_cover.csv"
                ))

sp_by_transect <- map(files, read_csv) %>%
  map_df(get_sp_df_transect) %>%
  group_by(SITE, YEAR, TRANSECT,
           GROUP,SPECIES) %>%
  slice(1L) %>%
  ungroup()

# load species list from web

sp_data <- readRDS("data/sp_data_ne_subtidal_worms_resolves.rds")


# make some changes
sp_by_transect <- sp_by_transect %>%
  mutate(SPECIES = case_when(
    SPECIES=="Boreochiton ruber" ~ "Ischnochiton ruber",
    SPECIES=="Crisularia turrita" ~ "Bugula turrita",
    SPECIES=="Dasysiphonia japonica" ~ "Heterosiphonia japonica",
    SPECIES=="Halichondria (Halichondria) panicea" ~ "Halichondria  panicea",
    SPECIES=="Euspira heros" ~ "Lunatia heros",
    SPECIES=="Schizomavella auriculata biaviculifera" ~ "Schizomavella auriculata",
    SPECIES=="Metridium senile pallidus" ~ "Metridium senile",
    SPECIES=="Pachycerianthus borealis" ~ "Cerianthus borealis",
    SPECIES=="Patinella verrucaria" ~ "Lichenopora verrucaria",
    SPECIES=="Porphyra" ~ "Porphyra spp.",
    SPECIES=="Lithophyllum" ~ "Lithophyllum spp.",
    SPECIES=="Sertularia" ~ "Sertularia sp.",
    SPECIES=="Testudinalia testudinalis" ~ "Tectura testudinalis",
    SPECIES=="Titanoderma" ~ "Titanoderma spp.",
    SPECIES=="Ulvaria" ~ "Ulvaria",
    SPECIES=="Sertularia" ~ "Sertularia sp.",
    SPECIES=="Sabella" ~ "Sabella sp.",
    SPECIES=="Zostera (Zostera) marina" ~ "Zostera marina",
    TRUE ~ SPECIES
  ))

#check
unique(sp_by_transect$SPECIES)[!(unique(sp_by_transect$SPECIES) %in% sp_data$valid_names)] %>% sort()

# expand aggregated species in data
ag_species <- sp_data$`Aggregated taxa` %>% na.omit() %>% unique()

add_expanded_sp <- function(adf){
  #do we need to expand anything?
  if(sum(adf$SPECIES %in% unique(ag_species))==0)
    return(adf$SPECIES)
  
  #make a df of taxa to add
  sp_to_add <- sp_data %>%
    filter(`Aggregated taxa` %in% adf$SPECIES) %>% 
    pull(valid_names) 
  
  c(adf$SPECIES, sp_to_add)
  
}

sp_by_transect_only <- sp_by_transect %>%
  group_by(SITE, YEAR, TRANSECT) %>%
  nest() %>%
  mutate(SPECIES = map(data, add_expanded_sp)) %>%
  select(-data) %>% #don't need
  unnest(SPECIES) %>%
  #get rid of dups
  group_by(SITE, YEAR, TRANSECT, SPECIES) %>%
  slice(1L) %>%
  ungroup()

# write species list by transect 
write_csv(sp_by_transect_only, "data/species_list_by_site_transect_year.csv")


# 4_site_year_web.R -------------------------------------------------------

#'---------------------------------------
#' Get Food Web at each site at each year
#' and calculate properties
#'---------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(glue)
#---
library(tidygraph)
library(ggraph)
library(NetIndices)

#functions
source("scripts/make_web_functions.R")

#get species
sp_by_transect_only<- read_csv("data/species_list_by_site_transect_year.csv")

# get interactions
foodweb <- read_csv("data/cleaned_joined_int_data.csv") %>%
  filter(interactionTypeName %in%
           c("preys on", "preyed upon by",
             "eats", "is eaten by"))



#

# Helpful functions from my ole' GCB paper
consumer_degrees<-function(a.matrix){ 
  cd<-0
  a.matrix<-as.matrix(a.matrix)
  if(!is.na(a.matrix[1])){
    if (length(a.matrix[1,])==1) a.matrix<-t(a.matrix)
    cd<-rowSums(a.matrix)[which(rowSums(a.matrix)!=0)]
  }
  return(cd)
}

mean_degrees<-function(a.matrix){ mean(consumer_degrees(a.matrix))}

sd_degrees<-function(a.matrix){ sd(consumer_degrees(a.matrix))}

degree_info <- function(a_matrix){
  cd <- consumer_degrees(a_matrix)
  tibble(
    avg_cons_degrees = mean(cd),
    sd_cons_degrees = sd(cd)
    
  )
}


###### OK, let's do things to data
# Add web info to data
xy_tab <- NULL

add_web_info <- . %>%
  nest %>%
  mutate(interactions = map(data, make_one_web),
         from_to = map(interactions, make_from_to),
         graph = map(from_to, ~tbl_graph(edges = .x)),
         adj_mat = map(graph, ~igraph::as_adjacency_matrix(.x) %>%
                         as.matrix)) %>%
  #get more detailed info
  mutate(graph = map2(graph, adj_mat, add_info_to_web),
         plot_web = map(graph, make_fw_ggraphs)) %>%
  ungroup()

add_web_metrics <- . %>%
  mutate(map_df(adj_mat, ~GenInd(Tij = .x) %>% as_tibble),
         map_df(adj_mat, ~PathInd(Tij = .x)%>% as_tibble),
         map_df(adj_mat, degree_info),
         diameter = map_dbl(graph, igraph::diameter)
  )


# Get the whole enchilada

web_all <- sp_by_transect_only %>%
  group_by(SPECIES) %>%
  slice(1L) %>%
  ungroup() %>%
  group_by(all=1) %>%
  add_web_info %>%
  add_web_metrics


xy_tab <- web_all$graph[[1]] %>% 
  activate("nodes") %>%
  as_tibble() %>% 
  dplyr::select(name, x, y)

web_transect_year_df <- sp_by_transect_only %>%
  group_by(SITE, TRANSECT, YEAR) %>%
  add_web_info %>%
  add_web_metrics

web_site_year_df <- sp_by_transect_only %>%
  group_by(SITE, YEAR, SPECIES) %>%
  slice(1L) %>%
  ungroup %>%
  group_by(SITE, YEAR) %>%
  add_web_info %>%
  add_web_metrics

web_site <- sp_by_transect_only %>%
  group_by(SITE, SPECIES) %>%
  slice(1L) %>%
  ungroup %>%
  group_by(SITE) %>%
  add_web_info %>%
  add_web_metrics

saveRDS(web_all, "data/web_all.rds")
saveRDS(web_site, "data/web_site.rds")
saveRDS(web_site_year_df, "data/web_site_year_df.rds")
saveRDS(web_transect_year_df, "data/web_transect_year_df.rds")


#some plot output
ggsave("figures/web_all.jpg", plot = web_all$plot_web[[1]])

walk2(web_site$SITE, web_site$plot_web,
      ~ggsave(glue("figures/{.x}.jpg"), plot=.y))

pwalk(list(web_site_year_df$SITE,
           web_site_year_df$YEAR,
           web_site_year_df$plot_web),
      ~ggsave(glue("figures/site_year/{..1}_{..2}.jpg"), plot=..3))


# 5_merge_with_local_data.R -----------------------------------------------

#'---------------------------------------
#' Merge FW and Site-level data
#'---------------------------------------

library(dplyr)
library(readr)
library(tidyr)

# Load and process data ####
# load site-year-level data
fw <- readRDS("data/web_transect_year_df.rds") %>%
  filter(SITE != "SW Appledore")

# load site-year-level kelp data
# and get GMC for cover and abundance
keen <- read_csv("https://github.com/jebyrnes/keen_gom_temp/raw/oisst_revision/derived_data/keen_merged_data.csv")

keen_site_trans <- keen %>%
  mutate(SITE = case_when(
    SITE=="HURR" ~ "Hurricane Island",
    SITE=="Pumphouse Beach" ~ "Nahant",
    SITE=="Canoe Beach" ~ "Nahant",
    TRUE ~ SITE
  )) %>%
  # group_by(SITE, YEAR) %>%
  # summarize(across(PERCENT_ROCK:POLLACK_PER_TRANSECT,
  #                  ~mean(.x, na.rm=TRUE))) %>%
  ungroup() %>%
  filter(SITE != "SW Appledore") %>%
  group_by(SITE) %>%
  mutate(s_latissima_mean = mean(S_LATISSIMA_SQ_M, na.rm=TRUE),
         s_latissima_anomaly = S_LATISSIMA_SQ_M - s_latissima_mean,
         kelp_mean = mean(TOTAL_KELP_SQ_M, na.rm=TRUE),
         kelp_anomaly = TOTAL_KELP_SQ_M - kelp_mean,
         kelp_cover_mean = mean(PERCENT_KELP, na.rm=TRUE),
         kelp_cover_anomaly = PERCENT_KELP - kelp_cover_mean) %>%
  ungroup()


keen_site <- keen %>%
  mutate(SITE = case_when(
    SITE=="HURR" ~ "Hurricane Island",
    SITE=="Pumphouse Beach" ~ "Nahant",
    SITE=="Canoe Beach" ~ "Nahant",
    TRUE ~ SITE
  )) %>%
  group_by(SITE, YEAR) %>%
  summarize(across(c(PERCENT_ROCK:POLLACK_PER_TRANSECT, TOTAL_RICHNESS),
                   ~mean(.x, na.rm=TRUE))) %>%
  ungroup() %>%
  filter(SITE != "SW Appledore") %>%
  group_by(SITE) %>%
  mutate(s_latissima_mean = mean(S_LATISSIMA_SQ_M, na.rm=TRUE),
         s_latissima_anomaly = S_LATISSIMA_SQ_M - s_latissima_mean,
         kelp_mean = mean(TOTAL_KELP_SQ_M, na.rm=TRUE),
         kelp_anomaly = TOTAL_KELP_SQ_M - kelp_mean,
         kelp_cover_mean = mean(PERCENT_KELP, na.rm=TRUE),
         kelp_cover_anomaly = PERCENT_KELP - kelp_cover_mean) %>%
  ungroup()

# load site-year-level temp data
# and get GMC on spring and summer mean temp
temps <- read_csv("https://github.com/jebyrnes/keen_gom_temp/raw/oisst_revision/derived_data/keen_sites_with_temps.csv")

temps <- temps %>%
  filter(SITE != "SW Appledore") %>%
  mutate(SITE = 
           case_when(
             SITE=="Canoe Beach" ~ "Nahant",
             SITE=="Canoe Beach, Nahant, MA" ~ "Nahant",
             SITE=="Pumphouse Beach" ~ "Nahant",
             SITE=="Pumphouse Beach, Nahant, MA" ~ "Nahant"
             SITE=="Nubble" ~ "Nubble Lighthouse",
             SITE=="SCHO" ~ "Schoodic",
             SITE=="HURR" ~ "Hurricane Island",
             SITE=="Pemaquid" ~"Pemaquid",
             SITE=="Fort Weatherill"  ~ "Fort Weatherill",
             TRUE ~ SITE
           )) %>%
  group_by(SITE, YEAR) %>%
  summarize(across(FALL_MAX_SEA_SURFACE_TEMPERATURE:WINTER_MIN_SEA_SURFACE_TEMPERATURE,
                   ~mean(.x, na.rm=TRUE))) %>%
  ungroup()

temps <- temps %>%
  #dplyr::select(c(SITE, YEAR, FALL_MAX_SEA_SURFACE_TEMPERATURE:WINTER_MIN_SEA_SURFACE_TEMPERATURE)) %>%
  group_by(SITE) %>%
  mutate(spring_sst_mean = mean(SPRING_MEAN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         spring_sst_anomaly = SPRING_MEAN_SEA_SURFACE_TEMPERATURE-spring_sst_mean,
         winter_sst_mean = mean(WINTER_MEAN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         winter_sst_anomaly = WINTER_MEAN_SEA_SURFACE_TEMPERATURE-winter_sst_mean,
         summer_sst_mean = mean(SUMMER_MEAN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         summer_sst_anomaly = SUMMER_MEAN_SEA_SURFACE_TEMPERATURE-summer_sst_mean
  ) %>%
  ungroup()

# merge
dat <- left_join(fw, keen_site_trans) %>%
  left_join(temps)

visdat::vis_dat(dat)

saveRDS(dat, "data/merged_data.rds")


dat_site <- left_join(fw, keen_site) %>%
  left_join(temps)

saveRDS(dat_site, "data/merged_data_site.rds")


# 6_analysis.R ------------------------------------------------------------

#'---------------------------------------
#' Analysis!
#'---------------------------------------

library(dplyr)
library(car)
library(lme4)

dat <- readRDS(file = "data/merged_data.rds") %>%
  mutate(rock = scale(PERCENT_ROCK))

#PERCENT_KELP
#S_LATISSIMA_SQ_M
#TOTAL_KELP_SQ_M
kelp_mod <- lmer(TOTAL_KELP_SQ_M ~
                   summer_sst_mean +
                   summer_sst_anomaly +
                   rock +
                   (1|SITE),
                 data = dat)

keen_sl_mod <-  glmer(TOTAL_KELP_SQ_M+1e-05~ 
                        summer_sst_mean +
                        summer_sst_anomaly +
                        winter_sst_mean +
                        winter_sst_anomaly +
                        rock + (1|SITE),
                      data = dat,
                      family=Gamma(link="log"),
                      control = glmerControl(optimizer = "bobyqa"))

Anova(keen_sl_mod)



library(glmmTMB)
keen_cover_mod <-  glmmTMB((PERCENT_KELP)/100+1e-05 ~ 
                             TOTAL_KELP_SQ_M +
                             summer_sst_mean +
                             summer_sst_anomaly +
                             winter_sst_mean +
                             winter_sst_anomaly +
                             rock + (1|SITE),
                           data = dat,
                           family=beta_family(link = "logit"))

Anova(keen_cover_mod)

# RICHNESS

mod_rich <- lmer(TOTAL_RICHNESS ~ 
                   summer_sst_mean +
                   summer_sst_anomaly +
                   winter_sst_mean +
                   winter_sst_anomaly +
                   TOTAL_KELP_SQ_M +
                   PERCENT_KELP+
                   rock +
                   (1|SITE),
                 data = dat)

Anova(mod_rich)

# FW properties
mod_connectance <- lmer(C ~ 
                          summer_sst_mean +
                          summer_sst_anomaly +
                          winter_sst_mean +
                          winter_sst_anomaly +
                          TOTAL_KELP_SQ_M +
                          PERCENT_KELP+
                          TOTAL_RICHNESS+
                          rock +
                          (1|SITE),
                        data = dat)

Anova(mod_connectance)


#APL

mod_pathlength <- lmer(APL ~ 
                         summer_sst_mean +
                         summer_sst_anomaly +
                         winter_sst_mean +
                         winter_sst_anomaly +
                         TOTAL_KELP_SQ_M +
                         PERCENT_KELP+
                         TOTAL_RICHNESS+
                         rock +
                         (1|SITE),
                       data = dat)

Anova(mod_pathlength)

#avg_cons_degrees
mod_degree <- lmer(avg_cons_degrees ~ 
                     summer_sst_mean +
                     summer_sst_anomaly +
                     winter_sst_mean +
                     winter_sst_anomaly +
                     TOTAL_KELP_SQ_M +
                     PERCENT_KELP+
                     TOTAL_RICHNESS+
                     rock +
                     (1|SITE),
                   data = dat)

Anova(mod_degree)

#sd_cons_degrees

mod_sd_degree <- lmer(sd_cons_degrees ~ 
                        summer_sst_mean +
                        summer_sst_anomaly +
                        winter_sst_mean +
                        winter_sst_anomaly +
                        TOTAL_KELP_SQ_M +
                        PERCENT_KELP+
                        TOTAL_RICHNESS+
                        rock +
                        (1|SITE),
                      data = dat)

Anova(mod_sd_degree)

#diameter

mod_diameter <- lmer(diameter ~ 
                       summer_sst_mean +
                       summer_sst_anomaly +
                       winter_sst_mean +
                       winter_sst_anomaly +
                       TOTAL_KELP_SQ_M +
                       PERCENT_KELP+
                       TOTAL_RICHNESS+
                       rock +
                       (1|SITE),
                     data = dat)


modlist <- list("Connectance" = mod_connectance,
                #  "Average Path Length" = mod_pathlength, #bettter covered by diameter
                "Average Consumer Degree" = mod_degree,
                `SD in Consumer Degree` = mod_sd_degree,
                `Diameter` = mod_diameter)


library(purrr)
library(broom)
library(broom.mixed)


make_ctab_with_aov <- function(term_names, modlist){
  atab <- map(modlist, Anova) %>% 
    map_df(tidy, .id="response") %>%
    filter(term %in% term_names) 
  
  ctab <- map_df(modlist, tidy, .id="response") %>%
    filter(term %in% term_names) %>%
    dplyr::select(-effect, -group) 
  
  left_join(ctab, 
            atab %>% dplyr::select(response, term, p.value)) %>%
    mutate(term = gsub("_", " ", term),
           term = stringr::str_to_title(term),
           term = gsub("Sst", "SST", term))%>%
    mutate(response = factor(response,
                             levels = rev(c("Connectance", 
                                            "Diameter",
                                            "Average Consumer Degree", 
                                            "SD in Consumer Degree"))
    ))
  
}

plot_coefs <- function(atab){
  
  ggplot(atab, 
         aes(x = response, y = estimate, 
             ymin = estimate-2*std.error, 
             ymax = estimate + 2*std.error)) + 
    geom_pointrange() + 
    facet_wrap(vars(term)) + 
    coord_flip() + 
    geom_hline(yintercept = 0, lty = 2) +
    theme_bw(base_size = 17) +
    labs(x="", y = "Coefficient")
}

temp_eff <- make_ctab_with_aov(c("summer_sst_anomaly", "winter_sst_anomaly"),
                               modlist)

temp_eff
plot_coefs(temp_eff)
ggsave("figures/anomoly_coefs.jpg", dpi = 600,
       width=7.4)

mean_eff <- make_ctab_with_aov(c("summer_sst_mean", "winter_sst_mean"),
                               modlist)
plot_coefs(mean_eff)

other_eff <- make_ctab_with_aov(c("TOTAL_KELP_SQ_M" ,
                                  "PERCENT_KELP",
                                  "TOTAL_RICHNESS"),
                                modlist)
plot_coefs(other_eff)


rich_eff <- make_ctab_with_aov(c( "TOTAL_RICHNESS"),
                               modlist)
plot_coefs(rich_eff)

ggsave("figures/rich_coefs.jpg", dpi = 600,
       width=7.4)


map(modlist, performance::r2_nakagawa)

eff_on_rich <- make_ctab_with_aov(c("summer_sst_anomaly", "winter_sst_anomaly"),
                                  list(Richness = mod_rich))

ggplot(eff_on_rich, 
       aes(x = term, y = estimate, 
           ymin = estimate-2*std.error, 
           ymax = estimate + 2*std.error)) + 
  geom_pointrange() + 
  coord_flip() + 
  geom_hline(yintercept = 0, lty = 2) +
  theme_bw(base_size = 17) +
  labs(x="", y = "Coefficient",
       subtitle="Effect of Temperature on\nSpecies Richness")

ggsave("figures/eff_on_rich.jpg", dpi = 600,
       width=7.4)


# make_web_functions.R ----------------------------------------------------

require(tidygraph)
require(ggraph)
require(NetIndices)

fw <- read_csv("data/cleaned_joined_int_data.csv") %>%
  filter(interactionTypeName %in%
           c("preys on", "preyed upon by",
             "eats", "is eaten by"))

# function to filter to interactions of just species involved
make_one_web <- function(adf, foodweb = fw){
  sp <- adf$SPECIES
  
  local_web <- foodweb %>%
    filter(sourceTaxonName %in% sp &
             targetTaxonName %in% sp)
  #cleanup
  local_web %>%
    dplyr::select(sourceTaxonName, 
                  interactionTypeName, 
                  targetTaxonName) %>%
    group_by(sourceTaxonName, 
             interactionTypeName, 
             targetTaxonName) %>%
    summarize(n_in_db = n()) %>%
    ungroup()
  
}

# from predator to prey
make_from_to <- function(aweb){
  aweb %>%
    mutate(from = ifelse(interactionTypeName %in% 
                           c("preys on", "eats"),
                         sourceTaxonName,
                         targetTaxonName
    ),
    to = ifelse(interactionTypeName %in% 
                  c("preyed upon by", "is eaten by"),
                sourceTaxonName,
                targetTaxonName
    )
    ) %>%
    dplyr::select(from, to)
  
}

#get xy and things
add_info_to_web <- function(agraph, amat, 
                            seed=NULL, sp_position = NULL){
  set.seed(seed)
  
  troph <- TrophInd(Tij = amat)
  
  agraph <- agraph %>%
    activate(nodes) %>%
    mutate(trophic_level = abs(troph$TL),
           omnivory_ind = troph$OI,
           x = runif(length(name)),
           y = trophic_level)
  
  if(!is.null(sp_position)){
    agraph <- agraph %>% 
      activate(nodes) %>%
      dplyr::select(-x, -y) %>%
      left_join(sp_position)
  }
  
  agraph
  
}


# to make a barebones plot
make_fw_ggraphs <- function(agraph) {
  ggraph(
    agraph,
    layout = "manual",
    circular = FALSE,
    x = agraph %>% activate(nodes) %>% pull(x),
    y = agraph %>% activate(nodes) %>% pull(y)
  )  +
    geom_edge_link0(alpha = 0.4) +
    geom_node_point(aes(x = x, y = trophic_level, 
                        color = trophic_level), size = 3) +
    theme_void() +
    scale_color_viridis_b(option = "C",guide=NULL ) 
}


# web_plots.R -------------------------------------------------------------

library(ggraph)
library(tidygraph)
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)

source("scripts/make_web_functions.R")

weball <- readRDS("data/web_all.rds")

fw_plot <- weball$plot_web[[1]]
fw <- weball$graph[[1]]
# fun
fw_ggraph <- function(agraph) {
  ggraph(
    agraph,
    layout = "manual",
    circular = FALSE,
    x = agraph %>% activate(nodes) %>% pull(x),
    y = agraph %>% activate(nodes) %>% pull(y)
  ) +
    theme_void()
}

fw_ggraph(fw) +
  geom_edge_link2(alpha = 0.2) +
  
  geom_node_point(aes(color = trophic_level), size = 3) +
  scale_color_viridis_b(option = "C",guide=NULL ) 

#from the total int data
int_dat <- read_csv("./data/cleaned_joined_int_data.csv") %>%
  filter(interactionTypeName %in%
           c("preys on", "preyed upon by",
             "eats", "is eaten by"))
int_dat <- int_dat %>%
  group_by(sourceTaxonName, targetTaxonName, interactionTypeName) %>%
  slice(1L)


web_all_int <- tibble(SPECIES = 
                        unique(int_dat$sourceTaxonName, 
                               int_dat$targetTaxonName)) %>%
  make_one_web(foodweb = int_dat) %>%
  make_from_to %>%
  tbl_graph(edges=.)

adj_mat_all_int <- igraph::as_adjacency_matrix(web_all_int) %>%
  as.matrix

web_all_int <- add_info_to_web(web_all_int, adj_mat_all_int)



fw_ggraph(web_all_int) +
  geom_edge_link2(alpha = 0.2) +
  
  geom_node_point(aes(color = trophic_level), size = 3) +
  scale_color_viridis_b(option = "C",guide=NULL ) 


#some queries
web_all_int %>%
  activate("nodes") %>%
  arrange(desc(trophic_level))

web_all_int %>%
  activate("nodes") %>%
  filter(name=="Strongylocentrotus droebachiensis")



web_all_int %>%
  activate("nodes") %>% 
  pull(name) %>% length()

web_all_int %>%
  activate("edges") %>% 
  pull(from) %>% length()
