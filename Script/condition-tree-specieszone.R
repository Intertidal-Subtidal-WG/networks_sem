## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##
## Intertidal-subtidal coupled community networks-SEM
## Contributor: Julien Beaulieu
## Last updated: 05-05-2021
## This script is to determine if species are considered as
## Subtidal, intertidal or both
## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##

library(tidyverse)
library(skimr)

## specify the full URL for the data
ghURL <- "https://raw.githubusercontent.com/Intertidal-Subtidal-WG/data_merge_intertidal_subtidal/master/tidy_data/combined_all_abundance_data_site.RDS"

## download the RDS file
download.file(ghURL, "combined_all_abundance_data_site.RDS", 
              method = "curl")

## read in the RDS file
data_site <- read_rds("combined_all_abundance_data_site.RDS")

# Identify species present --------------------------------------------------------
# This is from the code Joey developed to identify species present
## at our study site across the study period. 
## This dataset has been created by averaging abundance data per
## taxon (and sampling protocol) by site (and level) in each year

## lower case names
names(data_site)
names(data_site) <- tolower(names(data_site))

## add a column to group protocols by sampling location (int vs. sub)
data_site <- data_site %>% 
  ## using case_when to be explicit
  mutate(zone = case_when(protocol %in% c("Intertidal_Cover", 
                                          "Intertidal_Count") ~ 
                            "Intertidal", 
                          protocol %in% c("FISH", "PERCENT_COVER", 
                                          "QUAD", "SWATH") ~ 
                            "Subtidal"))
## check
n_missing(data_site$zone)
unique(data_site$protocol[data_site$zone == "Intertidal"])
unique(data_site$protocol[data_site$zone == "Subtidal"])


## list of unique taxa sampled in the dataset
unique(data_site$organism) ## 202 unique values -- need to clean up
unique(data_site$kingdom)
# [1] "Animalia"  NA          "Plantae"   "Chromista" "Fungi"    
n_missing((data_site$kingdom)) ## 1418 rows missing kingdom information

## filter data set to exclude rows where mean_value is 0
data_site2 <- data_site %>% 
  filter(mean_value > 0)

# clean up taxonomy -------------------------------------------------------

## how many unique taxa?
n_unique(data_site2$organism) ## 172 species

data_site2 %>% 
  group_by(kingdom, phylum) %>% 
  summarise(n = n_distinct(organism))

## 27 'species' with no taxonomic information...
## what are they?
data_site2 %>% 
  filter(is.na(kingdom)) %>% 
  distinct(organism) %>% 
  print(n = 50)

## COMMENT:
## most of the 27 are not biological species, but other features 
## (e.g., bare rock, shell hash, bedrock)
## others are more generic terms for other species that are potentially
## identified at a more specific level elsewhere in the dataset
## (e.g., amphipod tube mat, barnale, blady ulvoid, diatom tube mat, 
## encrusting coraline, red algae turf, tubular ulvoid, filamentous green)
## or unidentified organisms (e.g., UNID Juv Laminariales, unidentified
## erect coraline, unidentified filamentous red, unidentified red blad)
## 2 taxa (Testudinalia testudinalis, Tricellaria inopinata) that need
## higher-level taxonomy filled in...

## create a new 'kingdom' for abiotic features and fill in the
## kingdom for biological species with missing information
data_site2 <- data_site2 %>% 
  mutate(kingdom = 
           if_else(is.na(kingdom) & 
                     organism %in% c("Bare rock", "Black zone", 
                                     "Shell hash", "bedrock", 
                                     "boulder (>100cm)", "boulder (50-100cm)", 
                                     "boulder (25-50cm)", "boulder (<25cm)", 
                                     "cobble (<25cm)", "sand (>1cm)", 
                                     "shallow sand (<1cm)", "shell debris", 
                                     "pebbles (<5cm)"), 
                   "Abiotic", kingdom), 
         kingdom = 
           if_else(is.na(kingdom) & 
                     organism %in% c("Amphipod tube mat", "Barnacle", 
                                     "Testudinalia testudinalis", 
                                     "Tricellaria inopinata"), 
                   "Animalia", kingdom), 
         kingdom = 
           if_else(is.na(kingdom) & 
                     organism %in% c("Unidentified Red Blade", 
                                     "Unidentified Filamentous Red", 
                                     "Unidentified Erect Coralline", 
                                     "UNID Juv Laminariales", 
                                     "Red Algal Turf", "Encrusting coralline", 
                                     "Diatom Tube Mat"), 
                   "Chromista", kingdom), 
         kingdom = 
           if_else(is.na(kingdom) & 
                     organism %in% c("Filamentous Green", 
                                     "Blady Ulvoid", 
                                     "Tubular Ulvoid"), 
                   "Plantae", kingdom))

unique(data_site2$kingdom)

## what are the observation where kingdom is NA?
length(which(is.na(data_site2$kingdom))) ## 22 blank organisms
# filter(data_site2, is.na(kingdom)) %>% View()

## not sure what to do with these observations (need to be fixed)
## just drop them for now
data_site3 <- filter(data_site2, !is.na(kingdom))

## limit the data to years with both sub- and intertidal samples
data_site3 <- filter(data_site3, year %in% 2014:2018)

## what are the unique species observed in the data?
data_site3 %>% 
  group_by(kingdom) %>% 
  distinct(organism) %>% 
  arrange(kingdom, organism) %>% 
  print(n=200)

## COMMENT: 
## total of 166 unique 'organisms', including 12 abiotic features
## however, there are several typos or the same organism listed to
## different levels of specificity, which should be aggregated

## fix typos and species-genus non-specificity
data_site3$organism[data_site3$organism == 
                      "Carcinus maenus"] <- "Carcinus maenas"
data_site3$organism[data_site3$organism == 
                      "Idotea baltica"] <- "Idotea balthica"
data_site3$organism[data_site3$organism == 
                      "Metridium senile pallidus"] <- "Metridium senile"
data_site3$organism[data_site3$organism == 
                      "Dumontia concortum"] <- "Dumontia contorta"
data_site3$organism[data_site3$organism == 
                      "Amphipod tube mat"] <- "Amphipoda"
data_site3$organism[data_site3$organism == 
                      "Anomia"] <- "Anomia simplex"
data_site3$organism[data_site3$organism == 
                      "Tricellaria"] <- "Tricellaria inopinata"
data_site3$organism[data_site3$organism == 
                      "Ceramium"] <- "Ceramium virgatum"
data_site3$organism[data_site3$organism == 
                      "Phymatolithon"] <- "Phymatolithon lenormandii"
data_site3$organism[data_site3$organism == 
                      "Coralline"] <- "Corallina officinalis"

## re-check
data_site3 %>% 
  group_by(kingdom) %>% 
  distinct(organism) %>% 
  arrange(kingdom, organism) %>% 
  print(n=200)

## COMMENT: 
## after corrections, there are 156 organisms, 
## including 12 abiotic features

(int_sp <- data_site3 %>% 
    filter(zone == "Intertidal", kingdom != "Abiotic") %>% 
    distinct(organism) %>%
    add_column(zone = 'intertidal'))
n_unique(int_sp)
write_csv(int_sp, "intertidal_species.csv")

## list organisms sampled in the subtidal
(sub_sp <- data_site3 %>% 
    filter(zone == "Subtidal", kingdom != "Abiotic") %>% 
    distinct(organism) %>%
    add_column(zone = 'subtidal'))
n_unique(sub_sp)
sub_sp
#write_csv(sub_sp, "subtidal_species.csv")


## identify species found in both zones and label their zone "both" instead
## and make a final list of species found in the intertidal and subtidal during our study period (there are 144)
## 25 of these species are found in both zones
comb<- rbind(int_sp, sub_sp)
comb$zone[duplicated(comb$organism,fromLast = TRUE)] <- "both"
all_sp<- comb[!duplicated(comb$organism), ]
all_sp 

# For each specie found in both zones evaluate the actual zone it is in according
# to decision tree. A specie is considered both if (0.85 >= ab tot intertidal/ab tot subtidal >= 0.15),
# if the specie is present in at least 10% of the samples of each zone (intertidal and subtidal),
# and if there is no reason to beleave individuals from both zones are isolated 
# and constitute two distinct populations.

# 1) create a data frame with ab tot intertidal/ab tot subtidal for each specie found in both
# 3) The actual decision tree

Ab_ratio <- filter(comb, zone == "both", .preserve = T)
Ab_ratio$Ab_inter <- 0
Ab_ratio$Ab_sub <- 0

for (i in (1:nrow(Ab_ratio))){
  for (j in (1:nrow(data_site3))){
    if (Ab_ratio$organism[i] == data_site3$organism[j]){
      if (data_site3$zone[j] == "Intertidal"){
        Ab_ratio$Ab_inter[i] = Ab_ratio$Ab_inter[i] + data_site3$mean_value[j]
      } else {
        Ab_ratio$Ab_sub[i] = Ab_ratio$Ab_sub[i] + data_site3$mean_value[j]
      }
    }
  }
}

# count the amount of samples containing each specie in each zone
Ab_ratio$Nb_inter <- 0
Ab_ratio$Nb_sub <- 0

#count the number of time each specie is observed in each zone
for (i in (1:nrow(Ab_ratio))){
  for (j in (1:nrow(data_site3))){
    if (Ab_ratio$organism[i] == data_site3$organism[j]){
      if (data_site3$zone[j] == "Intertidal"){
        Ab_ratio$Nb_inter[i] = Ab_ratio$Nb_inter[i] + 1
      } else {
        Ab_ratio$Nb_sub[i] = Ab_ratio$Nb_sub[i] + 1
      }
    }
  }
}

# Create a column in data_site3 for which all unic combination of sample-organism can be filtered
data_site4 <- data_site3
data_site4 <- unite(data_site4, unic_sampleID, c("zone","year","site","level"),
                    sep = "_", remove = F)
data_4_inter <- filter(data_site4, zone == "Intertidal")
data_4_sub <- filter(data_site4, zone == "Subtidal")
Ab_ratio$pres_inter <- 0
Ab_ratio$pres_sub <- 0

# count the number of samples where each specie was observed in intertidal
for(i in (1:nrow(Ab_ratio))){
  for (j in (1:nrow(data_4_inter))){
    if(Ab_ratio$organism[i] == data_4_inter$organism[j]){
      temp <- filter(data_4_inter, organism == Ab_ratio$organism[i])
      temp2 <- unique(temp$unic_sampleID)
      Ab_ratio$pres_inter[i] <- length(temp2)
    }
  }
}

# count the number of samples where each specie was observed in subtidal

for(i in (1:nrow(Ab_ratio))){
  for (j in (1:nrow(data_4_sub))){
    if(Ab_ratio$organism[i] == data_4_sub$organism[j]){
      temp <- filter(data_4_sub, organism == Ab_ratio$organism[i])
      temp2 <- unique(temp$unic_sampleID)
      Ab_ratio$pres_sub[i] <- length(temp2)
    }
  }
}

#pourcntage d'échantillons de chaque zone contenant l'espèce en assumant 187 échantillons
#dans intertidal et 15 pour subtidal

Ab_ratio$pourc_inter <- Ab_ratio$pres_inter/187
Ab_ratio$pourc_sub <- Ab_ratio$pres_sub/15

#Calculate Ab_inter/Ab_int
Ab_ratio$Ab_ratio <- Ab_ratio$Ab_inter/(Ab_ratio$Ab_sub + Ab_ratio$Ab_inter)

#determine the zone of each specie

#first condition
for (i in (1:nrow(Ab_ratio))) {
  if (Ab_ratio$Ab_ratio[i] > 0.85){
    Ab_ratio$zone[i] = "intertidal"
  }
  if (Ab_ratio$Ab_ratio[i] < 0.15){
    Ab_ratio$zone[i] = "subtidal"
  } else {
    Ab_ratio$zone = "both"
  }
}

# deuxième condition

for (i in (1:nrow(Ab_ratio))) {
  if(Ab_ratio$zone[i] == "both"){
    if (Ab_ratio$pourc_inter[i] < 0.1){
      Ab_ratio$zone[i] = "subtidal"
    }
    if (Ab_ratio$pourc_sub[i] < 0.1){
      Ab_ratio$zone[i] = "intertidal"  
    } else {
      Ab_ratio$zone[i] = "both"
    }
  }
}
