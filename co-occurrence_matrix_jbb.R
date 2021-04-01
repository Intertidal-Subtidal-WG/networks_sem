## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##
## Intertidal-subtidal coupled community networks-SEM
## Contributor: Joey Burant
## Last updated: 01 April 2021
## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##

# initial set up ----------------------------------------------------------

## clear the decks
# rm(list = ls())

## where am I working?
here::here()

## load required packages
# library(readr)
# suppressMessages(library(dplyr))
# library(stringr)
# library(purrr)
# library(ggplot2)
library(skimr)
library(ggthemes)
# library(patchwork)
library(see)
library(cooccur)
library(tidyverse)

## set a plotting theme
theme_set(theme_few())


# data import -------------------------------------------------------------

## We want to import the combined intertidal-subtidal data that has
## been aggregated by site. This is available from the 
## data_merge_intertidal_subtidal repo, and stored as an RDS file
## file name: combined_all_abundance_data_site.RDS

## specify the full URL for the data
ghURL <- "https://raw.githubusercontent.com/Intertidal-Subtidal-WG/data_merge_intertidal_subtidal/master/tidy_data/combined_all_abundance_data_site.RDS"

## download the RDS file
download.file(ghURL, "combined_all_abundance_data_site.RDS", 
              method = "curl")

## read in the RDS file
data_site <- read_rds("combined_all_abundance_data_site.RDS")


# data exploration --------------------------------------------------------

## this dataset has been created by averaging abundance data per
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


## what variables do we have in this dataset?
skim(data_site)

## how many years of data are present?
range(data_site$year) ## 7 years of data (but not for both sub and int)

## how many sites?
unique(data_site$site) ## 3 sites

## list of unique taxa sampled in the dataset
unique(data_site$organism) ## 202 unique values -- need to clean up
unique(data_site$kingdom)
# [1] "Animalia"  NA          "Plantae"   "Chromista" "Fungi"    
n_missing((data_site$kingdom)) ## 1418 rows missing kingdom information


## filter data set to exclude rows where mean_value is 0
data_site2 <- data_site %>% 
  filter(mean_value > 0)

## How many species are sampled per year in each site?

## "heat map" (geom_tile)
data_site2 %>% 
  group_by(site, zone, year) %>% 
  summarise(n = n_unique(organism)) %>% 
  filter(year %in% c(2014:2018)) %>% 
  ggplot(mapping = aes(x = as.factor(year), y = zone, fill = n)) + 
  geom_tile(colour = "white") + 
  scale_fill_viridis_c(begin = 0.1, end = 0.85, direction = -1, 
                       name = "# of species") + 
  scale_y_discrete(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  # facet_wrap(~ site, ncol = 3) +
  facet_grid(zone ~ site, scales = "free_y") + 
  labs(x = NULL, y = NULL, 
       title = "Number of species observed annually") + 
  theme_blackboard() + 
  theme(legend.position = "right", 
        legend.title = element_blank(), 
        strip.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.y = element_text(angle = 90), 
        text = element_text(size = 15))

## bar plot (geom_col)
data_site2 %>% 
  group_by(site, zone, year) %>% 
  summarise(n = n_unique(organism)) %>% 
  filter(year %in% c(2014:2018)) %>% 
  ggplot(mapping = aes(x = year, y = n)) + 
  geom_col(mapping = aes(fill = n), colour = "white") + 
  # geom_point(colour = "white", fill = "white") + 
  # geom_smooth(method = "glm", colour = "white") + 
  scale_fill_viridis_c(begin = 0.1, end = 0.85, direction = -1, 
                       name = "# of species") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 75), expand = c(0, 0)) +
  facet_grid(zone ~ site) + 
  labs(x = NULL, y = NULL, 
       title = "Number of species observed annually") + 
  theme_blackboard() + 
  theme(legend.position = "right", 
        legend.title = element_blank(), 
        strip.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        text = element_text(size = 15))


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


## list organisms sampled in the intertidal
(int_sp <- data_site3 %>% 
  filter(zone == "Intertidal", kingdom != "Abiotic") %>% 
  distinct(organism))
n_unique(int_sp)

## list organisms sampled in the subtidal
(sub_sp <- data_site3 %>% 
  filter(zone == "Subtidal", kingdom != "Abiotic") %>% 
  distinct(organism))
n_unique(sub_sp)

## organisms sampled in both the intertidal and subtidal zones
sp <- intersect(int_sp, sub_sp) %>% 
    left_join(., unique(data_site3[c("organism", "kingdom")]), 
              by = "organism") %>% 
    arrange(kingdom, organism) %>% 
    print(n = 40)
n_unique(sp$organism)

## COMMENT:
## in the filtered data, there are 25 species that occur in both zones

## how many unique biotic organisms?
n_unique(data_site3$organism[data_site3$kingdom != "Abiotic"]) ## 144

## clean up
rm(sp, int_sp, sub_sp, ghURL)


# standardize measures ----------------------------------------------------

## depending on the sampling protocol and zone, organisms were
## recorded as percent cover or count. obviously these two metrics
## are not directly comparable...
## to account for this, letter mean-centre the measures grouped by 
## zone, organism, and sampling protocol

## mean-centre the data
data_site3 <- data_site3 %>% 
  group_by(zone, organism, protocol) %>% 
  mutate(scaled_value = scale(mean_value, 
                              center = TRUE, scale = TRUE)) %>% 
  ungroup() %>% 
  ## add a column to group data by year, site, and zone
  unite("year_site_zone", c("year", "site", "zone"), 
        sep = "_", remove = FALSE)

## COMMENT:
## because there are different sampling heights (levels) in the 
## intertidal, the data need to be averaged by zone first before
## they can be passed used in a co-occurrence matrix (otherwise
## there would be multiple measures for some organisms)

## average the scaled values for each organism by year, site, and zone
site3_zone_avg <- data_site3 %>% 
  group_by(year_site_zone, year, site, zone, kingdom, organism) %>% 
  summarise(avg_scl_val = mean(scaled_value, na.rm = TRUE)) %>% 
  ungroup()


# co-occurrence probabilities ---------------------------------------------

## create an abundance matrix
abund_mat <- site3_zone_avg %>% 
  filter(kingdom != "Abiotic") %>% 
  select(year_site_zone, organism, avg_scl_val) %>% 
  pivot_wider(names_from = year_site_zone, 
              values_from = avg_scl_val) %>% 
  column_to_rownames(var = "organism")

## change all NAs and NaNs to zeroes
abund_mat[is.na(abund_mat)] <- 0

## convert abundance matrix to a presence-absence matrix
pres_abs <- abund_mat
pres_abs[pres_abs > 0] <- 1
pres_abs[pres_abs < 0] <- 1 

## COMMENT:
## need to do both > and < 0 because the values are mean-centred

## calculate co-occurrence probabilities using presence-absence matrix
?cooccur()
site3_coocc <- cooccur(mat = pres_abs, 
                type = "spp_site", 
                thresh = TRUE, 
                spp_names = TRUE)

## get summary of co-occurrence matrix
summary(site3_coocc)

## plot co-occurrence probabilities
plot(site3_coocc, cex.axis = 0.5)
