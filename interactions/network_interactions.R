## load required packages
library(ggplot2)
library(skimr)
library(tidyverse)
library(rglobi)
library(cooccur)
library(patchwork)
library(ggthemes)
library(network)
library(igraph)
library(sna)
library(intergraph)

setwd()

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
write_csv(sub_sp, "subtidal_species.csv")

## identify species found in both zones and label their zone "both" instead
## and make a final list of species found in the intertidal and subtidal during our study period (there are 144)
## 25 of these species are found in both zones
comb<- rbind(int_sp, sub_sp)
comb$zone[duplicated(comb$organism,fromLast = TRUE)] <- "both"
all_sp<- comb[!duplicated(comb$organism), ]
all_sp 



## START MAKING TROPHIC INTERACTION NETWORKS ----------------------------------------------------------------------

## DATABASE 1: Jarrett's interaction database 
## Load Jarrett's interaction database
inter <- read_csv("interactions_final.csv")

## Identify interactions from Jarrett's interaction database. All  interactions have
## been directionalized to involve a source taxon that preys on a target (i.e. entries
## such as 'preyed upon by, have been changed to a 'preys on' interaction)

## trophic interactions only
inter_db1 <- inter %>%
  subset(interactionTypeName =='preys on') %>%
  filter(sourceTaxonName %in% all_sp$organism) %>%
  filter(targetTaxonName %in% all_sp$organism) %>%
  select(sourceTaxonName, targetTaxonName) %>%
  unique

## all interactions
inter_all_db1 <- inter %>%
  subset(interactionTypeName =='preys on' || 'mutualistically interacts with' || 'symbiotically interacts with') %>%
  filter(sourceTaxonName %in% all_sp$organism) %>%
  filter(targetTaxonName %in% all_sp$organism) %>%
  select(sourceTaxonName, targetTaxonName) %>%
  unique

## DATABASE 2: GLOBI
## The following command shows a list of interaction types in Globi
get_interaction_types()

## Create vector of all taxa present in intertidal and subtidal during our study period 
species <- dplyr::pull(all_sp,organism)


## Create matrices of interactions between species present at our intertidal and subtidal
## sites from Globi interaction database. This code filters the interactions in Globi by
## the species observed at our study sites. *Note, I have not currently filtered the Globi 
## interactions by our study site location.  
## The first commands gives a matrix where the direction is from predator to prey
## The second gives a matrix where the direction is from prey to predator; we will later 
## transpose this matrix to match its direction with the predator -> prey matrix

globi_interactions <- get_interaction_matrix(source.taxon.names = species,
                                             target.taxon.names = species,interaction.type = list('preysOn','eats'))

globi_interactions2 <- get_interaction_matrix(source.taxon.names = species,
                                              target.taxon.names = species,interaction.type = list('eatenBy','preyedUponBy'))

## The following code creates matrices for all directional interactions from Globi.
## Note: in the graphs below, these matrices are not being used. 
globi_all <- get_interaction_matrix(source.taxon.names = species,
                                    target.taxon.names = species,interaction.type = list('preysOn','eats',
                                                                                         'hostOf','symbiontOf',
                                                                                         'mutualistOf','commensalistOf',
                                                                                         'kills','parasiteOf',
                                                                                         'endoparasiteOf','ectoparasiteOf',
                                                                                         'pathogenOf'))

globi_all2 <- get_interaction_matrix(source.taxon.names = species,
                                     target.taxon.names = species,interaction.type = list('eatenBy','preyedUponBy',
                                                                                          'hasHost','killedBy',
                                                                                          'hasParasite','hasEndoparasite',
                                                                                          'hasEctoparasite','hasPathogen'))



## Convert the GloBi matrices to adjacency matrices that we can use to create graphs. 
## To create an adjacency matrix, you need a square matrix so we need to identify the
## first column as row names. 
## Note: This code uses the matrices that only give trophic interactions; if you want
## to include non-trophic interactions, you need to change the inputs to the matrices
## above that include all interactions. 
matrix_globi <- as.matrix(globi_interactions,row.names=1, header=TRUE)
rownames(matrix_globi) <- matrix_globi[,1]
matrix_globi <- matrix_globi[order(as.numeric(row.names(matrix_globi)), decreasing=FALSE),]
matrix_globi <- matrix_globi[,-1]
dim(matrix_globi) ## confirm it is a square matrix

matrix_globi2 <- as.matrix(globi_interactions2,row.names=1, header=TRUE)
rownames(matrix_globi2) <- matrix_globi2[,1]
matrix_globi2 <- matrix_globi2[order(as.numeric(row.names(matrix_globi2)), decreasing=FALSE),]
matrix_globi2 <- matrix_globi2[,-1]
dim(matrix_globi2) ## confirm it is a square matrix
globitrans <- t(matrix_globi2) ## transpose the second matrix, so the interaction direction is the same as the first matrix
dim(globitrans)

## Create directed graphs for each database and unite them into a single, directed graph
g_db1 <- graph_from_data_frame(inter_db1,directed=TRUE) ## Jarrett's database
g_globi <- graph_from_adjacency_matrix(matrix_globi,mode="directed") ## Globi matrix 1
g2_globi <- graph_from_adjacency_matrix(globitrans,mode="directed") ## Globi matrix 2, transposed
g_united <-igraph::union(g_db1,g_globi,g2_globi) # Unite all graphs

g_united ## check that g_united is now a graph

## PLOTTING GRAPHS ------------------------------------------------------------
## First, color the vertices.
## We will use the dataframe indicating our species/zones (all_sp) 
## to color our vertices (species) based on their zone.
## We need to make sure both the order of species in our graph and 
## our vertex attribute file are the same. If not, the colors will
## go to the wrong node.
ordered.vertices <-get.data.frame(g_united, what="vertices") 
 
vertex2<- all_sp[order(match(all_sp$organism,ordered.vertices$name)),]

## Now we add the zone column the zone from the dataframe that re-ordered the species to the graph
V(g_united)$zone <- vertex2$zone

## And finally, we can add colors to our nodes based on species' zones
V(g_united)[zone == 'intertidal']$color = "lightblue"
V(g_united)[zone == 'subtidal']$color = "lightgreen"
V(g_united)[zone == 'both']$color = "grey"
plot(g_united) ## testing the colors were added

## Now we can remove nodes that are not connected to any other nodes
isolated = which(igraph::degree(g_united)==0)
g_all = igraph::delete.vertices(g_united, isolated)
plot(g_all) ## testing the nodes were removed

## The following command also removes loops (i.e. a species interacting with itself, 
## for instance through cannibalism); It is here if we need it, but for now I'm 
## not using it. 
net = (igraph::delete.vertices(simplify(g_united), degree(g_united)==0)) 

## Create possible layouts for graphs
circ <- layout_in_circle(g_all)
nice <- layout_nicely(g_all)
fr <- layout.fruchterman.reingold(g_all)
kw <- layout.kamada.kawai(g_all)

## Plot graphs in a few different ways
plot(g_all, edge.arrow.size = 0.2) ## plot normally
plot(g_all, edge.arrow.size = 0.2,layout = circ) ## plot as circle with names
plot(g_all, edge.arrow.size = 0.2,vertex.label=NA,vertex.size=4, layout = nice) # plot as web, remove names


## Create sub-graphs with only certain species ---------------------------------------

## We can select a species we want to look at, and pull out all the interactions it has
inc.edges <- incident(g_all,  V(g_all)["Carcinus maenas"], mode="all")
g_sub<- subgraph.edges(g_all, inc.edges, delete.vertices = TRUE)
plot(g_sub,edge.arrow.size = 0.2)

## Use this if you want to change the color of any of the nodes (for instance, the central species)
V(g_sub)["Carcinus maenas"]$color="yellow"
plot(g_sub,edge.arrow.size = 0.2)

