## networks for BEM presentation
## Created by: Joey Burant
## Last updated: 22 March 2022

## load required packages
library(tidyverse)
# library(readr)
# library(tidyr)
# library(dplyr)
# library(purrr)
library(patchwork)
## all the graphing/network packages!
library(igraph)
library(tidygraph)
library(ggnetwork)
library(ggraph)

## set colour palette
## want these three colour but in a slightly different order
## green = subtidal, orange = intertidal, purple = subtidal
net_pal <- RColorBrewer::brewer.pal("Dark2", n = 3)[c(1,3,2)]

## load data

## these datasets come from wrangling in JEKB_foodwebs_scripts.R
## changed to this script to clean things up...

## interactions database (combined literature and GLOBI data)
interactions_comb2 <- read_csv(
  "data/combined_litsearch_globi_interactions2.csv")
nrow(interactions_comb2) ## [1] 550

## combined all epibiont/epiphyte interactions into one group
## remove one parasitic interaction and one 'visitor' (?)
interactions_comb2 <- interactions_comb2 %>% 
  mutate(interactionTypeName = ifelse(
    interactionTypeName == "epiphyte of", 
    "epibiont of", interactionTypeName)) %>% 
  filter(!interactionTypeName %in% c("parasite of", "visits"))

## island species list
sp_zone2 <- readRDS("data/species_list_by_zone2.RDS")
nrow(sp_zone2) ## [1] 165


# combined interaction network --------------------------------------------

## create a graph combing subtidal and intertidal communities
combined_graph <- igraph::graph_from_data_frame(interactions_comb2)

## inspect vertex (node) attributes
# vertex_attr(combined_graph)
vertex_attr_names(combined_graph)
# [1] "name"

V(combined_graph)$name

## add an attribute for zone (subtidal, intertidal, or both)
graph_sp_comb <- tibble(name = V(combined_graph)$name) %>% 
  left_join(., distinct(sp_zone2[c("organism", "sp_zone")]), 
            by = c("name" = "organism"))
V(combined_graph)$zone <- graph_sp_comb$sp_zone

## add attribute for interaction type (6 possible options)
unique(interactions_comb2$interactionTypeName)
# [1] "preys on"    "eats"        "epibiont of"
V(combined_graph)$interaction_type <- interactions_comb2$interactionTypeName

## calculate and add an attribute for degree 
## (i.e., how many connections are made to each node?)
V(combined_graph)$degree <- igraph::degree(combined_graph, 
                                           ## scale by 0.1
                                           mode = "all")
## use the degree as a weighting variable for plotting nodes
V(combined_graph)$weight <- igraph::degree(combined_graph, 
                                           ## scale by 0.1
                                           mode = "all") * 0.1

## inspect edge (link) attributes
# edge_attr(combined_graph)
edge_attr_names(combined_graph)

## convert the graph into a tidy object using {tidygraph}
tidy_combined_g <- tidygraph::as_tbl_graph(combined_graph)
tidy_combined_g

## plot the graph using {ggraph}
tidy_combined_g %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  ggraph(layout = "kk") + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  geom_edge_link(aes(linetype = interactionTypeName),
                 alpha = 0.5, width = 0.5) +
  scale_fill_manual(values = net_pal, name = "Zone") +
  # scale_fill_brewer(palette = "Dark2", name = "Zone") + 
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = FALSE, 
         fill = guide_legend("Zone"), 
         linetype = guide_legend(NULL)) + 
  theme(legend.position = "bottom", 
        legend.box = "vertical")

## create a layout for the graph
layout_combined <- create_layout(tidy_combined_g, 
                                 layout = "kk") %>% 
  rowwise() %>% 
  ## change x-axis values from layout so that 
  ## species are arranged/grouped by the zone they occupy
  mutate(x = case_when(zone == "Subtidal" ~ 
                         rnorm(1, mean = 1, sd = 0.2), 
                       zone == "Both" ~ 
                         rnorm(1, mean = 2, sd = 0.2), 
                       zone == "Intertidal" ~ 
                         rnorm(1, mean = 3, sd = 0.2))) 

## recreate the layout and add the previously recalculated x values
## NOTE: need to do this because the output above is a tibble, but
## we need the layout to be of class: "layout_tbl_graph"
layout_combined2 <- create_layout(tidy_combined_g, 
                                  layout = "kk")
## add specified x and y values (no doing anything with y here)
layout_combined2$x <- layout_combined$x
layout_combined2$y <- layout_combined$y
## check class of layout object
class(layout_combined2)

## replot the network
p.comb <- layout_combined2 %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  ggraph() + 
  # geom_edge_hive(aes(linetype = interactionTypeName),
  #                colour = "grey30", alpha = 0.5, width = 0.5) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey30", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  scale_fill_manual(values = net_pal, name = "Zone") + 
  # scale_fill_brewer(palette = "Dark2", name = "Zone") +
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = FALSE, 
         fill = guide_legend(order = 1)) + 
  labs(subtitle = "Integrated nearshore foodweb at Appledore") + 
  theme(legend.position = "bottom", 
        legend.box = "vertical")

ggsave(filename = "Plots/network_combined.png", plot = p.comb, 
       width = 4, heigh = 6.5, units = "in", dpi = "retina")


# subset interactions by zone ---------------------------------------------

## summarize the interactions by zone of source and target taxa
interactions_comb2 %>% 
  group_by(sourceTaxon_zone, targetTaxon_zone) %>% 
  count()
#   sourceTaxon_zone targetTaxon_zone     n
# 1 Both             Both                36
# 2 Both             Intertidal          31
# 3 Both             Subtidal            49
# 4 Intertidal       Both                39
# 5 Intertidal       Intertidal          53
# 6 Intertidal       Subtidal            53
# 7 Subtidal         Both                80
# 8 Subtidal         Intertidal          81
# 9 Subtidal         Subtidal           126

## COMMENT: 
## How many subtidal/both interactions should there be?
## Both+Both + Both+Subtidal + Subtidal+Both + Subtidal+Subtidal
## 36 + 49 + 80 + 126 = 291 <--- nrow(interactions_sub) below
## 391 / 548 = 53% of interactions set
##
## How many intertidal/both interactions should there be?
## Both+Both + Both+Intertidal + Intertidal+Both + Intertidal+Intertidal
## 36 + 31 + 39 + 53 = 159 <--- nrow(interactions_int) below
## 159 / 548 = 29% of interactions set
##
## How many interactions span the subtidal and intertidal?
## Intertidal+Subtidal + Subtidal+Intertidal
## 53 + 81 = 134 
## 134 / 548 = 24% of interactions set
## ^^ this is the percentage missed when you treat the subtidal
## and intertidal as separate networks! many of these interactions
## are likely missed in the observational data because of they involve
## mobile organisms *can* be found in both habitats but only enter
## intertidal at high tide (hence, missed in sampling protocol)
## for a "observed" Subtidal organism to interact with an "observed"
## Intertidal organism, they must move into the other zone (so should
## technically be classified as "Both")

## subset the interactions to include only those that occur between
## species found in the subtidal (or both)
interactions_sub <- interactions_comb2 %>% 
  filter(sourceTaxon_zone %in% c("Subtidal", "Both") & 
           targetTaxon_zone %in% c("Subtidal", "Both"))
nrow(interactions_sub) ## [1] 291

## subset the interaction to include only those that occur between 
## species found in the intertidal (or both)
interactions_int <- interactions_comb2 %>% 
  filter(sourceTaxon_zone %in% c("Intertidal", "Both") & 
           targetTaxon_zone %in% c("Intertidal", "Both"))
nrow(interactions_int) ## [1] 159

# subtidal interaction network --------------------------------------------

## create a graph combing subtidal and intertidal communities
sub_graph <- igraph::graph_from_data_frame(interactions_sub)

## inspect vertex (node) attributes
# vertex_attr(combined_graph)
vertex_attr_names(sub_graph)
# [1] "name"

V(sub_graph)$name

## add an attribute for zone (subtidal, intertidal, or both)
graph_sp_sub <- tibble(name = V(sub_graph)$name) %>% 
  left_join(., distinct(sp_zone2[c("organism", "sp_zone")]), 
            by = c("name" = "organism"))
V(sub_graph)$zone <- graph_sp_sub$sp_zone

## add attribute for interaction type (6 possible options)
unique(interactions_sub$interactionTypeName)
# [1] "preys on"    "eats"        "epibiont of"
V(sub_graph)$interaction_type <- interactions_sub$interactionTypeName

## calculate and add an attribute for degree 
## (i.e., how many connections are made to each node?)
V(sub_graph)$degree <- igraph::degree(sub_graph, 
                                      ## scale by 0.1
                                      mode = "all")
## use the degree as a weighting variable for plotting nodes
V(sub_graph)$weight <- igraph::degree(sub_graph, 
                                      ## scale by 0.1
                                      mode = "all") * 0.1

## inspect edge (link) attributes
# edge_attr(combined_graph)
edge_attr_names(sub_graph)

## convert the graph into a tidy object using {tidygraph}
tidy_sub_g <- tidygraph::as_tbl_graph(sub_graph)
tidy_sub_g

## plot the graph using {ggraph}
tidy_sub_g %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  ggraph(layout = "kk") + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 alpha = 0.5, width = 0.5) + 
  scale_fill_manual(values = net_pal, name = "Zone") + 
  # scale_fill_brewer(palette = "Dark2", name = "Zone") + 
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = FALSE, 
         fill = guide_legend("Zone"), 
         linetype = guide_legend(NULL)) + 
  theme(legend.position = "bottom", 
        legend.box = "vertical")

## create a layout for the graph
layout_sub <- create_layout(tidy_sub_g, 
                                 layout = "kk") %>% 
  rowwise() %>% 
  ## change x-axis values from layout so that 
  ## species are arranged/grouped by the zone they occupy
  mutate(x = case_when(zone == "Subtidal" ~ 
                         rnorm(1, mean = 1, sd = 0.2), 
                       zone == "Both" ~ 
                         rnorm(1, mean = 2, sd = 0.2))) 

## recreate the layout and add the previously recalculated x values
## NOTE: need to do this because the output above is a tibble, but
## we need the layout to be of class: "layout_tbl_graph"
layout_sub2 <- create_layout(tidy_sub_g, 
                                  layout = "kk")
## add specified x and y values (no doing anything with y here)
layout_sub2$x <- layout_sub$x
layout_sub2$y <- layout_sub$y
## check class of layout object
class(layout_sub2)

## replot the network
p.sub <- layout_sub2 %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  ggraph() + 
  # geom_edge_hive(aes(linetype = interactionTypeName),
  #                colour = "grey30", alpha = 0.5, width = 0.5) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey30", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  scale_fill_manual(values = net_pal, name = "Zone") + 
  # scale_fill_brewer(palette = "Dark2", name = "Zone") +
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = FALSE) + 
  labs(subtitle = "Subtidal foodweb at Appledore") + 
  theme(legend.position = "bottom", 
        legend.box = "vertical")

ggsave(filename = "Plots/network_subtidal.png", plot = p.sub, 
       width = 4, heigh = 6.5, units = "in", dpi = "retina")


# intertidal interaction network ------------------------------------------

## create a graph combing subtidal and intertidal communities
int_graph <- igraph::graph_from_data_frame(interactions_int)

## inspect vertex (node) attributes
# vertex_attr(combined_graph)
vertex_attr_names(int_graph)
# [1] "name"

V(int_graph)$name

## add an attribute for zone (subtidal, intertidal, or both)
graph_sp_int <- tibble(name = V(int_graph)$name) %>% 
  left_join(., distinct(sp_zone2[c("organism", "sp_zone")]), 
            by = c("name" = "organism"))
V(int_graph)$zone <- graph_sp_int$sp_zone

## add attribute for interaction type (6 possible options)
unique(interactions_int$interactionTypeName)
# [1] "preys on"    "eats"        "epibiont of"
V(int_graph)$interaction_type <- interactions_int$interactionTypeName

## calculate and add an attribute for degree 
## (i.e., how many connections are made to each node?)
V(int_graph)$degree <- igraph::degree(int_graph, 
                                      ## scale by 0.1
                                      mode = "all")
## use the degree as a weighting variable for plotting nodes
V(int_graph)$weight <- igraph::degree(int_graph, 
                                      ## scale by 0.1
                                      mode = "all") * 0.1

## inspect edge (link) attributes
# edge_attr(combined_graph)
edge_attr_names(int_graph)

## convert the graph into a tidy object using {tidygraph}
tidy_int_g <- tidygraph::as_tbl_graph(int_graph)
tidy_int_g

## plot the graph using {ggraph}
tidy_int_g %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  ggraph(layout = "kk") + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 alpha = 0.5, width = 0.5) + 
  scale_fill_manual(values = net_pal[2:3], name = "Zone") + 
  # scale_fill_brewer(palette = "Dark2", name = "Zone") + 
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = FALSE, 
         fill = guide_legend("Zone"), 
         linetype = guide_legend(NULL)) + 
  theme(legend.position = "bottom", 
        legend.box = "vertical")

## create a layout for the graph
layout_int <- create_layout(tidy_int_g, 
                            layout = "kk") %>% 
  rowwise() %>% 
  ## change x-axis values from layout so that 
  ## species are arranged/grouped by the zone they occupy
  mutate(x = case_when(zone == "Both" ~ 
                         rnorm(1, mean = 1, sd = 0.2), 
                       zone == "Intertidal" ~ 
                         rnorm(1, mean = 2, sd = 0.2))) 

## recreate the layout and add the previously recalculated x values
## NOTE: need to do this because the output above is a tibble, but
## we need the layout to be of class: "layout_tbl_graph"
layout_int2 <- create_layout(tidy_int_g, 
                             layout = "kk")
## add specified x and y values (no doing anything with y here)
layout_int2$x <- layout_int$x
layout_int2$y <- layout_int$y
## check class of layout object
class(layout_int2)

## replot the network
p.int <- layout_int2 %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  ggraph() + 
  # geom_edge_hive(aes(linetype = interactionTypeName),
  #                colour = "grey30", alpha = 0.5, width = 0.5) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey30", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  scale_fill_manual(values = net_pal[2:3], name = "Zone") + 
  # scale_fill_brewer(palette = "Dark2", name = "Zone") +
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = FALSE, fill = guide_legend(order = 1)) + 
  labs(subtitle = "Intertidal foodweb at Appledore") + 
  theme(legend.position = "bottom", 
        legend.box = "vertical")

ggsave(filename = "Plots/network_intertidal.png", plot = p.int, 
       width = 4, heigh = 6.5, units = "in", dpi = "retina")


# plot all three networks together ----------------------------------------

## using patchwork
(p.sub + p.int) / p.comb


# representation of Appledore species in networks -------------------------

## how many unique species are in the species list?
sp_zone2 %>% 
  group_by(sp_zone) %>% 
  summarise(n_sp = n_distinct(organism))
# sp_zone     n_sp
# 1 Both          25
# 2 Intertidal    35
# 3 Subtidal      84

## COMMENT:
## so there are:
## 144 total species (25+25+84)
## 109 subtidal species (25+84)
## 60 intertidal species (25+35)

## how many species are in each network?
## combined
n_distinct(V(combined_graph)$name) ## [1] 101
101/144 ## [1] 0.7013889
## COMMENT:
## our interactions dataset include ~70% of species found at Appledore

## subtidal
n_distinct(V(sub_graph)$name) ## [1] 71
71/109 ## [1] 0.6513761
## COMMENT:
## subtidal interactions include ~65% of species found in subtidal

## intertidal
n_distinct(V(int_graph)$name) ## [1] 45
45/60 ## [1] 0.75
## intertidal interactions include 75% of species found in intertidal


## other goals?
## create a graph of only interactions including a species found in 
## both zones (excluding species that only interact with a species 
## that isn't found in both zones)


# node centrality ---------------------------------------------------------

tidy_combined_g %>% mutate(centrality = centrality_authority())
