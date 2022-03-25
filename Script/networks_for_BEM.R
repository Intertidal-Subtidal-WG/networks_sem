## networks for BEM presentation
## Created by: Joey Burant
## Last updated: 22 March 2022

## set a random seed for plotting
set.seed(4444)

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
library(ggforce)
# library(concaveman)
library(ggnewscale)

## set colour palette
## want these three colour but in a slightly different order
## green = subtidal, orange = intertidal, purple = subtidal
# net_pal <- RColorBrewer::brewer.pal("Dark2", n = 3)[c(1,3,2)]
net_pal <- c("red", "grey40", "blue")

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

# ## add an edge attribute for interaction type (6 possible options)
# unique(interactions_comb2$interactionTypeName)
# # [1] "preys on"    "eats"        "epibiont of"
# E(combined_graph)$interaction_type <- interactions_comb2$interactionTypeName

## convert the graph into a tidy object using {tidygraph}
tidy_combined_g <- tidygraph::as_tbl_graph(combined_graph)
tidy_combined_g

## plot the graph using {ggraph}
tidy_combined_g %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  # ggraph(layout = "grid") + 
  # ggraph(layout = "star") +
  ggraph(layout = "kk") +
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  geom_edge_link(aes(linetype = interactionTypeName),
                 colour = "grey50", alpha = 0.5, width = 0.5) +
  # scale_fill_manual(values = net_pal[c(2,2,2)], name = "Zone") +
  scale_fill_manual(values = net_pal, name = "Zone") +
  # scale_fill_brewer(palette = "Dark2", name = "Zone") + 
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend("Zone"), 
         linetype = guide_legend(NULL)) + 
  theme(#legend.position = "bottom", 
        # legend.position = "right",
        legend.position = "none",
        legend.box = "vertical", 
        panel.background = element_blank(), 
        plot.background = element_blank())

ggsave(filename = "Plots/network_combined_2.png", plot = last_plot(), 
       width = 5, height = 5, units = "in", dpi = "retina")

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
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  scale_fill_manual(values = net_pal, name = "Zone") + 
  # scale_fill_brewer(palette = "Dark2", name = "Zone") +
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend(order = 1)) + 
  # labs(subtitle = "Integrated nearshore foodweb at Appledore") + 
  theme(#legend.position = "bottom", 
        # legend.position = "right",
        legend.position = "none",
        legend.box = "vertical", 
        panel.background = element_blank(), 
        plot.background = element_blank())

ggsave(filename = "Plots/network_combined_3.png", plot = p.comb, 
       width = 5, height = 5, units = "in", dpi = "retina")


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
36 + 49 + 80 + 126 ## [1] 291 <--- nrow(interactions_sub) below
291 / 548 ## [1] 53% of interactions set
##
## How many intertidal/both interactions should there be?
## Both+Both + Both+Intertidal + Intertidal+Both + Intertidal+Intertidal
36 + 31 + 39 + 53 ## [1] 159 <--- nrow(interactions_int) below
159 / 548 ## [1] 29% of interactions set

## How many interaction occur between species found in both zones?
## Include their first-degree interactions with species in either zone.
## Both+Both + Both+Subtidal + Both+Intertidal + Subtidal+Both + Intertidal+Both
36 + 49 + 31 + 80 + 39 ## [1] 235
##
## How many interactions span the subtidal and intertidal?
## Intertidal+Subtidal + Subtidal+Intertidal
53 + 81 ## [1] 134 
134 / 548 ## [1] 24% of interactions set
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

# ## add edge attribute for interaction type (6 possible options)
# unique(interactions_sub$interactionTypeName)
# # [1] "preys on"    "eats"        "epibiont of"
# E(sub_graph)$interaction_type <- interactions_sub$interactionTypeName

## convert the graph into a tidy object using {tidygraph}
tidy_sub_g <- tidygraph::as_tbl_graph(sub_graph)
tidy_sub_g

## plot the graph using {ggraph}
tidy_sub_g %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  # ggraph(layout = "grid") + 
  # ggraph(layout = "star") + 
  ggraph(layout = "kk") +
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  # scale_fill_manual(values = net_pal[c(1,1)], name = "Zone") +
  scale_fill_manual(values = net_pal, name = "Zone") +
  # scale_fill_brewer(palette = "Dark2", name = "Zone") + 
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend("Zone"), 
         linetype = guide_legend(NULL)) + 
  theme(#legend.position = "bottom", 
        # legend.position = "right", 
        legend.position = "none",
        legend.box = "vertical", 
        panel.background = element_blank(), 
        plot.background = element_blank())

ggsave(filename = "Plots/network_subtidal_2.png", plot = last_plot(), 
       width = 5, height = 5, units = "in", dpi = "retina")

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
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  # scale_fill_manual(values = net_pal[c(1,1)], name = "Zone") +
  scale_fill_manual(values = net_pal, name = "Zone") +
  # scale_fill_brewer(palette = "Dark2", name = "Zone") +
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none") + 
  # labs(subtitle = "Subtidal foodweb at Appledore") + 
  theme(#legend.position = "bottom", 
        # legend.position = "right", 
        legend.position = "none",
        legend.box = "vertical", 
        panel.background = element_blank(), 
        plot.background = element_blank())

ggsave(filename = "Plots/network_subtidal_3.png", plot = p.sub, 
       width = 5, height = 5, units = "in", dpi = "retina")


# intertidal interaction netwwork ------------------------------------------

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

# ## add an edge attribute for interaction type (6 possible options)
# unique(interactions_int$interactionTypeName)
# # [1] "preys on"    "eats"        "epibiont of"
# E(int_graph)$interaction_type <- interactions_int$interactionTypeName

## convert the graph into a tidy object using {tidygraph}
tidy_int_g <- tidygraph::as_tbl_graph(int_graph)
tidy_int_g

## plot the graph using {ggraph}
tidy_int_g %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  # ggraph(layout = "grid") + 
  # ggraph(layout = "star") + 
  ggraph(layout = "kk") + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  # scale_fill_manual(values = net_pal[c(3,3)], name = "Zone") +
  scale_fill_manual(values = net_pal[2:3], name = "Zone") +
  # scale_fill_brewer(palette = "Dark2", name = "Zone") + 
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend("Zone"), 
         linetype = guide_legend(NULL)) + 
  theme(#legend.position = "bottom", 
        # legend.position = "right", 
        legend.position = "none",
        legend.box = "vertical", 
        panel.background = element_blank(), 
        plot.background = element_blank())

ggsave(filename = "Plots/network_intertidal_2.png", plot = last_plot(), 
       width = 5, height = 5, units = "in", dpi = "retina")

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
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  # scale_fill_manual(values = net_pal[c(3,3)], name = "Zone") +
  scale_fill_manual(values = net_pal[2:3], name = "Zone") +
  # scale_fill_brewer(palette = "Dark2", name = "Zone") +
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend(order = 1)) + 
  # labs(subtitle = "Intertidal foodweb at Appledore") + 
  theme(#legend.position = "bottom", 
        # legend.position = "right", 
        legend.position = "none",
        legend.box = "vertical", 
        panel.background = element_blank(), 
        plot.background = element_blank())

ggsave(filename = "Plots/network_intertidal_3.png", plot = p.int, 
       width = 5, height = 5, units = "in", dpi = "retina")


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
101 / 144 ## [1] 0.7013889
## COMMENT:
## our interactions dataset include ~70% of species found at Appledore

## subtidal
n_distinct(V(sub_graph)$name) ## [1] 71
71 / 109 ## [1] 0.6513761
## COMMENT:
## subtidal interactions include ~65% of species found in subtidal

## intertidal
n_distinct(V(int_graph)$name) ## [1] 45
45 / 60 ## [1] 0.75
## intertidal interactions include 75% of species found in intertidal


# network metrics ---------------------------------------------------------

## node centrality
tidy_combined_g <- ## combined network
  tidy_combined_g %>% 
  mutate(centralityA = centrality_authority(), 
         centralityB = centrality_betweenness(), 
         centralityE = centrality_eigen())

tidy_sub_g <- ## subtidal network
  tidy_sub_g %>%
  mutate(centralityA = centrality_authority(), 
         centralityB = centrality_betweenness(), 
         centralityE = centrality_eigen())


tidy_int_g <- ## intertidal network
  tidy_int_g %>%
  mutate(centralityA = centrality_authority(), 
         centralityB = centrality_betweenness(), 
         centralityE = centrality_eigen())


## assortativity (by zone)
tidy_combined_g %>% ## combined network
  mutate(assortativity = graph_assortativity(attr = zone)) %>% 
  pull(assortativity) %>% 
  head(1) ## [1] 0.05430575

tidy_sub_g %>% ## subtidal network
  mutate(assortativity = graph_assortativity(attr = zone)) %>% 
  pull(assortativity) %>% 
  head(1) ## [1] 0.03177633

tidy_int_g %>% ## intertidal network
  mutate(assortativity = graph_assortativity(attr = zone)) %>% 
  pull(assortativity) %>% 
  head(1) ## [1] 0.11159

## community detection with Louvain algorithm
## ONLY WORKS FOR UNDIRECTED GRAPHS
# tidy_combined_g <- ## combined network
#   tidy_combined_g %>% 
#   activate(nodes) %>% 
#   # mutate(community = as.character(group_louvain())) %>% 
#   mutate(community=as.character(group_fast_greedy())) %>% 
#   mutate(modularity = graph_modularity(group=as.factor(zone))) %>% 
#   pull(modularity) %>% 
#   head(1)

## k-core decomposition
tidy_combined_g %>% 
  mutate(kcore=node_coreness()) %>% 
  ggraph("fr") + 
  geom_edge_link() + 
  geom_node_point(aes(colour = kcore), size = 3) + 
  scale_colour_viridis_c() + 
  theme_graph()

tidy_sub_g %>% 
  mutate(kcore=node_coreness()) %>% 
  ggraph("fr") + 
  geom_edge_link() + 
  geom_node_point(aes(colour = kcore), size = 3) + 
  scale_colour_viridis_c() + 
  theme_graph()

tidy_int_g %>% 
  mutate(kcore=node_coreness()) %>% 
  ggraph("fr") + 
  geom_edge_link() + 
  geom_node_point(aes(colour = kcore), size = 3) + 
  scale_colour_viridis_c() + 
  theme_graph()

## density = proportion of possible connections that exist
edge_density(combined_graph) ## [1] 0.05425743
edge_density(sub_graph) ## [1] 0.05855131
edge_density(int_graph) ## [1] 0.08030303

## diameter = longest shortest path across the network
with_graph(tidy_combined_g, graph_diameter()) ## [1] 8
with_graph(tidy_sub_g, graph_diameter()) ## [1] 9
with_graph(tidy_int_g, graph_diameter()) ## [1] 4

## mean distance between two nodes
with_graph(tidy_combined_g, graph_mean_dist()) ## [1] 2.437579
with_graph(tidy_sub_g, graph_mean_dist()) ## [1] 2.368
with_graph(tidy_int_g, graph_mean_dist()) ## [1] 1.626506

## transitivity = probability for adjacent nodes to be interconnected
transitivity(combined_graph) ## [1] 0.2091335
transitivity(sub_graph) ## [1] 0.2496075
transitivity(int_graph) ## [1] 0.2003515

## summary of degree (number of connections per node)
tidy_combined_g %>% ## combined network 
  as_tibble() %>% 
  # group_by(zone) %>% 
  summarise(median = median(degree), 
            mean_deg = mean(degree), 
            sd_deg = sd(degree), 
            min = min(degree), 
            max = max(degree))
# median mean_deg sd_deg   min   max
#      7     10.9   11.7     1    72

as_tibble(tidy_combined_g) %>% 
  qplot(x = degree, geom = "histogram", data = .) + 
  ggtitle("node degree - combined network")

tidy_sub_g %>% ## subtidal network 
  as_tibble() %>% 
  # group_by(zone) %>% 
  summarise(median = median(degree), 
            mean_deg = mean(degree), 
            sd_deg = sd(degree), 
            min = min(degree), 
            max = max(degree))
# median mean_deg sd_deg   min   max
#      4     8.20   9.99     1    61

as_tibble(tidy_sub_g) %>% 
  qplot(x = degree, geom = "histogram", data = .) + 
  ggtitle("node degree - subtidal network")

tidy_int_g %>% ## combined network 
  as_tibble() %>% 
  # group_by(zone) %>% 
  summarise(median = median(degree), 
            mean_deg = mean(degree), 
            sd_deg = sd(degree), 
            min = min(degree), 
            max = max(degree))
# median mean_deg sd_deg   min   max
#      5     7.07   7.04     1    30

as_tibble(tidy_int_g) %>% 
  qplot(x = degree, geom = "histogram", data = .) + 
  ggtitle("node degree - intertidal network")

## average centrality
tidy_combined_g %>% ## combined network 
  as_tibble() %>% 
  # group_by(zone) %>% 
  summarise(median = median(centralityE), 
            mean_deg = mean(centralityE), 
            sd_deg = sd(centralityE), 
            min = min(centralityE), 
            max = max(centralityE))
#   median mean_deg sd_deg     min   max
# 1  0.159    0.204  0.191 0.00210     1

as_tibble(tidy_combined_g) %>% 
  qplot(x = centralityE, geom = "histogram", data = .) + 
  ggtitle("node centrality - combined network")

tidy_sub_g %>% ## subtidal network 
  as_tibble() %>% 
  # group_by(zone) %>% 
  summarise(median = median(centralityE), 
            mean_deg = mean(centralityE), 
            sd_deg = sd(centralityE), 
            min = min(centralityE), 
            max = max(centralityE))
#   median mean_deg sd_deg     min   max
# 1  0.130    0.215  0.221 0.00320     1

as_tibble(tidy_sub_g) %>% 
  qplot(x = centralityE, geom = "histogram", data = .) + 
  ggtitle("node centrality - subtidal network")

tidy_int_g %>% ## combined network 
  as_tibble() %>% 
  # group_by(zone) %>% 
  summarise(median = median(centralityE), 
            mean_deg = mean(centralityE), 
            sd_deg = sd(centralityE), 
            min = min(centralityE), 
            max = max(centralityE))
#   median mean_deg sd_deg     min   max
# 1  0.241    0.302  0.249 0.00921     1

as_tibble(tidy_int_g) %>% 
  qplot(x = centralityE, geom = "histogram", data = .) + 
  ggtitle("node centrality - intertidal network")


# additional exploration --------------------------------------------------

## other goals?


# the "missing" network directly linking zones ----------------------------

## create a graph of interactions that span the subtidal and intertidal
## (i.e., the interactions you would miss by treating the zones separately)

## subset interactions to those between strictly subtidal and strictly 
## intertidal taxa, and vice verse
interactions_span <- interactions_comb2 %>% 
  filter(sourceTaxon_zone %in% c("Subtidal") & 
           targetTaxon_zone %in% c("Intertidal") | 
           sourceTaxon_zone %in% c("Intertidal") & 
           targetTaxon_zone %in% c("Subtidal"))
nrow(interactions_span) ## [1] 134
## this aligns with calculation above

## create a graph combing subtidal and intertidal communities
span_graph <- igraph::graph_from_data_frame(interactions_span)

## inspect vertex (node) attributes
# vertex_attr(combined_graph)
vertex_attr_names(span_graph)
# [1] "name"

V(span_graph)$name

## add an attribute for zone (subtidal, intertidal, or both)
graph_sp_span <- tibble(name = V(span_graph)$name) %>% 
  left_join(., distinct(sp_zone2[c("organism", "sp_zone")]), 
            by = c("name" = "organism"))
V(span_graph)$zone <- graph_sp_span$sp_zone

## calculate and add an attribute for degree 
## (i.e., how many connections are made to each node?)
V(span_graph)$degree <- igraph::degree(span_graph, 
                                       ## scale by 0.1
                                       mode = "all")
## use the degree as a weighting variable for plotting nodes
V(span_graph)$weight <- igraph::degree(span_graph, 
                                       ## scale by 0.1
                                       mode = "all") * 0.1

## inspect edge (link) attributes
# edge_attr(combined_graph)
edge_attr_names(span_graph)

# ## add attribute for interaction type (6 possible options)
# unique(interactions_span$interactionTypeName)
# # [1] "preys on"    "eats"        "epibiont of"
# V(span_graph)$interaction_type <- interactions_span$interactionTypeName


## convert the graph into a tidy object using {tidygraph}
tidy_span_g <- tidygraph::as_tbl_graph(span_graph)
tidy_span_g

## plot the graph using {ggraph}
tidy_span_g %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  # ggraph(layout = "grid") + 
  # ggraph(layout = "star") + 
  ggraph(layout = "kk") + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  scale_fill_manual(values = net_pal[c(1,3)], name = "Zone") + 
  # scale_fill_brewer(palette = "Dark2", name = "Zone") + 
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend("Zone"), 
         linetype = guide_legend(NULL)) + 
  theme(#legend.position = "bottom", 
    # legend.position = "right",
    legend.position = "none",
    legend.box = "vertical", 
    panel.background = element_blank(), 
    plot.background = element_blank())

ggsave(filename = "Plots/network_span_1.png", plot = last_plot(), 
       width = 5, height = 5, units = "in", dpi = "retina")

## create a layout for the graph
layout_span <- create_layout(tidy_span_g, 
                             layout = "kk") %>% 
  rowwise() %>% 
  ## change x-axis values from layout so that 
  ## species are arranged/grouped by the zone they occupy
  mutate(x = case_when(zone == "Subtidal" ~ 
                         rnorm(1, mean = 1, sd = 0.2), 
                       zone == "Intertidal" ~ 
                         rnorm(1, mean = 2, sd = 0.2))) 

## recreate the layout and add the previously recalculated x values
## NOTE: need to do this because the output above is a tibble, but
## we need the layout to be of class: "layout_tbl_graph"
layout_span2 <- create_layout(tidy_span_g, 
                              layout = "kk")
## add specified x and y values (no doing anything with y here)
layout_span2$x <- layout_span$x
layout_span2$y <- layout_span$y
## check class of layout object
class(layout_span2)

## replot the network
p.span <- layout_span2 %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  ggraph() + 
  # geom_edge_hive(aes(linetype = interactionTypeName),
  #                colour = "grey30", alpha = 0.5, width = 0.5) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  # scale_fill_manual(values = net_pal[c(3,3)], name = "Zone") +
  scale_fill_manual(values = net_pal[c(1,3)], name = "Zone") +
  # scale_fill_brewer(palette = "Dark2", name = "Zone") +
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend(order = 1)) + 
  # labs(subtitle = "The missing network linking the subtidal and intertidal") +
  theme(#legend.position = "bottom", 
    # legend.position = "right", 
    legend.position = "none",
    legend.box = "vertical", 
    panel.background = element_blank(), 
    plot.background = element_blank())

ggsave(filename = "Plots/network_span_2.png", plot = p.span, 
       width = 5, height = 5, units = "in", dpi = "retina")


# network of species found in both habitats, and their links --------------

## create a graph of only interactions including a species found in 
## both zones (excluding species that only interact with a species 
## that isn't found in both zones)

## subset interactions to those between species found in both, and 
## their first degree links (i.e. direct interactions with species 
## found strictly in subtidal or intertidal zones)
## intertidal taxa, and vice verse
interactions_both <- interactions_comb2 %>% 
  # ## interactions between species that occur in both zones (exclusive)
  # filter(sourceTaxon_zone %in% c("Both") &
  #          targetTaxon_zone %in% c("Both"))
  ## interactions involving at least one species found in both zones
  filter(sourceTaxon_zone %in% c("Both") |
           targetTaxon_zone %in% c("Both"))
nrow(interactions_both) ## [1] 235 ## [1] 36

## COMMENT:
## use the & "AND" script version of the subsetting code above to 
## produce the network involving only species that occur in both zones
## (but not their other interactions)
## use the | "OR" script version to produce a network of species species
## that occur in both zones, as well as their direct interactions with
## zone-restricted species

## create a graph combing subtidal and intertidal communities
both_graph <- igraph::graph_from_data_frame(interactions_both)

## inspect vertex (node) attributes
# vertex_attr(combined_graph)
vertex_attr_names(both_graph)
# [1] "name"

V(both_graph)$name

## add an attribute for zone (subtidal, intertidal, or both)
graph_sp_both <- tibble(name = V(both_graph)$name) %>% 
  left_join(., distinct(sp_zone2[c("organism", "sp_zone")]), 
            by = c("name" = "organism"))
V(both_graph)$zone <- graph_sp_both$sp_zone

## calculate and add an attribute for degree 
## (i.e., how many connections are made to each node?)
V(both_graph)$degree <- igraph::degree(both_graph, 
                                       ## scale by 0.1
                                       mode = "all")
## use the degree as a weighting variable for plotting nodes
V(both_graph)$weight <- igraph::degree(both_graph, 
                                       ## scale by 0.1
                                       mode = "all") * 0.1

## inspect edge (link) attributes
# edge_attr(combined_graph)
edge_attr_names(both_graph)

# ## add attribute for interaction type (6 possible options)
# unique(interactions_both$interactionTypeName)
# # [1] "preys on"    "eats"        "epibiont of"
# V(both_graph)$interaction_type <- interactions_both$interactionTypeName

## convert the graph into a tidy object using {tidygraph}
tidy_both_g <- tidygraph::as_tbl_graph(both_graph)
tidy_both_g

## plot the graph using {ggraph}
tidy_both_g %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  # ggraph(layout = "grid") + 
  # ggraph(layout = "star") + 
  ggraph(layout = "kk") + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  scale_fill_manual(values = net_pal, name = "Zone") + 
  # scale_fill_brewer(palette = "Dark2", name = "Zone") + 
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend("Zone"), 
         linetype = guide_legend(NULL)) + 
  theme(#legend.position = "bottom", 
    # legend.position = "right",
    legend.position = "none",
    legend.box = "vertical", 
    panel.background = element_blank(), 
    plot.background = element_blank())

ggsave(filename = "Plots/network_both_1.png", plot = last_plot(), 
       width = 5, height = 5, units = "in", dpi = "retina")

## create a layout for the graph
layout_both <- create_layout(tidy_both_g, 
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
layout_both2 <- create_layout(tidy_both_g, 
                              layout = "kk")
## add specified x and y values (no doing anything with y here)
layout_both2$x <- layout_both$x
layout_both2$y <- layout_both$y
## check class of layout object
class(layout_both2)

## replot the network
p.both <- layout_both2 %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", "Both", "Intertidal"))) %>% 
  ggraph() + 
  # geom_edge_hive(aes(linetype = interactionTypeName),
  #                colour = "grey30", alpha = 0.5, width = 0.5) +
  geom_edge_link(aes(linetype = interactionTypeName), 
                 colour = "grey50", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = weight), shape = 21) +
  # scale_fill_manual(values = net_pal[c(3,3)], name = "Zone") +
  scale_fill_manual(values = net_pal, name = "Zone") +
  # scale_fill_brewer(palette = "Dark2", name = "Zone") +
  # geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) +
  scale_edge_linetype(name = "Interaction\ntype") + 
  theme_graph() + 
  guides(size = "none", 
         fill = guide_legend(order = 1)) + 
  # labs(subtitle = "Network of species found in both zones \n(and 1st-degree links)") +
  theme(#legend.position = "bottom", 
    # legend.position = "right", 
    legend.position = "none",
    legend.box = "vertical", 
    panel.background = element_blank(), 
    plot.background = element_blank())

ggsave(filename = "Plots/network_both_2.png", plot = p.both, 
       width = 5, height = 5, units = "in", dpi = "retina")


# key players in the networks ---------------------------------------------

## look at eigen centrality and degree (related but not the same thing)
## use top 5 species as examples
## combined
as_tibble(tidy_combined_g) %>% 
  select(name, zone, centralityE) %>% 
  arrange(desc(centralityE)) #%>% 
  # mutate(x = 1:nrow(.)) %>% 
  # ggplot() + 
  # geom_point(aes(x = x, y = centralityB))
## COMMENT: 7 species with centrality > 0.50
#  name                              zone       centralityE
# 1 Strongylocentrotus droebachiensis Both             1    
# 2 Cancer irroratus                  Subtidal         0.783
# 3 Homarus americanus                Subtidal         0.765
# 4 Carcinus maenas                   Both             0.635
# 5 Mytilus edulis                    Both             0.572

as_tibble(tidy_combined_g) %>% 
  select(name, zone, degree) %>% 
  arrange(desc(degree))
## COMMENT: 17 species with degree > 20
# name                              zone       degree
# 1 Strongylocentrotus droebachiensis Both           72
# 2 Carcinus maenas                   Both           43
# 3 Homarus americanus                Subtidal       40
# 4 Cancer irroratus                  Subtidal       39
# 5 Littorina littorea                Intertidal     37

## subtidal
as_tibble(tidy_sub_g) %>% 
  select(name, zone, centralityE) %>% 
  arrange(desc(centralityE))
## COMMENT: 9 species with centrality > 0.50
#   name                              zone     centralityE
# 1 Strongylocentrotus droebachiensis Both           1    
# 2 Cancer irroratus                  Subtidal       0.859
# 3 Homarus americanus                Subtidal       0.824
# 4 Myoxocephalus                     Subtidal       0.674
# 5 Mytilus edulis                    Both           0.643

as_tibble(tidy_sub_g) %>% 
  select(name, zone, degree) %>% 
  arrange(desc(degree))
## COMMENT: 7 species with degree > 20
#   name                              zone     degree
# 1 Strongylocentrotus droebachiensis Both         61
# 2 Cancer irroratus                  Subtidal     31
# 3 Homarus americanus                Subtidal     31
# 4 Carcinus maenas                   Both         28
# 5 Mytilus edulis                    Both         25

## intertidal
as_tibble(tidy_int_g) %>% 
  select(name, zone, centralityE) %>% 
  arrange(desc(centralityE))
## COMMENT: 9 species with centrality > 0.50
#   name                              zone       centralityE
# 1 Carcinus maenas                   Both             1    
# 2 Littorina littorea                Intertidal       0.825
# 3 Nucella lapillus                  Intertidal       0.815
# 4 Strongylocentrotus droebachiensis Both             0.751
# 5 Ascophyllum nodosum               Both             0.563

as_tibble(tidy_int_g) %>% 
  select(name, zone, degree) %>% 
  arrange(desc(degree))
## COMMENT: 4 species with degree > 20
#   name                              zone       degree
# 1 Carcinus maenas                   Both           30
# 2 Strongylocentrotus droebachiensis Both           25
# 3 Littorina littorea                Intertidal     23
# 4 Nucella lapillus                  Intertidal     23
# 5 Hemigrapsus sanguineus            Intertidal     16

## span
tidy_span_g <- tidy_span_g %>%
  mutate(centralityA = centrality_authority(), 
         centralityB = centrality_betweenness(), 
         centralityE = centrality_eigen())

as_tibble(tidy_span_g) %>% 
  select(name, zone, centralityE) %>% 
  arrange(desc(centralityE))
## COMMENT: 11 species with centrality > 0.50
#  name                   zone       centralityE
# 1 Lacuna vincta          Intertidal       1    
# 2 Littorina littorea     Intertidal       0.888
# 3 Saccharina latissima   Subtidal         0.871
# 4 Littorina obtusata     Intertidal       0.810
# 5 Ulvaria                Subtidal         0.679

as_tibble(tidy_span_g) %>% 
  select(name, zone, degree) %>% 
  arrange(desc(degree))
## COMMENT: 8 species with degree > 10 
#  name                   zone       degree
# 1 Lacuna vincta          Intertidal     19
# 2 Littorina littorea     Intertidal     14
# 3 Semibalanus balanoides Intertidal     13
# 4 Littorina saxatilis    Intertidal     12
# 5 Isopods                Intertidal     12

## both
tidy_both_g <- tidy_both_g %>%
  mutate(centralityA = centrality_authority(), 
         centralityB = centrality_betweenness(), 
         centralityE = centrality_eigen())

as_tibble(tidy_both_g) %>% 
  select(name, zone, centralityE) %>% 
  arrange(desc(centralityE))
## COMMENT: 2 species with centrality > 0.49
#   name                              zone       centralityE
# 1 Strongylocentrotus droebachiensis Both             1    
# 2 Carcinus maenas                   Both             0.514
# 3 Mytilus edulis                    Both             0.491
# 4 Homarus americanus                Subtidal         0.386
# 5 Ulvaria                           Subtidal         0.278

as_tibble(tidy_both_g) %>% 
  select(name, zone, degree) %>% 
  arrange(desc(degree))
## COMMENT: 3 species with degree > 20
#   name                              zone       degree
# 1 Strongylocentrotus droebachiensis Both           72
# 2 Carcinus maenas                   Both           43
# 3 Mytilus edulis                    Both           28
# 4 Amphipoda                         Both           18
# 5 Testudinalia testudinalis         Both           15


# -------------------------------------------------------------------------


a <- sort(as_tibble(tidy_both_g)$name)
b <- sort(filter(as_tibble(tidy_combined_g), zone == "Both")$name)



# modularity analysis -----------------------------------------------------

tidy_combined_g %>%
  to_undirected() %>%
  mutate(modularity = graph_modularity(group=as.factor(zone))) %>%
  pull(modularity) %>%
  head(1) ## [1] 0.03039753

tidy_combined_g2 <- tidy_combined_g %>% 
  to_undirected() %>% 
  mutate(group = group_leading_eigen()) 

tidy_combined_g2 %>% 
  mutate(modularity = graph_modularity(group=as.factor(group))) %>% 
  pull(modularity) %>% 
  head(1) ## [1] 0.2974819


## plot modularity figures

## create network layout (note: different from one used above)
layout_fr <- layout_with_gem(tidy_combined_g2)


## add edge columns indicating whether edges are moving into
## or out of a module (to be used to colour edges in plots)

## easy to do for zone-based modules
tidy_combined_g2 <- tidy_combined_g2 %>% 
  activate(edges) %>% 
  mutate(edge_dir1 = ifelse(sourceTaxon_zone == targetTaxon_zone, 
                            "in", "out"))

## a bit more complicated for optimized modules
t <- tidy_combined_g2 %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  select(group) %>%
  mutate(row = row_number())

tidy_combined_g2 <- tidy_combined_g2 %>% 
  activate(edges) %>% 
  left_join(., t, by = c("from" = "row")) %>% 
  rename(group_from = group) %>% 
  left_join(., t, by = c("to" = "row")) %>% 
  rename(group_to = group) %>% 
  mutate(edge_dir2 =  ifelse(group_from == group_to, 
                             "in", "out"))


## plot modularity by zone
tidy_combined_g2 %>% 
  activate(nodes) %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", 
                                  "Both", "Intertidal"))) %>% 
  ggraph(., layout = layout_fr) + 
  geom_mark_hull(mapping = aes(x = as.data.frame(layout_fr)[,1],
                               y = as.data.frame(layout_fr)[,2],
                               colour = as.factor(zone),
                               fill = as.factor(zone))) +
  guides(fill = guide_legend(title = "Zone"), 
         colour = guide_legend(title = "Zone")) + 
  scale_fill_manual(values = net_pal, name = "Zone") + 
  scale_colour_manual(values = net_pal, name = "Zone") + 
  geom_edge_link(mapping = aes(#linetype = interactionTypeName, 
    colour = edge_dir2), 
    end_cap = circle(.2, "cm"), 
    arrow = arrow(type = "closed", 
                  ends = "last", 
                  length = unit(1, "mm"))) + 
  scale_edge_colour_manual(values = c("grey50", "black"), 
                           name = "Direction") + 
  new_scale_fill() + 
  geom_node_point(mapping = aes(fill = as.factor(zone), 
                                size = weight), 
                  shape = 21) + 
  scale_fill_manual(values = net_pal, name = "Zone") + 
  # scale_edge_width(range = c(0.2, 1.5), guide = "none") +
  # scale_size_continuous("Degree", range =  c(2, 10)) +
  # scale_fill_discrete("Media Type") +
  # scale_colour_discrete("Community") +
  guides(size = "none") + 
  theme_graph() + 
  theme(legend.position = "none", 
        legend.box = "vertical", 
        plot.background = element_blank(), 
        panel.background = element_blank())

ggsave(filename = "Plots/modularity_imposed_habitats.png", plot = last_plot(), 
       width = 8, height = 8, units = "in", dpi = "retina")

## modularity by eigenvector grouping
# greens_pal <- RColorBrewer::brewer.pal("Greens", n=5)[2:5]
tidy_combined_g2 %>% 
  activate(nodes) %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", 
                                  "Both", "Intertidal"))) %>% 
  ggraph(., layout = layout_fr) + 
  geom_mark_hull(mapping = aes(x = as.data.frame(layout_fr)[,1],
                               y = as.data.frame(layout_fr)[,2],
                               colour = as.factor(group),
                               fill = as.factor(group))) +
  guides(fill = guide_legend(title = "Group"), 
         colour = guide_legend(title = "Group")) + 
  geom_edge_link(mapping = aes(#linetype = interactionTypeName, 
                               colour = edge_dir2), 
                 end_cap = circle(.2, "cm"), 
                 arrow = arrow(type = "closed", 
                               ends = "last", 
                               length = unit(1, "mm"))) + 
  scale_edge_colour_manual(values = c("grey50", "black"), 
                      name = "Direction") + 
  new_scale_fill() + 
  geom_node_point(mapping = aes(fill = as.factor(zone), 
                                size = weight), 
                  shape = 21) + 
  scale_fill_manual(values = net_pal, name = "Zone") + 
  # scale_edge_width(range = c(0.2, 1.5), guide = "none") +
  # scale_size_continuous("Degree", range =  c(2, 10)) +
  # scale_fill_discrete("Media Type") +
  # scale_colour_discrete("Community") +
  guides(size = "none") + 
  theme_graph() + 
  theme(legend.position = "none", 
        legend.box = "vertical", 
        plot.background = element_blank(), 
        panel.background = element_blank())

ggsave(filename = "Plots/modularity_optimized.png", plot = last_plot(), 
       width = 8, height = 8, units = "in", dpi = "retina")

tidy_combined_g2 %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(zone = factor(zone, 
                       levels = c("Subtidal", 
                                  "Both", "Intertidal"))) %>% 
  ggplot(data = ., 
         mapping = aes(x = group)) + 
  annotate("rect", xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf, 
           fill = "#F8766D", alpha = 0.4) +
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, 
           fill = "#7CAE00", alpha = 0.4) +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf, 
           fill = "#00BFC4", alpha = 0.4) + 
  annotate("rect", xmin = 3.5, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "#C77CFF", alpha = 0.4) + 
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) + 
  new_scale_fill() + 
  geom_bar(mapping = aes(fill = zone), colour = "black") + 
  scale_fill_manual(values = net_pal, name = "Zone") + 
  labs(x = "Group", y = "Count") + 
  theme_classic() + 
  theme(legend.position = c(0.85, 0.8), 
        plot.background = element_blank(), 
        panel.background = element_blank(), 
        legend.background = element_blank())


ggsave(filename = "Plots/habitat_by_optimized_module.png", plot = last_plot(), 
       width = 5, height = 5, units = "in", dpi = "retina")

