## networks for BEM presentation

## load required packages
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
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

## interactions database (combined literature and GLOBI data)
interactions_comb2 <- read_csv(
  "data/combined_litsearch_globi_interactions2.csv")

## combined all epibiont/epiphyte interactions into one group
## remove one parasitic interaction and one 'visitor' (?)
interactions_comb2 <- interactions_comb2 %>% 
  mutate(interactionTypeName = ifelse(
    interactionTypeName == "epiphyte of", 
    "epibiont of", interactionTypeName)) %>% 
  filter(!interactionTypeName %in% c("parasite of", "visits"))

## island species list
sp_zone2 <- readRDS("data/species_list_by_zone2.RDS")


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
  geom_node_point(aes(fill = zone, size = degree), shape = 21) +
  geom_edge_arc(aes(linetype = interactionTypeName), 
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
  # geom_edge_arc(aes(linetype = interactionTypeName), 
  #               alpha = 0.5, width = 0.5) + 
  geom_edge_hive(aes(linetype = interactionTypeName), 
                 colour = "grey30", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = degree), shape = 21) +
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
       width = 6, heigh = 4.5, units = "in", dpi = "retina")


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
## 
## How many intertidal/both interactions should there be?
## Both+Both + Both+Intertidal + Intertidal+Both + Intertidal+Intertidal
## 36 + 31 + 39 + 53 = 159 <--- nrow(interactions_int) below
##
## How many interactions span the subtidal and intertidal?
## Intertidal+Subtidal + Subtidal+Intertidal
## 53 + 81 = 134

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
  geom_node_point(aes(fill = zone, size = degree), shape = 21) +
  geom_edge_arc(aes(linetype = interactionTypeName), 
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
  # geom_edge_arc(aes(linetype = interactionTypeName), 
  #               alpha = 0.5, width = 0.5) + 
  geom_edge_hive(aes(linetype = interactionTypeName), 
                 colour = "grey30", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = degree), shape = 21) +
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
       width = 6, heigh = 4.5, units = "in", dpi = "retina")


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
  geom_node_point(aes(fill = zone, size = degree), shape = 21) +
  geom_edge_arc(aes(linetype = interactionTypeName), 
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
  # geom_edge_arc(aes(linetype = interactionTypeName), 
  #               alpha = 0.5, width = 0.5) + 
  geom_edge_hive(aes(linetype = interactionTypeName), 
                 colour = "grey30", alpha = 0.5, width = 0.5) + 
  geom_node_point(aes(fill = zone, size = degree), shape = 21) +
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
       width = 6, heigh = 4.5, units = "in", dpi = "retina")

# library(patchwork)
# (p.sub + p.int) / p.comb
