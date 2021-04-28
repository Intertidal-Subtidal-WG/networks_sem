
# This Script is for exploration in temporal and spatial trends 
# Contributor: Julien Beaulieu

#data loaded with : https://github.com/Intertidal-Subtidal-WG/networks_sem/blob/master/Script/Load-data/function_load_data_merge_intertidal_subtidal.R

#load packages
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)
library(data.table)
library(tibble)

df <- merg__combined_all_abundance_data
 
#make a column for organism with size and type if != NA

for (i in (1:nrow(df))) {
  if (is.na(df$SIZE[i]) == F){ df$organism2[i] <-  paste(df$ORGANISM[i], df$SIZE[i], sep = "_")
    } else { df$organism2[i] <- df$ORGANISM[i]
    } 
  if (is.na(df$ORGANISM_TYPE[i]) == F) { df$organism2[i] <- paste(df$organism2[i], df$ORGANISM_TYPE[i], sep = "_") 
    } 
  }
 

## make large format
df <- unite(df, "key", c(1:5,7), sep = "_", remove = F )
df <- as.data.table(df)
ab_large <- df[ ,c(1, 13, 30)]
ab_large <- unique(ab_large)
ab_large <- na.omit(ab_large)
ab_large <- spread(ab_large, organism2, VALUE) # something wrong in the data here?
ab_large[is.na(ab_large)] <- 0

# transform and standardize
ab_large <- ab_large %>% remove_rownames %>% column_to_rownames(var="key")
ab_tr <- decostand(ab_large, method = "standardize") # scale x to zero mean and unit variance (default MARGIN = 2)
ab_tr <- ab_tr[ , colSums(is.na(ab_tr)) == 0]  # the sum of some species = 0 -> to investigate

#Claculate ordination
PCOA <- capscale(ab_tr~1, distance = "euclidean") # 7, 2.7 et 2 % explained
summary(PCOA)

#extract score
score <- scores(PCOA, choices = c(1,2,3), display = "sites")
score <- as.data.frame(score)
#merge score with relevant variables to group
group <- df[,c("key", "YEAR", "SITE", "TRANSECT", "INTERTIDAL_TRANSECT", "LEVEL", "PROTOCOL")]
setDT(score, keep.rownames = "key")
score_merg <-  merge(group, score, by.x = 'key', by.y = 'key')
score_merg$LEVEL <- as.factor(score_merg$LEVEL)
#Graph###############################################

#calculate average coordinates for each site
dt <- score_merg
dt <- dt[,c(3,8:10)]
dt[, MDS1 := mean(MDS1), by = .(SITE)]
dt[, MDS2 := mean(MDS2), by = .(SITE)]
dt[, MDS3 := mean(MDS2), by = .(SITE)]
dt<-unique(dt)
site_coor <- dt
  
# faire graph with elipses for sites and colours for time for MDS1 and MDS2
graph1 <- ggplot()+ 
  annotate("text", x = Inf , y = -Inf, label = "", size = 4, fontface= 3,hjust=1.15, vjust= -1) +
  geom_point(data = score_merg, aes(y = MDS2, x = MDS1, colour = YEAR), alpha = 0.7, size = 2.85)+ #this is the points
  scale_color_continuous(type = "gradient")+
  stat_ellipse(data = score_merg, geom="polygon", aes(y = MDS2, x = MDS1, fill = SITE), #?lipses
               alpha = 0.15, 
               show.legend = FALSE, 
               level = 0.95) +
  geom_segment(data = site_coor, aes(x = 0, xend = MDS1, y = 0, yend = MDS2), alpha = 0.6, #fl?ches
               arrow = arrow(length = unit(0.02, "npc"), type = "open"),
               lwd = 1)+
  geom_text_repel(data = site_coor, aes(x = MDS1, y = MDS2, label = SITE), force = 1.1, point.padding = 0.1, hjust = T, size = 4.8)+
  geom_hline(yintercept = 0, lty = 2) + #lignes ? 0,0
  geom_vline(xintercept = 0, lty = 2) +
  xlab("PCoA 1 (7.0%)") + 
  ylab("PCoA 2 (2.7%)") +
  labs(title = "elipses for sites and colours for time for MDS1 and MDS2")+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        axis.text=element_text(size=15),
        axis.title=element_text(size=17),
        legend.text = element_text(size=16),
        legend.title = element_text(size=17),
  )
graph1


# faire graph with elipses for sites and colours for time for MDS1 and MDS3

graph2 <- ggplot()+ 
  annotate("text", x = Inf , y = -Inf, label = "", size = 4, fontface= 3,hjust=1.15, vjust= -1) +
  geom_point(data = score_merg, aes(y = MDS3, x = MDS1, colour = YEAR), alpha = 0.7, size = 2.85)+ #this is the points
  scale_color_continuous(type = "gradient")+
  stat_ellipse(data = score_merg, geom="polygon", aes(y = MDS3, x = MDS1, fill = SITE), #?lipses
               alpha = 1, 
               show.legend = FALSE, 
               level = 0.95) +
  geom_segment(data = site_coor, aes(x = 0, xend = MDS1, y = 0, yend = MDS3), alpha = 0.6, #fl?ches
               arrow = arrow(length = unit(0.02, "npc"), type = "open"),
               lwd = 1)+
  geom_text_repel(data = site_coor, aes(x = MDS1, y = MDS3, label = SITE), force = 1.1, point.padding = 0.1, hjust = T, size = 4.8)+
  geom_hline(yintercept = 0, lty = 2) + #lignes ? 0,0
  geom_vline(xintercept = 0, lty = 2) +
  xlab("PCoA 1 (7.0%)") + 
  ylab("PCoA 3 (2.0%)") +
  labs(title = "elipses for sites and colours for time for MDS1 and MDS3")+
theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        axis.text=element_text(size=15),
        axis.title=element_text(size=17),
        legend.text = element_text(size=16),
        legend.title = element_text(size=17),
        )
graph2

# faire graph with elipses for level, shapes for site and colours for time for MDS1 and MDS2

  #replace NA per sub 
score_merg$LEVEL[is.na(score_merg$LEVEL)] <- 20

graph3 <- ggplot()+ 
  annotate("text", x = Inf , y = -Inf, label = "", size = 4, fontface= 3,hjust=1.15, vjust= -1) +
  geom_point(data = score_merg, aes(y = MDS2, x = MDS1, colour = YEAR, shape = SITE), alpha = 0.7, size = 2.85)+ #this is the points
  scale_color_continuous(type = "gradient")+
  stat_ellipse(data = score_merg, geom="polygon", aes(y = MDS2, x = MDS1, fill = LEVEL), #?lipses
               alpha = 0.15, 
               show.legend = FALSE, 
               level = 0.95) +
  geom_segment(data = site_coor, aes(x = 0, xend = MDS1, y = 0, yend = MDS2), alpha = 0.6, #fl?ches
               arrow = arrow(length = unit(0.02, "npc"), type = "open"),
               lwd = 1)+
  geom_text_repel(data = site_coor, aes(x = MDS1, y = MDS2, label = SITE), force = 1.1, point.padding = 0.1, hjust = T, size = 4.8)+
  geom_text_repel(data = score_merg, aes(x = MDS1, y = MDS2, label = LEVEL), force = 1, point.padding = 0.1, hjust = T, size = 2)+
  geom_hline(yintercept = 0, lty = 2) + #lignes ? 0,0
  geom_vline(xintercept = 0, lty = 2) +
  xlab("PCoA 1 (7.0%)") + 
  ylab("PCoA 2 (2.7%)") +
  labs(title = "elipses for sites and colours for time for MDS1 and MDS2")+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        axis.text=element_text(size=15),
        axis.title=element_text(size=17),
        legend.text = element_text(size=16),
        legend.title = element_text(size=17),
  )
graph3



