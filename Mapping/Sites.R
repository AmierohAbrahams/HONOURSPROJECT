
# Loading packages --------------------------------------------------------

library(tidyverse)
library(ggrepel)

# Creating graphs to better view and understand the site location
# Clusters in clus 1
# cluster 1
# Cape agulhas
# Ystervarkpunt
# mossel bay
# Gansbaai


# Loading the data --------------------------------------------------------
load("~/Documents/HONOURSPROJECT-master/Mapping/sa_provinces.RData")
site_coordinates1 <- read_csv("Mapping/site_coordinates1.csv")
load("~/Documents/HONOURSPROJECT-master/Mapping/south_africa_coast.RData")


# mapping -----------------------------------------------------------------


ggplot(data = site_coordinates1, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = site_coordinates1)  +
  geom_label_repel(data =site_coordinates1, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# cluster 2
# Gordons Bay
# Tsitsikamma
# Kalk Bay
# Muizenberg
site_coordinates2 <- read_csv("Mapping/site_coordinates2.csv")
ggplot(data = site_coordinates2, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = site_coordinates2)  +
  geom_label_repel(data =site_coordinates2, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
# cluster 3
# Hondeklipbaai
# Oudekraal
# Paternoster
# Port Nolloth
site_coordinates3 <- read_csv("Mapping/site_coordinates3.csv")
ggplot(data = site_coordinates3, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = site_coordinates3)  +
  geom_label_repel(data =site_coordinates3, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# cluster 4
# Aliwal Shoal
# Sodwana
# Glenmore
# Port Edward
site_coordinates4 <- read_csv("Mapping/site_coordinates4.csv")
ggplot(data = site_coordinates4, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = site_coordinates4)  +
  geom_label_repel(data =site_coordinates4, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())




