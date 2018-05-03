# site_clustering.R
# This script creates a new site list
# by clustering the original site list by statistical properties
# April 18th, 2018
# AJ smit


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(zoo)
library(lubridate)
library(ggpubr)
library(ggrepel)
library(FNN)

# Load data ---------------------------------------------------------------

load("site_list_v4.2.RData")


# Perform clustering ------------------------------------------------------

# Neat
set.seed(10)
kmeans(site_list[,c(15, 18, 19)], 3)$cluster


# Visualise different cluster options -------------------------------------

clust_columns <- site_list[,c(15, 18:19)]

clust_i <- function(i) {
  set.seed(10)
  ggplot(data = site_list, 
         aes(x = lon, y = lat, 
             colour = as.factor(kmeans(clust_columns, i)$cluster))) +
    borders() +
    geom_point() +
    labs(colour = "cluster") +
    coord_equal(xlim = c(15, 35), ylim = c(-37, -27)) +
    ggtitle(paste0("clust = ", i))
}

clust_6 <- clust_i(6)
clust_5 <- clust_i(5)
clust_4 <- clust_i(4)
clust_3 <- clust_i(3)

clusters <- ggarrange(clust_6, clust_5, clust_4, clust_3, common.legend = T)
clusters

# ggsave(clusters, filename = "cluster.png")

# Cluster sites by k-means results ----------------------------------------

load("SACTN_data/SACTN_daily_v4.2.RData")

# Create cluster index
set.seed(10)
site_list$cluster <- as.factor(kmeans(site_list[,c(15, 18:19)], 4)$cluster)

SACTN_daily_clusters <- left_join(SACTN_daily_v4.2, site_list[,c(4, 13, 21)]) %>% 
  filter(length >= 3650 * 3) 
# Here adjusted to 30 years, which works with the KZNSB data;
# use a shorter time for sites along other coastlines

SACTN_clust_1 <- SACTN_daily_clusters %>% 
  filter(cluster == 1) %>% 
  select(-length) %>% 
  droplevels()

# How many sites in this cluster?
length(unique(SACTN_clust_1$index)) 
# 42 of them if time series are 10 years or less; 
# or 33 if lengths of 30 years are selected...
# This might have to be adjusted depending on the coastline 
# we are working on

# This code below takes only a few minutes on my computer (AJS)...
# I now also wrapped it in an argument-less function...
# match_sites <- function() {
  SACTN_clust_1_match <- data.frame()
  for (i in 1:length(levels(SACTN_clust_1$index))) {
    # for (i in 1:10) {
    SACTN_df_1 <- filter(SACTN_clust_1, index == levels(index)[i])
    for (j in 1:length(levels(SACTN_clust_1$index))) {
      # for(j in 1:10) {
      if (i == j) {
        next
      }
      if (j < i) {
        next
      }
      SACTN_df_2 <- filter(SACTN_clust_1, index == levels(index)[j])
      SACTN_df_3 <-
        left_join(SACTN_df_1, SACTN_df_2, by = "date") %>%
        na.trim() %>%
        select(date,
               index.x,
               temp.x,
               index.y,
               temp.y,
               -cluster.x,
               -cluster.y) %>%
        rename(
          index_1 = index.x,
          temp_1 = temp.x,
          index_2 = index.y,
          temp_2 = temp.y
        ) %>%
        mutate(index_pair = paste0(index_1, " - ", index_2))
      SACTN_clust_1_match <- rbind(SACTN_clust_1_match, SACTN_df_3)
    }
  }
# }

# Run the function, and also time it for interest sake...
# system.time(match_sites())
# On my computer (AJS)... time in seconds
# > system.time(match_sites())
# user  system elapsed 
# 394.857 145.019 541.344 
# this is about 9 minutes

# Create paired legitimate difference values  -----------------------------

SACTN_clust_1_legit <- SACTN_clust_1_match %>% 
  mutate(temp_legit = temp_1-temp_2,
         month = month(date, abbr = T, label = T),
         year = year(date)) %>% 
  select(-temp_1, -temp_2) %>% 
  group_by(index_pair, month, year) %>% 
  summarise(temp_mean_month_year = mean(temp_legit, na.rm = T),
            temp_sd_month_year = sd(temp_legit, na.rm = T))

  # Sites present in cluster_1
(unique(SACTN_clust_1$index))
  
# Visualise results -------------------------------------------------------

# Paired values

ggplot(data = SACTN_clust_1_match, aes(x = index_pair)) +
  geom_boxplot(aes(y = temp_1), fill = "khaki4", alpha = 0.3) +
  geom_boxplot(aes(y = temp_2), fill = "chartreuse3", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Legit mean values
# I plot only the sites that are paired with "Glenmore/KZNSB -"
# We need the stringr package for that
# Also, for the KZNSB data we have time series of >30 years, and these are the ones
# plotted here...
library(stringr)
SACTN_clust_1_legit %>% 
  # filter(str_detect(index_pair, "Glenmore/KZNSB -")) %>% 
  ggplot(aes(x = year, y = temp_mean_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7, show.legend = F) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair), show.legend = T)  +
  facet_wrap(~month) + 
  theme_pubclean()
# Referring to the plot above: since the KZNSB data series are very long, we can
# have time series of 30 years in length;
# I'd also calculate a column with the distances between site pairs, and then
# colour the lines as a function of the site-site distances in this column

# Legit SD values
ggplot(data = SACTN_clust_1_legit, 
       aes(x = year, y = temp_sd_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair))  +
  facet_wrap(~month) + 
  theme_pubclean()


# Load wave data ----------------------------------------------------------

source("Load wave function.R")

# Subset the two different depths
sites_complete_7 <- sites_complete %>% 
  filter(depth == "wave_7")

sites_complete_15 <- sites_complete %>% 
  filter(depth == "wave_15")

# Cluster wave data -------------------------------------------------------

# First subset the site list to match the 3 decade sites
site_list_30_years <- site_list %>% 
  filter(index %in% SACTN_daily_clusters$index)

# Use fastest nearest neighbour (FNN) to find nearest wave data for each SACTN site
waves_idx_7 <- as.data.frame(knnx.index( as.matrix(sites_complete_7[,2:3]), as.matrix(site_list_30_years[,5:6]), k = 1)) 
waves_idx_15 <- as.data.frame(knnx.index( as.matrix(sites_complete_15[,2:3]), as.matrix(site_list_30_years[,5:6]), k = 1)) 

# Subset out the 7 and 15 metre wave sites
sites_complete_subset_7 <- sites_complete_7[as.matrix(waves_idx_7),]
sites_complete_subset_15 <- sites_complete_15[as.matrix(waves_idx_15),]

# Stick them together and clean it up
sites_complete_subset <- cbind(sites_complete_subset_7, sites_complete_subset_15[,4:5])
colnames(sites_complete_subset)[c(5, 7)] <- c("wave_7", "wave_15")
sites_complete_subset <- sites_complete_subset[, c(1:3, 5, 7)]

# Then add the cluster column to the wave sites info
site_list_match <- cbind(site_list_30_years[,c(4:6,21)], sites_complete_subset)
colnames(site_list_match) <- c("index", "lon1", "lat1", "cluster", "site_real", "lon2", "lat2", "wave_7", "wave_15")

# Now calculate the distances between these points
source("earthdist.R")

# First convert the degrees to radians
site_list_match_dist <- site_list_match %>% 
  mutate(lon1 = deg2rad(lon1),
         lat1 = deg2rad(lat1),
         lon2 = deg2rad(lon2),
         lat2 = deg2rad(lat2),
         dist = NA)

# Quick for loop to get distances
for(i in 1:nrow(site_list_match_dist)){
  dist <- gcd.hf(site_list_match_dist$lon1[i], site_list_match_dist$lat1[i], 
                 site_list_match_dist$lon2[i], site_list_match_dist$lat2[i])
  site_list_match_dist$dist[i] <- dist
}

# Now stick this info onto the 30 year site list
site_list_30_years <- left_join(site_list_30_years, site_list_match_dist[, c(1, 8:10)])
# With that info now all in one place it will be easier to get everything to "talk"

# Up next we want to combine the temperature and wave data
# First for the 7 metre depth waves
SACTN_daily_clusters_7 <- left_join(SACTN_daily_clusters, site_list_30_years[, c(4, 22)]) %>% 
  left_join(wave_daily[, c(2:3, 6:16)], by = c("date", "wave_7" = "site_wave"))
# Then the 15 metre depth waves
SACTN_daily_clusters_15 <- left_join(SACTN_daily_clusters, site_list_30_years[, c(4, 23)]) %>% 
  left_join(wave_daily, by = c("date", "wave_15" = "site_wave"))


# Summary stats -----------------------------------------------------------

SACTN_daily_clusters_7 %>%
  group_by(index) %>%
  summarise(temp_dir_cor = cor(temp, dir_mean))

# Plotting temps and waves ------------------------------------------------

# With the values now synced up it is possible to plot them together
ggplot(data = SACTN_daily_clusters_7, aes(x = temp, y = dir_mean)) +
  geom_line() +
  facet_wrap(~index)

