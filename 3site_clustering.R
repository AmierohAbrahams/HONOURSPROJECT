# The rest of the clusters
# Inccluding wave_data analysis

# Loading libraries -------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(zoo)
library(lubridate)
library(ggpubr)
library(ggrepel)
library(FNN)

# Load data ---------------------------------------------------------------
load("SACTN_data/SACTN_daily_v4.2.RData")
load("site_list_v4.2.RData")
load("Mapping/south_africa_coast.RData")
load("Mapping/sa_provinces.RData")

# Create cluster index
set.seed(10)
site_list$cluster <- as.factor(kmeans(site_list[,c(15, 18:19)], 4)$cluster)

SACTN_daily_clusters <- left_join(SACTN_daily_v4.2, site_list[,c(4, 13, 21)]) %>% 
  filter(length >= 3650 * 3) 


# Cluster 1 ---------------------------------------------------------------

# Mapping
cluster1_sites <- read_csv("Mapping/3_site_clustering/cluster1_sites.csv")
ggplot(data = cluster1_sites, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = cluster1_sites)  +
  geom_label_repel(data =cluster1_sites, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Cluster 2 ---------------------------------------------------------------

SACTN_clust_2 <- SACTN_daily_clusters %>% 
  filter(cluster == 2) %>% 
  select(-length) %>% 
  droplevels()

length(unique(SACTN_clust_2$index)) 

# Sites present
(unique(SACTN_clust_2$index))

# Mapping sites
cluster2_sites <- read_csv("Mapping/3_site_clustering/cluster2_sites.csv")
ggplot(data = cluster2_sites, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = cluster2_sites)  +
  geom_label_repel(data =cluster2_sites, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Matching the sites
SACTN_clust_2_match <- data.frame()
for (i in 1:length(levels(SACTN_clust_2$index))) {
  # for (i in 1:10) {
  SACTN_df_1 <- filter(SACTN_clust_2, index == levels(index)[i])
  for (j in 1:length(levels(SACTN_clust_2$index))) {
    # for(j in 1:10) {
    if (i == j) {
      next
    }
    if (j < i) {
      next
    }
    SACTN_df_2 <- filter(SACTN_clust_2, index == levels(index)[j])
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
    SACTN_clust_2_match <- rbind(SACTN_clust_2_match, SACTN_df_3)
  }
}

# Paired difference values
SACTN_clust_2_legit <- SACTN_clust_2_match %>% 
  mutate(temp_legit = temp_1-temp_2,
         month = month(date, abbr = T, label = T),
         year = year(date)) %>% 
  select(-temp_1, -temp_2) %>% 
  group_by(index_pair, month, year) %>% 
  summarise(temp_mean_month_year = mean(temp_legit, na.rm = T),
            temp_sd_month_year = sd(temp_legit, na.rm = T))

# Visualising the results
# Paired values

ggplot(data = SACTN_clust_2_match, aes(x = index_pair)) +
  geom_boxplot(aes(y = temp_1), fill = "khaki4", alpha = 0.3) +
  geom_boxplot(aes(y = temp_2), fill = "chartreuse3", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

SACTN_clust_2_legit %>% 
  ggplot(aes(x = year, y = temp_mean_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7, show.legend = F) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair), show.legend = T)  +
  facet_wrap(~month) + 
  theme_pubclean()

ggplot(data = SACTN_clust_2_legit, 
       aes(x = year, y = temp_sd_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair))  +
  facet_wrap(~month) + 
  theme_pubclean()

# Load wave data ----------------------------------------------------------
# Ignore this
# Me practising A.A
source("Load wave function.R")

# 2 different depths
sites_complete_7 <- sites_complete %>% 
  filter(depth == "wave_7")

sites_complete_15 <- sites_complete %>% 
  filter(depth == "wave_15")

# matching the decades
site_list_30_years <- site_list %>% 
  filter(index %in% SACTN_daily_clusters$index)

# Nearest wave data for  the sites
waves_idx_7 <- as.data.frame(knnx.index( as.matrix(sites_complete_7[,2:3]), as.matrix(site_list_30_years[,5:6]), k = 1)) 
waves_idx_15 <- as.data.frame(knnx.index( as.matrix(sites_complete_15[,2:3]), as.matrix(site_list_30_years[,5:6]), k = 1)) 

# Subset out the 7 and 15 metre wave sites
sites_complete_subset_7 <- sites_complete_7[as.matrix(waves_idx_7),]
sites_complete_subset_15 <- sites_complete_15[as.matrix(waves_idx_15),]

# cleanup
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
# 7m
SACTN_daily_clusters_7 <- left_join(SACTN_daily_clusters, site_list_30_years[, c(4, 22)]) %>% 
  left_join(wave_daily[, c(2:3, 6:16)], by = c("date", "wave_7" = "site_wave"))
# 15m
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


# Cluster_3 ---------------------------------------------------------------

SACTN_clust_3 <- SACTN_daily_clusters %>% 
  filter(cluster == 3) %>% 
  select(-length) %>% 
  droplevels()

length(unique(SACTN_clust_3$index)) 

# Sites present
(unique(SACTN_clust_3$index))

# Mapping sites
cluster3_sites <- read_csv("Mapping/3_site_clustering/cluster3_sites.csv")
ggplot(data = cluster3_sites, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = cluster3_sites)  +
  geom_label_repel(data =cluster3_sites, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Matching the sites
SACTN_clust_3_match <- data.frame()
for (i in 1:length(levels(SACTN_clust_3$index))) {
  # for (i in 1:10) {
  SACTN_df_1 <- filter(SACTN_clust_3, index == levels(index)[i])
  for (j in 1:length(levels(SACTN_clust_3$index))) {
    # for(j in 1:10) {
    if (i == j) {
      next
    }
    if (j < i) {
      next
    }
    SACTN_df_2 <- filter(SACTN_clust_3, index == levels(index)[j])
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
    SACTN_clust_3_match <- rbind(SACTN_clust_3_match, SACTN_df_3)
  }
}

# Paired difference values
SACTN_clust_3_legit <- SACTN_clust_3_match %>% 
  mutate(temp_legit = temp_1-temp_2,
         month = month(date, abbr = T, label = T),
         year = year(date)) %>% 
  select(-temp_1, -temp_2) %>% 
  group_by(index_pair, month, year) %>% 
  summarise(temp_mean_month_year = mean(temp_legit, na.rm = T),
            temp_sd_month_year = sd(temp_legit, na.rm = T))

# Visualising the results
# Paired values

ggplot(data = SACTN_clust_3_match, aes(x = index_pair)) +
  geom_boxplot(aes(y = temp_1), fill = "khaki4", alpha = 0.3) +
  geom_boxplot(aes(y = temp_2), fill = "chartreuse3", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

SACTN_clust_3_legit %>% 
  ggplot(aes(x = year, y = temp_mean_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7, show.legend = F) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair), show.legend = T)  +
  facet_wrap(~month) + 
  theme_pubclean()

ggplot(data = SACTN_clust_3_legit, 
       aes(x = year, y = temp_sd_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair))  +
  facet_wrap(~month) + 
  theme_pubclean()

# Cluster 4 ---------------------------------------------------------------

SACTN_clust_4 <- SACTN_daily_clusters %>% 
  filter(cluster == 4) %>% 
  select(-length) %>% 
  droplevels()

length(unique(SACTN_clust_4$index)) 

# Sites present
(unique(SACTN_clust_4$index))

# Mapping
cluster4_sites <- read_delim("Mapping/3_site_clustering/cluster4_sites.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE)

ggplot(data = cluster4_sites, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = cluster4_sites)  +
  geom_label_repel(data =cluster4_sites, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Matching the sites
# SACTN_clust_4_match <- data.frame()
# for (i in 1:length(levels(SACTN_clust_4$index))) {
#   # for (i in 1:10) {
#   SACTN_df_1 <- filter(SACTN_clust_4, index == levels(index)[i])
#   for (j in 1:length(levels(SACTN_clust_4$index))) {
#     # for(j in 1:10) {
#     if (i == j) {
#       next
#     }
#     if (j < i) {
#       next
#     }
#     SACTN_df_2 <- filter(SACTN_clust_4, index == levels(index)[j])
#     SACTN_df_3 <-
#       left_join(SACTN_df_1, SACTN_df_2, by = "date") %>%
#       na.trim() %>%
#       select(date,
#              index.x,
#              temp.x,
#              index.y,
#              temp.y,
#              -cluster.x,
#              -cluster.y) %>%
#       rename(
#         index_1 = index.x,
#         temp_1 = temp.x,
#         index_2 = index.y,
#         temp_2 = temp.y
#       ) %>%
#       mutate(index_pair = paste0(index_1, " - ", index_2))
#     SACTN_clust_4_match <- rbind(SACTN_clust_4_match, SACTN_df_3)
#   }
# }

# RWS: One way of dealing with long calculation times like this is to save the results
# so that you don't need to keep running the code each time you start up again
# save(SACTN_clust_4_match, file = "data/SACTN_clust_4_match.RData")
# load("data/SACTN_clust_4_match.RData")
# That being said, there are too many sites being compared to one another here.
# We need to make a plan about what to do with that.

# Paired difference values
SACTN_clust_4_legit <- SACTN_clust_4_match %>% 
  mutate(temp_legit = temp_1-temp_2,
         month = month(date, abbr = T, label = T),
         year = year(date)) %>% 
  select(-temp_1, -temp_2) %>% 
  group_by(index_pair, month, year) %>% 
  summarise(temp_mean_month_year = mean(temp_legit, na.rm = T),
            temp_sd_month_year = sd(temp_legit, na.rm = T))

# Visualising the results
# Paired values

ggplot(data = SACTN_clust_4_match, aes(x = index_pair)) +
  geom_boxplot(aes(y = temp_1), fill = "khaki4", alpha = 0.3) +
  geom_boxplot(aes(y = temp_2), fill = "chartreuse3", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

SACTN_clust_4_legit %>% 
  ggplot(aes(x = year, y = temp_mean_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7, show.legend = F) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair), show.legend = T)  +
  facet_wrap(~month) + 
  theme_pubclean()

ggplot(data = SACTN_clust_4_legit, 
       aes(x = year, y = temp_sd_month_year)) +
  geom_line(aes(group = index_pair, colour = index_pair), alpha = 0.7) +
  geom_smooth(method = "gam", se = F, aes(colour = index_pair))  +
  facet_wrap(~month) + 
  theme_pubclean()
