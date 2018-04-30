# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(zoo)
library(lubridate)

# Load data ---------------------------------------------------------------

load("site_list_v4.2.RData")

# Checking how many sites in dataset --------------------------------------
length(unique(SACTN_daily_v4.2$index)) 


# The other clusters ------------------------------------------------------

# Cluster 2 ---------------------------------------------------------------
SACTN_clust_2 <- SACTN_daily_clusters %>% 
  filter(cluster == 2) %>% 
  select(-length) %>% 
  droplevels()

length(unique(SACTN_clust_2$index))

# Sites present in cluster_2
(unique(SACTN_clust_2$index))

# Mapping the sites pressent in cluster_2

sites_coordinates2 <- read_csv("Mapping/site_coordinates2.csv")

ggplot(data = sites_coordinates2, aes(x = lon, y = lat)) +
  geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
  coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_point(data = sites_coordinates2)  +
  geom_label_repel(data =sites_coordinates2, aes(x = lon, y = lat, label = Site), 
                   size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


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
# }

SACTN_clust_2_legit <- SACTN_clust_2_match %>% 
  mutate(temp_legit = temp_1-temp_2,
         month = month(date, abbr = T, label = T),
         year = year(date)) %>% 
  select(-temp_1, -temp_2) %>% 
  group_by(index_pair, month, year) %>% 
  summarise(temp_mean_month_year = mean(temp_legit, na.rm = T),
            temp_sd_month_year = sd(temp_legit, na.rm = T))

ggplot(data = SACTN_clust_2_match, aes(x = index_pair)) +
  geom_boxplot(aes(y = temp_1), fill = "khaki4", alpha = 0.3) +
  geom_boxplot(aes(y = temp_2), fill = "chartreuse3", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Cluster 3 ---------------------------------------------------------------

SACTN_clust_3 <- SACTN_daily_clusters %>% 
  filter(cluster == 3) %>% 
  select(-length) %>% 
  droplevels()


length(unique(SACTN_clust_3$index))

# sites present in cluster_3 
(unique(SACTN_clust_3$index))


site_coordinates3 <- read_delim("Mapping/site_coordinates3.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)

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
# }

SACTN_clust_3_legit <- SACTN_clust_3_match %>% 
  mutate(temp_legit = temp_1-temp_2,
         month = month(date, abbr = T, label = T),
         year = year(date)) %>% 
  select(-temp_1, -temp_2) %>% 
  group_by(index_pair, month, year) %>% 
  summarise(temp_mean_month_year = mean(temp_legit, na.rm = T),
            temp_sd_month_year = sd(temp_legit, na.rm = T))

ggplot(data = SACTN_clust_3_match, aes(x = index_pair)) +
  geom_boxplot(aes(y = temp_1), fill = "khaki4", alpha = 0.3) +
  geom_boxplot(aes(y = temp_2), fill = "chartreuse3", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Cluster 4 ---------------------------------------------------------------

SACTN_clust_4 <- SACTN_daily_clusters %>% 
  filter(cluster == 4) %>% 
  select(-length) %>% 
  droplevels()

length(unique(SACTN_clust_4$index))

# Sites present in cluster_2
(unique(SACTN_clust_4$index))

# Mapping the sites pressent in cluster_2

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

SACTN_clust_4_match <- data.frame()
for (i in 1:length(levels(SACTN_clust_4$index))) {
  # for (i in 1:10) {
  SACTN_df_1 <- filter(SACTN_clust_4, index == levels(index)[i])
  for (j in 1:length(levels(SACTN_clust_4$index))) {
    # for(j in 1:10) {
    if (i == j) {
      next
    }
    if (j < i) {
      next
    }
    SACTN_df_2 <- filter(SACTN_clust_4, index == levels(index)[j])
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
    SACTN_clust_4_match <- rbind(SACTN_clust_4_match, SACTN_df_3)
  }
}
# }

SACTN_clust_4_legit <- SACTN_clust_4_match %>% 
  mutate(temp_legit = temp_1-temp_2,
         month = month(date, abbr = T, label = T),
         year = year(date)) %>% 
  select(-temp_1, -temp_2) %>% 
  group_by(index_pair, month, year) %>% 
  summarise(temp_mean_month_year = mean(temp_legit, na.rm = T),
            temp_sd_month_year = sd(temp_legit, na.rm = T))

ggplot(data = SACTN_clust_4_match, aes(x = index_pair)) +
  geom_boxplot(aes(y = temp_1), fill = "khaki4", alpha = 0.3) +
  geom_boxplot(aes(y = temp_2), fill = "chartreuse3", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





