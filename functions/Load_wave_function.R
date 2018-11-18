# Wave data
# try1
# Wave data
# SWAN MODEL
# working with monthly and annual but not with daily
# Need to calculate daily wave data

library(tidyverse)
library(lubridate)
library(ggpubr)

# Create function ---------------------------------------------------------
# Directory with wave files

test_func <- function(wave_files){
#Determining the site names
  site_type <- sapply(strsplit(as.character(wave_files), "/"), "[[", 3)
  site_num <- sapply(strsplit(as.character(wave_files), "/"), "[[", 4)
  site_num <- sapply(strsplit(site_num, ".txt"), "[[", 1)
  site_name <- paste(site_type, site_num, sep = "")
  
# Time zone of africa/Jhb
# creating a new column but includes time and date
# get time only???
  try1 <- read.csv(wave_files, col.names = c("date", "hs", "tp", "dir", "dirw", "spw"), sep = "", header = F) %>%
    filter(tp != -999) %>% 
    mutate(date = as.POSIXct(as.character(date), "%Y%m%d%H%M", tz = "Africa/Johannesburg")) %>%
    mutate(site = site_name) %>%
    select(site, everything())
    
  # Finish
  return(try1)
}

# feed as vector

trial_load <- function(directory){
  files_list <- dir(directory, full.names = TRUE)
  final <- plyr::ldply(files_list, test_func, .progress = "text")
  return(final)
}

# Loading the site wave data ----------------------------------------------

sites_complete <- read_csv("../data/sites_complete.csv") %>% 
  select(-site_list) %>% 
  rename(site_real = site) %>% 
  gather(key = "depth", value = "site", -site_real, -lon, -lat)
# load("~/Honours/Honours_Project/Mapping/sa_provinces.RData")
# load("~/Honours/Honours_Project/Mapping/south_africa_coast.RData")

# Testing with the wave data ----------------------------------------------

FB <- trial_load("../data/wave_data/FB")
TB <- trial_load("../data/wave_data/TB")
HE <- trial_load("../data/wave_data/HE")
SH <- trial_load("../data/wave_data/SH")

# Combining all the wave data collected a the different sites 
wave_data <- rbind(FB, TB, HE, SH)
wave_data <- left_join(wave_data, sites_complete)%>% 
  rename(site_wave = site,
         site = site_real)
rm(FB, TB, HE, SH)

# Calculating the daily monthly and annual wave data

########### NW######
#####Calculating the daily wave data
wave_daily <- wave_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(site, site_wave, depth, lon, lat, date) %>% 
  summarise_all(funs(mean = mean, sd = sd), na.rm = T) %>% 
  ungroup()

##############
# same/ as file name climatology but working with the wave data

wave_monthly <- wave_data %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>%
  group_by(site, site_wave, depth, lon, lat, date) %>% 
  summarise_all(funs(mean = mean, sd = sd), na.rm = TRUE) %>%
  filter(date %in% c("Jan","Feb","Mar","Apr",
                     "May","Jun","Jul","Aug",
                     "Sep","Oct","Nov","Dec")) %>% 
  ungroup()

wave_annually <- wave_data %>% 
  mutate(date = lubridate::year(date)) %>% 
  group_by(site, site_wave, depth, lon, lat, date) %>% 
  summarise_all(funs(mean = mean, sd = sd), na.rm = TRUE) %>%
  ungroup() %>%
  filter(date != 2000) # The year 2000 only has 1 row of data 

# Only focusingon dir and dirw --------------------------------------------

# directions <- subset(wave_data, select = c(1, 2, 5, 6)) 

# Seasons calculating the stats(min/max/mean etc)
# working with all the sites
# dirs_monthly <- directions %>% 
#   mutate(date = lubridate::month(date, label = TRUE)) %>% 
#   group_by(site, date) %>% 
#   summarise(dir_min = min(dir, na.rm = TRUE),
#             dir_max = max(dir, na.rm = TRUE),
#             dir_range = range(dir, na.rm = TRUE)[2]-range(dir, na.rm = TRUE)[1],
#             dir_median = median(dir, na.rm = TRUE), 
#             dirw_min = min(dirw, na.rm = TRUE),
#             dirw_max = max(dirw, na.rm = TRUE),
#             dirw_range = range(dirw, na.rm = TRUE)[2]-range(dirw, na.rm = TRUE)[1],
#             dirw_median = median(dirw, na.rm = TRUE)) %>%
#   filter(date %in% c("Jan","Feb","Mar","Apr",
#                      "May","Jun","Jul","Aug",
#                      "Sep","Oct","Nov","Dec")) %>% 
#   ungroup()

# dirs_annually <- directions %>% 
#   mutate(date = lubridate::month(date, label = TRUE)) %>% 
#   group_by(site, date) %>% 
#   summarise(dir_min = min(dir, na.rm = TRUE),
#             dir_max = max(dir, na.rm = TRUE),
#             dir_range = range(dir, na.rm = TRUE)[2]-range(dir, na.rm = TRUE)[1],
#             dir_median = median(dir, na.rm = TRUE), 
#             dirw_min = min(dirw, na.rm = TRUE),
#             dirw_max = max(dirw, na.rm = TRUE),
#             dirw_range = range(dirw, na.rm = TRUE)[2]-range(dirw, na.rm = TRUE)[1],
#             dirw_median = median(dirw, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   select(-date) %>% 
#   group_by(site) %>% 
#   summarise_all(funs(mean(., na.rm = TRUE))) %>% 
#   mutate(date = "Annually") %>% 
#   select(site, date, everything())

# dirs_monthly <- subset(dirs_monthly, select = c(1, 2, 6, 10))
# dirs_annually <- subset(dirs_annually, select = c(1, 2, 6, 10))


# Loading package ---------------------------------------------------------

# library(ggrepel)

# plotting the wave data
# Wave data sites given by AJ

# ggplot(data = sites_complete, aes(x = lon, y = lat)) +
#   geom_polygon(data = south_africa_coast, aes(group = group), fill = "grey70") +
#   coord_cartesian(xlim = c(12, 36), ylim = c(-38, -26)) +
#   geom_path(data = sa_provinces, aes(group = group)) +
#   geom_point(data = sites_complete)  +
#   geom_label_repel(data =sites_complete, aes(x = lon, y = lat, label = site), 
#                    size = 3, box.padding = 2, nudge_x = -0.5, nudge_y = 0.2, segment.alpha = 0.4) +
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank())

# Combining all the dirs which include annual and monthly directions
# Combining all the metrics(Both 7 and 15m)

# combining_dirs <- rbind(dirs_monthly, dirs_annually) %>% 
#   group_by(site) %>% 
#   gather(key = statistic, value = value, 
#          3:4) %>% 
#   mutate(clim_stat = paste(date, statistic, sep = ":")) %>% 
#   select(-date, -statistic) %>% 
#   spread(key = clim_stat, value = value) %>% 
#   left_join(sites_complete, by = c("site" = "wave_15")) %>% 
#   ungroup() %>% 
#   select(-site, wave_7) %>%
#   rename(site = site.y) %>% 
#   select(site, everything()) %>% 
#   na.omit()
# Only looking at the wave data from a 15m metric
# should i look at 7m metric as well?

# wave_15m <- rbind(wave_monthly, wave_annually) %>% 
#   group_by(site) %>% 
#   gather(key = statistic, value = value, 
#          hs_mean:spw_sd) %>% 
#   mutate(clim_stat = paste(date, statistic, sep = ":")) %>% 
#   select(-date, -statistic) %>% 
#   spread(key = clim_stat, value = value) %>% 
#   left_join(sites_complete, by = c("site" = "wave_15")) %>% 
#   ungroup() %>% 
#   select(-site, wave_7) %>%
#   rename(site = site.y) %>% 
#   select(site, everything()) %>% 
#   na.omit()

# Combining both the wave at 15m and wave directions
# wave_final <- cbind(wave_15m, combining_dirs)

# temp <- load("~/Honours/Honours_Project/SACTN_data/SACTN_daily_v4.2.RData")

# Tclimatology is found in the script labelled climatology
# This works with monthly and yearly values not daily

# index_temp <- merge(Tclimatology, temp, by = "index")
# gathered_index_temp <- gather(data = index_temp, "stat", "value", 2:66)
