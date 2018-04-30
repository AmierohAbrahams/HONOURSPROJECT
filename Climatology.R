# Loading the packages ----------------------------------------------------
library(tidyverse)
library(ggpubr)
library(dplyr)
library(lubridate)
library(gridExtra)

View(SACTN_daily_v4.2)
temp<- load("~/Honours/Honours_Project/SACTN_data/SACTN_daily_v4.2.RData")
temp <- SACTN_daily_v4.2
temp$month <- format(temp$date,"%b")
temp$month <- factor(temp$month, levels = c("Jan","Feb","Mar","Apr",
                                                      "May","Jun","Jul","Aug",
                                                      "Sep","Oct","Nov","Dec"))

#Monthly climatology
temp_monthly <- temp %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(index, date) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE),
            range_temp = range(temp, na.rm = TRUE)[2]-range(temp, na.rm = TRUE)[1],
            sd_temp = sd(temp, na.rm = TRUE)) %>%
  filter(date %in% c("Jan","Feb","Mar","Apr",
                     "May","Jun","Jul","Aug",
                     "Sep","Oct","Nov","Dec")) %>% 
  ungroup()

# Annual climatology 
temp_annually <- temp %>% 
  mutate(date = lubridate::month(date, label = TRUE)) %>% 
  group_by(index, date) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            min_temp = min(temp, na.rm = T),
            max_temp = max(temp, na.rm = T),
            range_temp = range(temp, na.rm = T)[2]-range(temp, na.rm = T)[1],
            sd_temp = sd(temp, na.rm = T)) %>% 
  ungroup() %>% 
  select(-date) %>% 
  group_by(index) %>% 
  summarise_all(funs(mean(., na.rm = T))) %>% 
  mutate(date = "Annually") %>% 
  select(index, date, everything())

Tclimatology <- rbind(temp_monthly, temp_annually) %>% 
  group_by(index) %>% 
  gather(key = statistic, value = value, 
         mean_temp:sd_temp) %>% 
  mutate(climate_stats = paste(date, statistic, sep = "_")) %>% 
  select(-date, -statistic) %>% 
  spread(key = climate_stats, value = value)

# Converting thermal metrics into Euklidian distances
library(vegan)
env3 <- temp_climatology %>% 
  dplyr::ungroup(index) %>% 
  dplyr::select(-index) %>% 
  vegdist(method = 'euclidian', na.rm = TRUE)

# Creating a matrix of the data
env3.matrix <- as.matrix(env3)
env3.diag <- diag(env3.mat[-1, -nrow(env3.matrix)])
env3.diag <- append(0, env3.diag, after = 1)
