## Seasonal match###


clust_matched <- function(df) {
  SACTN_clust_x_matched <- df %>%
    mutate(temp_matched = temp_1-temp_2,
           month = month(date, abbr = T, label = T),
           year = year(date)) %>%
    select(-temp_1, -temp_2) %>%
    group_by(index_pair, index_dist, month, year) %>%
    summarise(temp_mean_month_year = mean(temp_matched, na.rm = T),
              temp_sd_month_year = sd(temp_matched, na.rm = T)) %>% 
    mutate(season = ifelse(month %in% c("Jan", "Feb", "Mar"), "Summer",        
                           ifelse(month %in% c("Apr", "May", "Jun"), "Autumn",
                                  ifelse(month %in% c("Jul", "Aug", "Sep"), "Winter",
                                         ifelse(month %in% c("Oct", "Nov", "Dec"), "Spring","Error")))))
  return(SACTN_clust_x_matched)
}