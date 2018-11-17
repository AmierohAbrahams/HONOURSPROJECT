###Coefficient of determination #####

temp_wave_compare <- function(df){
  df1 <- df %>% 
    dplyr::mutate(year = year(date), 
                  week = week(date)) %>% 
    dplyr::select(-date) %>% 
    tidyr::gather(key = variable, value = value, 
                  -c(index:src, year, week, length:depth)) %>% 
    na.omit() %>% 
    ungroup() %>% 
    group_by(index, year, week, variable) %>% 
    na.omit() %>% 
    dplyr::summarise(value = mean(value, na.rm = T)) %>% 
    arrange(index, year, week) %>% 
    group_by(index, variable) %>% 
    mutate(index2 = 1:n())
  ggplot(data = df1, aes(x = index2)) +
    geom_line(aes(y = value, colour = variable, group = )) +
    facet_grid(variable~index, scales = "free_y")
}