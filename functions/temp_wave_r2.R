##### Creating a func####

# A function that runs a linear model on each metric of the combined data and get the R^2 value
temp_wave_R2 <- function(df) {
  results <- df %>% 
    dplyr::select(-index, -src, -date, -length, -c(num:depth), -temp) %>%
    gather(key = variable, 
           value = value,
           -cluster, -site, -temp_flat) %>%
    dplyr::group_by(cluster, site, variable) %>%
    na.omit() %>% 
    nest() %>%
    dplyr::mutate(model = purrr::map(data, ~lm(temp_flat ~ value, data = .))) %>%
    unnest(model %>% purrr::map(glance)) %>%
    dplyr::select(cluster:variable, adj.r.squared, p.value) %>% 
    dplyr::arrange(cluster, site) %>% 
    dplyr::mutate(adj.r.squared = round(adj.r.squared, 2),
                  p.value = round(p.value, 4))
  return(results)
}