### R2 values for the temp data###

temp_wave_R2_ <- function(df){
  results <- df %>% 
    select(-index, -src, -date, -length, -c(num:depth)) %>%
    gather(key = variable, 
           value = value,
           -cluster, -site, -temp) %>%
    group_by(cluster, site, variable) %>%
    na.omit() %>% 
    nest() %>%
    mutate(model = purrr::map(data, ~lm(temp ~ value, data = .))) %>%
    unnest(model %>% purrr::map(glance)) %>%
    select(cluster:variable, adj.r.squared, p.value) %>% 
    arrange(cluster, site) %>% 
    mutate(adj.r.squared = round(adj.r.squared, 2),
           p.value = round(p.value, 4)) %>% 
    dplyr::rename("R^2" = adj.r.squared) %>% 
    dplyr::rename(P = p.value) %>% 
    dplyr::rename(Cluster = cluster) %>% 
    dplyr::rename(Site = site) %>% 
    dplyr::rename(Variable = variable) 
  return(results)
}
