### Matching function###


clust_match <- function(df, clust) {
  SACTN_clust_x_match <- data.frame()
  df2 <- df %>%
    filter(cluster == clust) %>%
    select(-length) %>%
    droplevels()
  for (i in 1:length(levels(df2$index))) {  
    # for (i in 1:10) {
    SACTN_df_1 <- filter(df2, index == levels(index)[i])
    
    # Find the cumulative distance of site 1 along the coast
    site_dist_1 <- site_list_clusters %>% 
      filter(index == as.character(SACTN_df_1$index)[1])
    site_dist_1 <- knnx.index(data = as.matrix(sa_coast_dist[,1:2]),
                              query = as.matrix(site_dist_1[,5:6]), k = 1)
    site_dist_1 <- sa_coast_dist$cum_dist[site_dist_1]
    
    for (j in 1:length(levels(df2$index))) {
      # for(j in 1:10) {
      if (i == j) {
        next
      }
      if (j < i) {
        next
      }
      SACTN_df_2 <- filter(df2, index == levels(index)[j])
      
      # Find the cumulative distance of site 2 along the coast
      site_dist_2 <- site_list_clusters %>% 
        filter(index == as.character(SACTN_df_2$index)[1])
      site_dist_2 <- knnx.index(data = as.matrix(sa_coast_dist[,1:2]),
                                query = as.matrix(site_dist_2[,5:6]), k = 1)
      site_dist_2 <- sa_coast_dist$cum_dist[site_dist_2]
      
      # # The distance between the two sites
      site_dist_3 <- round(abs(site_dist_1-site_dist_2), 0)
      
      SACTN_df_3 <- left_join(SACTN_df_1, SACTN_df_2, by = "date") %>%
        na.trim() %>%
        select(date, index.x, temp.x, index.y, temp.y, -cluster.x, -cluster.y) %>%
        dplyr::rename(index_1 = index.x,
                      temp_1 = temp.x,
                      index_2 = index.y,
                      temp_2 = temp.y) %>%
        mutate(index_pair = paste0(index_1, " - ", index_2, " - ", site_dist_3),
               index_dist = site_dist_3)
      SACTN_clust_x_match <- rbind(SACTN_clust_x_match, SACTN_df_3)
    }
  }
  return(SACTN_clust_x_match)
}