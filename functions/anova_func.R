###Anova function###

anova_func <- function(df){
  sites_aov <- aov(temp_mean_month_year ~ index_pair * year * season, data = df)
  return(sites_aov)
}