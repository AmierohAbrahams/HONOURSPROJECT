install.packages("fetchR")
library(fetchR)
library(rgdal)

setwd("wind_direction")

safrica =readOGR (dsn = "wind_direction", layer = "SA")

# Using rough co-ordins
sites = data.frame(lon = c( 18.27,  18.12, 31.41),
                   lat = c(-34.27, -34.12, -30.09 ),
                   name = c("p", "p2","p3"))

sites_locs = SpatialPoints(sites[, 1:2], CRS(proj4string(safrica)))

safrica_proj = spTransform(safrica, CRS("+init=epsg:2055")) ##(reference system is Hartebeesthoek94 aka South Africas system)
sites_locs_proj = spTransform(sites_locs, CRS("+init=epsg:2055"))

windfetch = fetch(polygon_layer = safrica_proj,
                      site_layer = sites_locs_proj,
                      max_dist = 300,
                      n_directions = 9,
                      site_names = sites$name)

windfetch

windfetch_latlon = spTransform(windfetch, CRS(proj4string(safrica)))
windfetch_latlon.df = as(windfetch_latlon, "data.frame")
head(windfetch_latlon.df)
plot(windfetch_latlon, safrica)

kml(windfetch_latlon, colour = "white", overwrite = TRUE)
