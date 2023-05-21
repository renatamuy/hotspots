library(raster)
library(sp)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
require(sf)

## Attention, we had to manually add country-cluster interactions for Timor East and West Papua, 
# as this shapefile  does not acknowledge them

target <- ne_countries(type = "countries", country = c('Bangladesh',
                                                       'Bhutan',
                                                       'Brunei',
                                                       'Cambodia',
                                                       'China', 
                                                       'India',
                                                       'Indonesia',
                                                       'Laos',
                                                       'Malaysia',
                                                       'Myanmar',
                                                       'Nepal',
                                                       'Philippines',
                                                       'Singapore', 
                                                       'Sri Lanka',
                                                       'Thailand',
                                                       'Timor-Leste', #not in
                                                       'Vietnam'))

countrysf <- sf::st_as_sf(target)

countrysf$formal_en

setwd(here())

setwd('results')

dir.create('countries_shapefile')

setwd('countries_shapefile')

shapefile(target, 'countries.shp')

input_path <- "C:/Users/rdelaram/Documents/GitHub/hotspots/results/countries_shapefile/countries.shp"

join_path <- "C:/Users/rdelaram/Documents/GitHub/hotspots/results/skater_optimal_cluster_size_19/s19s.shp"

input_sf <- st_read(input_path)

join_sf <- st_read(join_path)

output_sf <- st_join(input_sf, join_sf, join = st_intersects)

nrow(input_sf)
nrow(output_sf)

st_write(output_sf, "countries_and_clusters_sf.shp")

edges_list <- data.frame(country= output_sf$name, clusters= output_sf$layer)

edges_list

# Option: Adding Timor and West Papua interactions

manual <- data.frame(country= c('Timor East', 'West Papua'), clusters= c(9, 9))

edges_list <- rbind(edges_list, manual) 

table(edges_list$clusters)

sum(table(edges_list$clusters) == 1)

#---------------------------------------------------------------------------------------------------
