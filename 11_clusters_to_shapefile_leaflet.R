# Cluster overlays

require(stringr)
require(rnaturalearth)
require(tidyverse)
require(reshape2)
require(here)
library(gridExtra)
library(grid)
require(bivariatemaps)
library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)

# Open shapefiles of clusters
setwd(here())
setwd('results/skater_optimal_cluster_size_09/')

s9 <- shapefile('clusters_rgeoda_c09.shp')

setwd(here())
setwd('region')

raster_access <- raster('motor_travel_time_weiss.tif')

mini <- min(na.omit(values(raster_access)))
maxi <- max(na.omit(values(raster_access)))
values(raster_access) <-( values(raster_access) - mini)/ (maxi-mini)

ras_dom <-raster::raster(xmn=68.25, xmx= 141.0, ymn=-10.25, ymx=53.5,
                         crs="+proj=longlat +datum=WGS84 +no_defs ",
                         resolution=res(raster_access), vals=NA)

s9d <- s9@data
s9d$x <- coordinates(s9)[,1]
s9d$y <- coordinates(s9)[,2]

coordinates(s9d) <- ~ x + y 

crs(s9d) <- "+proj=longlat +datum=WGS84 +no_defs "

# Transform it onto raster 

s9r <- rasterize(s9d, ras_dom, field = c("cluster"), update = TRUE) 

plot(s9r)

# Retrieve shapefile back

s9s <- rasterToPolygons(s9r, dissolve = TRUE)

nrow(s9s@data)

# Export as shapefile

# Open shapefiles of clusters
setwd(here())
setwd('results/skater_optimal_cluster_size_19/')

s19 <- shapefile('clusters_rgeoda_c19_rows.shp')

s19d <- s19@data
s19d$x <- coordinates(s19)[,1]
s19d$y <- coordinates(s19)[,2]

coordinates(s19d) <- ~ x + y 

crs(s19d) <- "+proj=longlat +datum=WGS84 +no_defs "

# Transform it onto raster 

s19r <- rasterize(s19d, ras_dom, field = c("cluster"), update = TRUE) 

par(mfrow=c(1,2))
plot(s9s, lwd=1)
plot(s19s, lwd=1)



# Retrieve shapefile back

s19s <- rasterToPolygons(s19r, dissolve = TRUE)

nrow(s19s@data)

plot(s19s)

# Leaflets
library(lattice)
library(ggplot2)
library(ggmap)
library(sp)
library(raster)
library(sp)
library(dplyr)
library(leaflet)
library(randomcolorR)
require(platexpress)
library(htmltools)
library(leaflet.opacity)
library(leafem)
library(rnaturalearth)
library(rnaturalearthdata)
library(ncdf4)
require(sf)
library(RColorBrewer)

# Export as shapefile

setwd(here())
setwd('results/skater_optimal_cluster_size_19')
shapefile(s19s, 's19s.shp')

setwd(here())
setwd('results/skater_optimal_cluster_size_09')
shapefile(s9s, 's9s.shp')

# Make leaflet

popups <- paste("Cluster:", s9s@data$layer, "<br>")

pal9 <- wesanderson::wes_palette("Moonrise3", length(unique(s9s@data$layer)), type = "continuous")
pal19 <- wesanderson::wes_palette("Moonrise3", length(unique(s19s@data$layer)), type = "continuous")

popups9 <- paste("Cluster9:", s9s@data$layer, "<br>")
popups19 <- paste("Cluster:", s19s@data$layer, "<br>")

leaflet(s19s) %>% 
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
    addPolygons( weight = 2, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0.1,
              fillColor = pal19,
              highlightOptions = highlightOptions(color = "yellow", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addPolygons(popup = popups19, weight = 1, group = "Cluster" ) %>% 
  #addPolygons(data = s9s, weight = 1, smoothFactor = 0.5,
   #          opacity = 1.0, fillOpacity = 0.5,
    #       fillColor = pal9, highlightOptions = highlightOptions(color = "yellow", weight = 2,
     #                                         bringToFront = FALSE)) %>% 
  #addPolygons(popup = popups9, weight = 1, group = "Cluster9" ) %>% 
    #addRasterImage(rbin, opacity = 0.7, colors = palbin, group="Suitable areas for Lonchophylla dekeyseri") %>% 
  #addLegend(pal = palbin, values=values(rbin),  title = "Suitable areas for Lonchophylla dekeyseri", position = "bottomright") %>% 
  #addCircleMarkers(lng = coordinates(s19s)[,1] , lat = coordinates(s19s)[,2], weight=1.2,
  #                 color = "black" , group = "Occurrences" , popup = popups) %>% 
  #addRasterImage(mines, colors = palmines, opacity = 0.6, group="Mining exploitations (SIGMINE)") %>% 
  #addLegend(pal = palmines, values=values(mines),  title = "Mining exploitations (SIGMINE)", position = "bottomright") %>%
  addLayersControl(  baseGroups = c( "Esri WorldImagery","Open SM"),
                     options = layersControlOptions(collapsed=FALSE),
                     overlayGroups =c("Cluster" ))
                                      #"Protected areas",
                                      #"Suitable areas for Lonchophylla dekeyseri",
                                      #"Mining exploitations (SIGMINE)"   ) ) 


# open rasters
setwd(here())
setwd('region')
filenames <- list.files()

f <- filenames[stringr::str_ends(filenames, pattern= ".tif" , negate = FALSE)]

s <- lapply(f, raster)

def <- s[[12]]
crs(def) <- CRS("+init=EPSG:4326")

paldef <-colorFactor("PRGn", values(def), na.color = "transparent")

leaflet(s19s) %>% 
  addTiles() %>%
  addProviderTiles(providers$OpenStreetMap, group = 'Open SM') %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri WorldImagery") %>% 
  addPolygons( weight = 2, smoothFactor = 0.5,
               opacity = 1, fillOpacity = 0.1,
               fillColor = pal19,
               highlightOptions = highlightOptions(color = "yellow", weight = 2,
                                                   bringToFront = TRUE)) %>% 
  addPolygons(popup = popups19, weight = 1, group = "Cluster" ) %>% 
 addRasterImage(def, colors = paldef, opacity = 0.6,
                group="Deforestation risk") %>% 
 addLegend(pal = paldef, values=values(def), 
           title = "Deforestation risk", 
           position = "bottomright") %>%
addLayersControl(  baseGroups = c( "Esri WorldImagery","Open SM"),
                   options = layersControlOptions(collapsed=FALSE),
                   overlayGroups =c("Cluster", 'Deforestation risk' ))


# Plot with vote count over
