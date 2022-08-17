#-------------- SKATER ---------------------

library(sp)
library(spdep)
library(here())
library(tidyverse)

setwd(here())

setwd('results')

dfg <- read.csv('gstar.csv')

dfg$ID <- NULL 

# Subset (if slow)

dfsub <- dplyr::sample_n(dfg, 10000) # want to increase!

# all data 

dfsub <- dfg

dfsub <- dfsub %>% select(c('x','y', !contains("95")))

colnames(dfsub)

#------------------------------
# Comparing with geoda skater

require(rgeoda)

queen_w <- queen_weights(sf::st_as_sf(dfsub, coords = c("x","y")))

# No need to rescale as we have the z-scores as input here

clusg <- rgeoda::skater(20,  queen_w,  dfsub[,tofocus],
                        scale_method = "raw",
                        distance_method = "euclidean",
                        random_seed = 123456789 )

clusg$Clusters

# assign clusters to spatial object (careful, names reduced when exported)

result <- dfsub

coordinates(result)<-~x+y

raster::crs(result) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

result$cluster <- clusg$Clusters

colnames(result@data)

class(result)

#------------------------------
# Export shapefile of points and csv 

colwant <- c(tofocus, 'cluster')

colnames(result[colwant]@data)

filenamesh <- paste0('clusters_rgeoda_c20_random',
                     nrow(result),       ".shp")

raster::shapefile(result[colwant], filenamesh,  overwrite=TRUE)

hist(table(result[colwant]@data$cluster))

resultdf <- result@data
resultdf$x <- dfsub$x
resultdf$y <- dfsub$y

filenamec <- paste0('clusters_rgeoda_c20_random',
                     nrow(dfsub),       ".csv")

write.csv(resultdf, file = filenamec, row.names = FALSE)

#----------------------------------------------------------------------------------