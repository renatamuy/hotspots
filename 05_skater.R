#-------------- SKATER and maxp_sa ---------------------
# Based on 
#https://geodacenter.github.io/rgeoda/articles/rgeoda_tutorial.html#spatial-clustering

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

tofocus <- colnames(   dfsub %>% select(!c('x','y'))    )

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

filenamesh <- paste0('clusters_rgeoda_c20_rows',
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
# Cluster optimization based on maxp_sa

prepdf <- read.csv('prepdf.csv')

bound_variable <- prepdf['pop_2020_worldpop']

min_bound <- 0.05 * sum(bound_variable) # at least 5% pop?

maxp_clusters <- maxp_sa(queen_w, 
                         dfsub[,tofocus], 
                         bound_variable, min_bound, 
                         cooling_rate = 0.85, 
                         sa_maxit=1)

maxp_clusters

save(maxp_clusters, file = "maxp_clusters.RData")

