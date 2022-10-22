#-------------- SKATER ---------------------
# Based on 
#https://geodacenter.github.io/rgeoda/articles/rgeoda_tutorial.html#spatial-clustering

memory.size()
memory.limit()

ls()
rm(list = ls())
gc() 
options(digits=7, scipen=999)

library(sp)
library(spdep)
library(here())
library(tidyverse)
library(rgeoda)
library(raster)

setwd(here())
setwd('results')

dfg <- read.csv('gstar.csv')

dfg$ID <- NULL 

# Subset (if slow) dfsub <- dplyr::sample_n(dfg, 10000) # want to increase!

# all data 

dfsub <- dfg

dfsub <- dfsub %>% dplyr::select( c('x','y', !contains("95")) )

colnames(dfsub)

# cattle-only
tofocus <- colnames(dfsub %>% dplyr::select(!c('x','y', 
                                               'hosts_muylaert',
                                               'hosts_sanchez',
                                               "mammals_iucn_mode", 
                                               "bovliv",
                                               'trans',
                                               'pollution',
                                               'motor_travel_time_weiss'   ))    )

colnames(dfsub[tofocus])

#------------------------------
# Get projected coords, generate weights matrix ---
mercator <- raster::shapefile('mercator.shp')

queen_w <- queen_weights(st_as_sf(mercator))

head(dfsub[,tofocus])

# No need to rescale as we have the z-scores as input here

its <- c(38:40)
  
dir.create('skater')
setwd('skater')

for(i in its){
  
  print(i)
  
 clusg <- rgeoda::skater(i,  queen_w,  dfsub[,tofocus],
                        scale_method = "raw",
                        distance_method = "euclidean" )

save(clusg, file = paste0(i,'_clusters.RData') ) 

}

ratio <- c()

twss <-c()

loads <- c(1:40)

for(lo in loads){
    print(lo)
load(paste0(lo,'_clusters.RData') ) 

twss <- c(twss, clusg$`Total within-cluster sum of squares`)

ratio <- c(ratio,  clusg$`The ratio of between to total sum of squares` )

}

ratio
twss

# Elbow plots

png(filename= 'elbow_plot.png', width = 20, height = 14, unit='cm',
    res=300)
plot(twss ~ loads, pch = 19, xlab= 'Number of clusters',
     ylab='Total within-cluster sum of squares'  )
dev.off()

png(filename= 'elbow_plot_ratio.png', width = 20, height = 14, unit='cm',
    res=300)

plot(ratio ~ loads, pch = 19, xlab= 'Number of clusters',
     ylab= 'Ratio of between/total sum of squares')
dev.off()

# Export ss data

filenametwss <- paste0('clusters_twss_ratio', ".csv")

write.csv(data.frame(its, twss, ratio), file = filenametwss, row.names = FALSE)

# After inspecting
# assign optimal k clusters to spatial object (careful, col names reduced when exported)

otimo <- 19

result <- dfsub

coordinates(result)<-~x+y

raster::crs(result) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

setwd(here())
setwd('results/skater')

load(paste0(otimo,'_clusters.RData') ) 

table(clusg$Clusters)

result$cluster <- clusg$Clusters

colnames(result@data)

class(result)

#------------------------------
# Export shapefile of points and csv 

setwd('../')

dir.create('skater_optimal_cluster_size_19')
setwd('skater_optimal_cluster_size_19')
colwant <- c(tofocus, 'cluster')

colnames(result[colwant]@data)

filenamesh <- paste0('clusters_rgeoda_c19_rows', ".shp")

raster::shapefile(result[colwant], filenamesh,  overwrite=TRUE)

hist(table(result[colwant]@data$cluster))

resultdf <- result@data
resultdf$x <- dfsub$x
resultdf$y <- dfsub$y

filenamec <- paste0('clusters_rgeoda_c19', ".csv")

write.csv(resultdf, file = filenamec, row.names = FALSE)

#--------------------------------------------------------------------------------
#   #          #  
#  #           # 
#    #       #
#     ###### 
#      ####
#       ##
#-------------------------------------------------------------------------------
  
# bovliv -------------------------


tofocus <- colnames(dfsub %>% dplyr::select(!c('x','y', 
                                               'hosts_muylaert',
                                               'hosts_sanchez',
                                               "mammals_iucn_mode", 
                                               "cattle_gilbert",
                                               'trans',
                                               'pollution',
                                               'motor_travel_time_weiss'   ))    )

colnames(dfsub[tofocus])

#------------------------------
# Get projected coords, generate weights matrix ---
mercator <- raster::shapefile('mercator.shp')

queen_w <- queen_weights(st_as_sf(mercator))

# Rdata may corrupt spatial objects
#save(queen_w, file = 'queen_w.RData')
#load('queen_w.RData')

queen_w

head(dfsub[,tofocus])

# No need to rescale as we have the z-scores as input here

its <- c(1:40)

dir.create('skater')
setwd('skater')

for(i in its){
  
  print(i)
  
  clusg <- rgeoda::skater(i,  queen_w,  dfsub[,tofocus],
                          scale_method = "raw",
                          distance_method = "euclidean" )
  
  save(clusg, file = paste0(i,'_clusters.RData') ) 
  
}

ratio <- c()

twss <-c()

loads <- c(1:40)

for(lo in loads){
  print(lo)
  load(paste0(lo,'_clusters.RData') ) 
  
  twss <- c(twss, clusg$`Total within-cluster sum of squares`)
  
  ratio <- c(ratio,  clusg$`The ratio of between to total sum of squares` )
  
}

ratio
twss

# Elbow plots

png(filename= 'elbow_plot.png', width = 20, height = 14, unit='cm',
    res=300)
plot(twss ~ loads, pch = 19, xlab= 'Number of clusters',
     ylab='Total within-cluster sum of squares'  )
dev.off()

png(filename= 'elbow_plot_ratio.png', width = 20, height = 14, unit='cm',
    res=300)

plot(ratio ~ loads, pch = 19, xlab= 'Number of clusters',
     ylab= 'Ratio of between/total sum of squares')
dev.off()

# Export ss data

filenametwss <- paste0('clusters_twss_ratio', ".csv")

write.csv(data.frame(its, twss, ratio), file = filenametwss, row.names = FALSE)

# After inspecting

# assign optimal k clusters to spatial object (careful, col names reduced when exported)

otimo <- 19

result <- dfsub

coordinates(result)<-~x+y

raster::crs(result) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

setwd(here())
setwd('results/skater')

load(paste0(otimo,'_clusters.RData') ) 

table(clusg$Clusters)

result$cluster <- clusg$Clusters

colnames(result@data)

class(result)

#------------------------------
# Export shapefile of points and csv 

setwd('../')

dir.create('skater_optimal_cluster_size_19')
setwd('skater_optimal_cluster_size_19')
colwant <- c(tofocus, 'cluster')

colnames(result[colwant]@data)

filenamesh <- paste0('clusters_rgeoda_c19_rows',
                     nrow(result),       ".shp")

raster::shapefile(result[colwant], filenamesh,  overwrite=TRUE)

hist(table(result[colwant]@data$cluster))

resultdf <- result@data
resultdf$x <- dfsub$x
resultdf$y <- dfsub$y

filenamec <- paste0('clusters_rgeoda_c19', ".csv")

write.csv(resultdf, file = filenamec, row.names = FALSE)

#--------------------------------------------------------------------------------

