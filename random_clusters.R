#-------------- SKATER RANDOM ---------------------
# Reference: https://geodacenter.github.io/rgeoda/articles/rgeoda_tutorial.html#spatial-clustering


set.seed(12345)

memory.size()
memory.limit()

ls()
rm(list = ls())
gc() 
options(digits=7, scipen=999)

library(sp)
library(spdep)
library(here)
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


#------------------------------
# Get projected coords, generate weights matrix ---
mercator <- raster::shapefile('mercator.shp')

queen_w <- queen_weights(st_as_sf(mercator))

head(dfsub[,tofocus])

# Creating random data

hist(dfsub[,tofocus]$pig_gilbert)

hist(dbeta(runif(nrow(rd)), shape1 = 5, shape2 = 20) )

#dir.create('skater_random')
setwd('skater_random')

rd <- data.frame(replicate(10,rnorm(nrow(dfsub), 0, 1)))
colnames(rd) <- colnames(dfsub[,tofocus])

write.csv(rd, row.names = FALSE, file= 'random_data.csv')

rd <- read.csv('random_data.csv')

#head(rd)


# iterations

its <- c(40:1)

for(i in its){
  
  print(i)
  
  clusg <- rgeoda::skater(i,  queen_w,  rd,
                          scale_method = "raw",
                          distance_method = "euclidean" )
  
  save(clusg, file = paste0(i,'_clusters_random.RData') ) 
  
}

ratio <- c()

twss <-c()

loads <- c(1:40)

for(lo in loads){
  print(lo)
  load(paste0(lo,'_clusters_random.RData') ) 
  
  twss <- c(twss, clusg$`Total within-cluster sum of squares`)
  
  ratio <- c(ratio,  clusg$`The ratio of between to total sum of squares` )
  
}

ratio
twss



# Elbow plots
#

png(filename= 'elbow_plot_random.png', width = 20, height = 14, unit='cm',
    res=300)
plot(twss ~ loads, pch = 19, xlab= 'Number of clusters (random)',
     ylab='Total within-cluster sum of squares'  )
dev.off()

png(filename= 'elbow_plot_ratio_random.png', width = 20, height = 14, unit='cm',
    res=300)
plot(ratio ~ loads, pch = 19, xlab= 'Number of clusters',
     ylab= 'Ratio of between/total sum of squares')
dev.off()


# Export ss data

filenametwss <- paste0('clusters_twss_ratio_random', ".csv")

write.csv(data.frame(its, twss, ratio), file = filenametwss, row.names = FALSE)

# After inspecting
# assign optimal k clusters to spatial object (careful, col names reduced when exported)

otimo <- 9

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

dir.create('skater_optimal_cluster_size_09')
setwd('skater_optimal_cluster_size_09')
colwant <- c(tofocus, 'cluster')

colnames(result[colwant]@data)

filenamesh <- paste0('clusters_rgeoda_c09', ".shp")

raster::shapefile(result[colwant], filenamesh,  overwrite=TRUE)

hist(table(result[colwant]@data$cluster))

resultdf <- result@data
resultdf$x <- dfsub$x
resultdf$y <- dfsub$y

filenamec <- paste0('clusters_rgeoda_c09', ".csv")

write.csv(resultdf, file = filenamec, row.names = FALSE)

#--------------------------------------------------------------------------------
#   #          #  
#  #           # 
#    #       #
#     ###### 
#      ####
#       ##
#-------------------------------------------------------------------------------

# bovliv (all bovidae livestock) -------------------------

setwd(here())
setwd('results')

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

its <- c(40:1)

dir.create('skater_bovliv')
setwd('skater_bovliv')

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
setwd('results/skater_bovliv')

load(paste0(otimo,'_clusters.RData') ) 

table(clusg$Clusters)

result$cluster <- clusg$Clusters

colnames(result@data)

class(result)

#---- 
#---------
#------------------------------
# Export shapefile of points and csv bovliv

setwd('../')

dir.create('skater_optimal_cluster_size_19_bovliv')
setwd('skater_optimal_cluster_size_19_bovliv')
colwant <- c(tofocus, 'cluster')

colnames(result[colwant]@data)

filenamesh <- paste0('clusters_rgeoda_c19',  ".shp")

raster::shapefile(result[colwant], filenamesh,  overwrite=TRUE)

resultdf <- result@data
resultdf$x <- dfsub$x
resultdf$y <- dfsub$y

filenamec <- paste0('clusters_rgeoda_c19', ".csv")

write.csv(resultdf, file = filenamec, row.names = FALSE)

#--------------------------------------------------------------------------------