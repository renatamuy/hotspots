#-------------- SKATER ---------------------

library(sp)
library(spdep)
library(here())

setwd(here())

setwd('results/hotspots_region')


dfg <- read.csv('gstar.csv')

dfsub <- dplyr::sample_n(dfg, 1000)

coords <- cbind(dfsub$x, dfsub$y)

#Please also note that triangulation of grid points will give arbitrary 
# diagonal neighbours, which may not be a sensible outcome, 
# and dnearneigh() may serve better where tri2nb() cannot be used.
# rule 8: queens' case

neighbours = tri2nb(coords, row.names = NULL)

# nbcosts method:
#Character or function to declare distance method. If method is character, method must be "mahalanobis" or "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowisk". If method is one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowisk", see dist for details, because this function as used to compute the distance. If method="mahalanobis", the mahalanobis distance is computed between neighbour areas. If method is a function, this function is used to compute the distance.

lcosts <- nbcosts(neighbours, dfsub)

nb <- nb2listw(neighbours, lcosts, style="B")

# Minimum spanning tree - SLOW

mst <- mstree(nb)

edgelist = mst[,1:2]

# Plot min span tree

plot(mst, coords, col = 2,  cex.lab= .6, cex.circles=0.035, fg="blue")

# Opening 

setwd(here())
setwd('results')

#save(mst.bh, file = "mstree.RData")
#save(edges1, file = "edges.RData")

#load("mstree.RData")
#load("edges.RData")

queen_w <- queen_weights(guerry)

tofocus <- c("cattle_gilbert",
             "pig_gilbert"  ,
             "chicken_gilbert" ,
             "forest_division_jung" ,
             "forest_integrity_grantham" ,
             "lincomb_hosts",
             "mammals_iucn_mode"  ,
             "motor_travel_time_weiss"  ,
             "pop_2020_worldpop"         )

# it works by iteratively partitioning the graph by identifying which edge to 
#remove to maximize the quality of resulting clusters as measured by the sum of
# the intercluster square deviations SSD. 
#Regions that are similar to one another will have lower values. 
#This is implemented via spdep::skater and the ncuts arg 
#indicates the number of partitions to make, resulting in ncuts+1 groups.

# Cluster analysis with 9+1 cuts
#This function implements a spatial constrained cluster-wise regression based on the Skater procedure
#by Assuncao et al. (2002).

ncutsless1 <- 29

clus <- skater(edges = edgelist, data = dfsub[,tofocus], ncuts = ncutsless1)
     
# check cluster ----------------------------------------------------------------------------------------------

dfsp <- dfsub
coordinates(dfsp)<-~x+y

raster::crs(dfsp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

co <- rev(terrain.colors(length(clus$edges.groups)))

par(mfrow=c(2,1))

# Figures

plot.skater(clus, coords)

plot(mst, coords, col = 2,  cex.lab= .6, cex.circles=0.035, fg="blue")

plot(clus, coordinates(dfsp), cex.circles = 0.35, cex.lab = 0.7, groups.colors = co)

clus$edges.groups[[30]]$node

# ssto = The total dissimilarity in each step of edge removal
# ssw = within cluster sim?

filename <- paste0('clus_spdep_',nrow(dfsub), '_', ncutsless1 + 1 ,'cuts' ,  ".RData")

save(clus, file = filename)

#save.image(file = "my_work_space.RData")
#load("my_work_space.RData")


#https://www.dshkol.com/post/spatially-constrained-clustering-and-regionalization/

#------------------------------

# Comparing with geoda skater

require(rgeoda)

queen_w <- queen_weights(sf::st_as_sf(dfsub, coords = c("x","y")))

clusg <- rgeoda::skater(20,  queen_w,  dfsub[,tofocus],
   scale_method = "raw",
  distance_method = "euclidean",
  random_seed = 123456789 )

clusg$Clusters

# assign clusters to df

result <- dfsp
result$clusg <- clusg$Clusters

colnames(result@data)
class(result)

colwant <- c(tofocus, 'clusg')

# Export shapefile of points 
colnames(result[colwant])

result[colwant]@data

raster::shapefile(result[colwant], "random1000_rgeoda_c20.shp", overwrite=TRUE)


#------------------------------
