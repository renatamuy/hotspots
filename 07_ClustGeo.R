# Clusters with ClustGeo
# Ward-like hierarchical clustering algorithm with soft contiguity constrain
#https://cran.r-project.org/web/packages/ClustGeo/vignettes/intro_ClustGeo.html

rm(list=ls())

library(ClustGeo)
library(raster)
library(here())
library(tidyverse)
library(rgeos)

setwd(here())

setwd('results')

gc()

# read  data 

mercator <-raster::shapefile("mercator.shp")

g <- read.csv('gstar.csv')

mercator$cluster <- NULL

tofocus <- colnames(  mercator@data %>% dplyr::select(!c( contains("95"),
                                               'hsts_my', 'hsts_sn',
                                               'trans',
                                               'pollutn',
                                               'mtr_t__' ))    )


library(ClustGeo)

# SLOW -------------------------
# with one dissimilarity matrix
D <- dist(mercator@data[tofocus])
tree <- hclustgeo(D)
sum(tree$height)
inertdiss(D)
inert(mercator@data[tofocus])
plot(tree,labels=FALSE)
part <- cutree(tree,k=5)
sp::plot(mercator, border = "grey", col = part)

# with two dissimilarity matrix
Dgeo <- gDistance(mercator, byid=T) # the geographical distances
D1 <- as.dist(Dgeo) 
alpha <- 0.2 # the mixing parameter
treeg <- hclustgeo(D,D1,alpha=alpha)
plot(treeg,labels=FALSE)
partg <- cutree(treeg,k=5)
sp::plot(mercator, border = "grey", col = partg)

#### Plotting, evaluating, exporting for further map scripts

#setwd(here())
#setwd('results')
#setwd('trees_ward')
#load('tree.RData')
#load('tree.RData')

x<-tree$merge[,c(1:2)]
part_dtsh <- cutree(tree,k=1:40)
SS <- aggregate(x, by=list(part_dtsh[-c(1), 6]),
                function(x) sum(scale(x,
                                      scale=FALSE)^2))
SS
SS.a <- rowSums(SS[, -1])
TSS <- sum(SS[, -1])  
TSS <- function(x, g) {
  sum(aggregate(x, by=list(g), function(x) sum(scale(x, 
                                                     scale=FALSE)^2))[, -1])
}
TSS.al <- apply(part_dtsh[-c(1),], 2, function(g) TSS(x, g))
TSS.al
plot(TSS.al, xlab = "Cluster", ylab = 'Score', pch = 16, yaxt='n')

# Spatial TSS
part_dtshg <- cutree(treeg,k=1:40)
TSS.alg <- apply(part_dtshg[-c(1),], 2, function(g) TSS(x, g))
plot(TSS.alg, xlab = "Ward-like cluster", ylab = 'Score', pch = 16, yaxt='n')

# Non-spatial 10
part <- cutree(tree,k=10)
g$tree10 <- part 
par(mfrow = c(2, 2))
par(mar = c(1, 1, 1, 1))
sp::plot(estuary, border = "grey", col = part, pch=19)
plot(tree,labels=FALSE)
rect.hclust(tree, k =10, border = unique(part) )


# Spatial 10
partg <- cutree(treeg,k=10)
g$treeg10 <- partg 
sp::plot(estuary, border = "grey", col = partg, pch=19)
plot(treeg,labels=FALSE)
rect.hclust(treeg, k =10, border = unique(partg))

# Non Spatial 12
part <- cutree(tree, k=12)
g$tree12 <- part 
par(mfrow = c(2, 2))
par(mar = c(1, 1, 1, 1))
sp::plot(estuary, border = "grey", col = part, pch=19)
plot(tree,labels=FALSE)
rect.hclust(tree, k =12, border = unique(part))

# Spatial 2
partg <- cutree(treeg,k=12)
g$treeg12 <- partg 
sp::plot(estuary, border = "grey", col = partg, pch=19)
plot(treeg,labels=FALSE)
rect.hclust(treeg, k =12, border = unique(partg)) 

setwd(here())
setwd('results')
dir.create('trees_ward')
setwd('trees_ward')

#Exporting objects
save(tree, file = 'tree.RData')
save(treeg, file = 'treeg.RData')
save(g, file = 'g_ward_clusters.RData')
write.csv(g, file = 'g_ward_clusters.csv', row.names = FALSE)

#-----------------------------