# Clusters with ClustGeo
#https://cran.r-project.org/web/packages/ClustGeo/vignettes/intro_ClustGeo.html

rm(list=ls())

library(ClustGeo)
library(raster)
library(here())
library(tidyverse)
library(rgeos)

setwd(here())

setwd('results')

# read all data 

ds <- raster::shapefile("mercator.shp")

g <- read.csv('gstar.csv')

tofocus <- colnames(   g %>% dplyr::select(!c('x','y','ID', contains("95"),
                                               'hosts_muylaert', 'hosts_sanchez',
                                               'trans',
                                               'pollution',
                                               'motor_travel_time_weiss' ))    )

#d <- ds[sample(1:length(ds), 1000),]

d <- g[,tofocus]

d <- cbind(d, ds)

d <- d[,tofocus]

d$cluster <- NULL

# In case weights are wanted (I did not use them)
w <- d['pop_2020_worldpop']

dat <- d 

D <- as.dist(dist(d)) # environmental distances

tree <- hclustgeo(D, wt=NULL)

sum(tree$height)
inertdiss(D,wt=NULL)
inert(dat,w=NULL)
plot(tree,labels=FALSE)
part <- cutree(tree, k=5)
sp::plot(dat, border = "grey", col = part)

# with two dissimilarity matrix
Dgeo <- gDistance(ds, byid=T)
D1 <- as.dist(Dgeo) # the geographical distances
alpha <- 0.2 # the mixing parameter
tree <- hclustgeo(D, D1, alpha=alpha, wt=NULL)
plot(tree,labels=FALSE)
part <- cutree(tree,k=5)

sp::plot(dat, border = "grey", col = part)

#### 
x<-tree$merge[,c(1:2)]
part_dtsh <- cutree(tree,k=1:25)
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

## interestingly for this 'toy' data set you can see there are two 'elbow' bends, around 6 and 20, so let's plot them

part <- cutree(tree,k=6)
par(mfrow = c(2, 2))
par(mar = c(1, 1, 1, 1))
sp::plot(dat, border = "grey", col = part)
plot(tree,labels=FALSE)
rect.hclust(tree, k = 6, border = part)

part <- cutree(tree,k=20)
sp::plot(dat, border = "grey", col = part)
plot(tree,labels=FALSE)
rect.hclust(tree, k = 20, border = part)

