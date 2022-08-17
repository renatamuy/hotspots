# Clusters with ClustGeo
#https://cran.r-project.org/web/packages/ClustGeo/vignettes/intro_ClustGeo.html

library(ClustGeo)
library(raster)
library(here())
library(tidyverse)
library(rgeos)

setwd(here())

setwd('results')

# read all data 

d <- raster::shapefile("clusters_rgeoda_c20_random25796_mercator.shp")

dat <- d@data %>% as_tibble() %>% 
  select(!contains("cluster"))

#                  
D0 <- dist(dat)

tree <- hclustgeo(D0)

plot(tree,hang = -1, label = FALSE, 
     xlab = "", sub = "",
     main = "Ward dendrogram with environmental data only")

rect.hclust(tree , k = 5, border = c(4,5,3,2,1))

legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

# Geographical data 

d$cluster <- NULL
# Get distances with rgeos first
# GEOS expects planar coordinates

Dgeos <- gDistance(d, byid=T)

D1 <- as.dist(d) 

range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0, D1, range.alpha, K, graph = FALSE)
cr$Q # proportion of explained inertia

plot(cr)

