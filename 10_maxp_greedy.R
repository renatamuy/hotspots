#-------------- MAXP GREEDY ---------------------

ls()
rm(list = ls())
gc() 
options(digits=7, scipen=999)

library(sf)
library(sp)
library(spdep)
library(here)
library(rgeoda)
library(raster)

setwd(here())

setwd('results')

d <- read.csv('prepdf.csv')

d$X <- NULL

mercator <- raster::shapefile('mercator.shp')

queen_w <- queen_weights(st_as_sf(mercator))

tofocus <- colnames(d %>% dplyr::select(!c('x','y', 
                                           'hosts_muylaert',
                                           'hosts_sanchez',
                                           'trans',
                                           'pollution',
                                           'motor_travel_time_weiss'   ))    )



# 5% pop

minbound <- 0.05*sum(d$pop_2020_worldpop) 

maxp_o5pct <- maxp_greedy(
  w = queen_w,
  df = d[,tofocus],
  bound_variable = d['pop_2020_worldpop'], # always index it as df, not as vector
  min_bound = minbound,
  iterations = 99,
  initial_regions = vector("numeric"),
  scale_method = "standardize",
  distance_method = "euclidean",
  random_seed = 123456789)

setwd(here())

setwd('results')

dir.create('maxp')

save(maxp_o5pct, 'maxp_o5pct.RData')

print('5% bounding variable completed')

# 10% pop

minbound10pct <- 0.1*sum(d$pop_2020_worldpop) 

maxp_o10pct <- maxp_greedy(
  w = queen_w,
  df = d[,tofocus],
  bound_variable = d['pop_2020_worldpop'], # always index it as df, not as vector
  min_bound = minbound10pct,
  iterations = 99,
  initial_regions = vector("numeric"),
  scale_method = "standardize",
  distance_method = "euclidean",
  random_seed = 123456789)

print('10% bounding variable completed')

# Export

save(maxp_o10pct, 'maxp_o10pct.RData')

d$maxp5pct <- maxp_o5pct$Clusters
d$maxp10pct <- maxp_o10pct$Clusters

save(d, 'prepdf_maxp.RData')

write.csv(d, 'prepdf_maxp.RData', row.names = FALSE)

print('Job completed')





#------------------------------------------------------------------------
# Description: https://geodacenter.github.io/rgeoda/articles/rgeoda_tutorial.html#max-p

#max-p regions model (outlined in Duque, Anselin, and Rey 2012) 
#uses a different approach and considers the regionalization problem 
#as an application of integer programming. 
#Besides, the number of regions is determined endogenously.
#The algorithm itself consists of a search process that starts 
# with an initial feasible solution and iteratively improves upon it 
#while maintaining contiguity among the elements of each cluster. 

#NOTE: the max-p algorithm is very sensitive to the initial positions 
#for constructing final solutions. Therefore, the random seed, 
#which is used to determine the initial positions, 
#could be used to execute several rounds of max-p algorithms for 
# sensitive analysis.