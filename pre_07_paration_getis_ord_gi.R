#---- Preparation for getis ord G*
#-------------------------------------------------------------------------------
require(raster)
require(here)
require(tidyverse)
require(xlsx)
# read prepared rasters

setwd(here())
setwd('data/region/')

# Read all prepared rasters for the region of intest

all <- stack(list.files(pattern = '.tif$')) 

names(all)
want_landscape <- c('builtup', 'energy', 'trans', 'agriharv', 'pollution',
                   'forest_integrity_grantham',
                   'hewson_forest_transition_potential'                  )

plot(is.na(all[[want_landscape]]))

plot(all[[want_landscape]])

want_sechost <- c('pig_gilbert', 'cattle_gilbert','mammals_iucn_mode')

plot(is.na(all[[want_sechost]]))

plot(all[[want_sechost]])

want_bat <- c('hosts_muylaert', 'hosts_sanchez')

plot(is.na(all[[want_bat]]))  


plot(all[[want_bat]])

want_expo_vuln <- c('pop_2020_worldpop', 'motor_travel_time_weiss')

plot(all[[want_expo_vuln]])

res(all)

p <- data.frame(rasterToPoints(all))

table(is.na(p))

# Data to lose
sum(colSums(is.na(p)))

# Number of rows lost (excluding edge distance) -----------------------#########
p$edge_distance_jung <- NULL

# Correct later in extraction

nrow(na.omit(p)) - nrow(p)

# Selection
want_coords <- c('x', 'y')
want_landscape
want_sechost
want_bat
want_expo_vuln

prepdf <- data.frame(na.omit(p[c(want_coords,want_landscape,want_sechost,want_bat, want_expo_vuln)]))

# ------------------------------------------------------------------------------

# Number of rows in raster

dim(all)[1]*dim(all)[2]

# Number of content data

nrow(prepdf)

# Export raw data prepdf for getis ord Gi* analysis

setwd(here())
dir.create('results')
setwd('results')
write.csv(prepdf, 'prepdf.csv')

#-----------------------------
