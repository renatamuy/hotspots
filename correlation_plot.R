# Data exploring

require(raster)
require(here)
require(tidyverse)

# read prepared rasters

setwd(here())
setwd('region/')

# Read all prepared rasters for the region of intest

quero <- list.files(pattern = '.tif')
quero <- c("motor_travel_time_weiss.tif" ,
           "pig_gilbert.tif" ,
           "cattle_gilbert.tif" ,
           "hosts_sanchez.tif",
           "mammals_iucn_mode.tif" ,
           "pop_2020_worldpop.tif" ,
           "buffalo_cattle_goat_sheep.tif"   ,
           "builtup.tif"    ,
           "energy.tif" ,
           "forest_integrity_grantham.tif"  ,       
           "hewson_forest_transition_potential.tif",
           "hosts_muylaert.tif"
)

all <- stack(quero)

d <- data.frame(rasterToPoints(all) )

introduce(d)

head(d)

# add trade and show correlations

colnames(d) <- c(  "x"    ,                              "y"    ,                              "Trave time to healthcare (motor)"    ,        "Pig"      ,                 
                   "Cattle"    ,                 " Bat hosts (Sanchez et al. 2022)",
                   "Wild mammals"         ,         "Human population (2020)"    , 'Bovidae livestock',            
                   "Builtup land"  ,                          "Mining and energy"  , 
                   "Forest quality"  ,        "Deforestation risk",
                   "Bat hosts (Muylaert et al. 2022)" )

plot_correlation(na.omit(d[3:ncol(d)]), maxcat = 5L)

#-----------------------------------------------------------------------