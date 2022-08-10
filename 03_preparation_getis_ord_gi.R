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

quero <- list.files(pattern = '.tif')

all <- stack(list.files(pattern = '.tif')) 

#plot(all)

plot(is.na(all))

res(all)

# For this data, you should have NAs in ~48060 cells.
summary(all)

# So you remove the known NAs by their ID rows to go on with spatial analysis

# Also, you replace NAs by zero in layers such as Sanchez data and Edge distance data to match region

# From stack to dataframe

p <- data.frame(rasterToPoints(all))

table(is.na(p))

# Data to lose
sum(colSums(is.na(p)))


# Number of rows lost (excluding edge distance) -----------------------#########
p$edge_distance_jung <- NULL

# Correct later in extraction

nrow(na.omit(p)) - nrow(p)


prepdf <- data.frame(na.omit(p))

# ------------------------------------------------------------------------------

# Number of rows 

dim(all)[1]*dim(all)[2]

nrow(prepdf)

# Export p for getis ord Gi* analysis

plot(prepdf$x, prepdf$y)

head(prepdf)

setwd(here())
dir.create('results')
setwd('results')
dir.create('hotspots_region')
setwd('hotspots_region')
write.csv(prepdf, 'prepdf.csv')


#-----------------------------
