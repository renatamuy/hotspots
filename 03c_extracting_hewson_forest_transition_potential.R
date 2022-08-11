# forest loss transition potential

# Hewson et al 2019

# Extracting data for study region matchin res and extention
library(here)
require(raster)
library(rnaturalearth)
library(sp)
library(scico)

setwd(here())

# Read estimated bat richness layer

setwd('data')
r <- raster('richnessm.tif')

# Define working extent 
##############################################################################################################
# not included: Taiwan, Papua New Guinea

target <- ne_countries(type = "countries", country = c('Bangladesh',
                                                       'Bhutan',
                                                       'Brunei',
                                                       'Cambodia',
                                                       'China', 
                                                       'India',
                                                       'Indonesia',
                                                       'Laos',
                                                       'Malaysia',
                                                       'Myanmar',
                                                       'Nepal',
                                                       'Philippines',
                                                       'Singapore', 
                                                       'Sri Lanka',
                                                       'Thailand',
                                                       'Timor-Leste',
                                                       'Vietnam'))

plot(target, col='blue')
#plot(r, add=TRUE)

tr <- raster(target, res=res(r), crs= crs(r))

values(tr) <- 1
plot(tr)

a <- runif(dim(tr)[1]*dim(tr)[2] , min=0 , max=1)

# Generate random background points

ab <- ifelse(a>0.5, 1, NA)

values(tr) <- ab

plot(tr)

#
setwd("hewson")

forloss <- raster("Continent_transition_potential.tif")

plot(forloss)

downr <- resample(forloss, r) # bilinear is default
downrc <- mask(downr, target)
downrcc <- crop(downrc, tr)

setwd(here())
setwd('data')
setwd('region')

writeRaster(downrcc, filename = 'hewson_forest_transition_potential.tif', format="GTiff", overwrite=FALSE)

#---------------------------------------------------------------------
