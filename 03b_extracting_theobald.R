# Theobald
# Workflow for characterizing bat points and background points 
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

# Open indicators
#"static_builtup" (urban & built-up), 
#"static_ag" (agriculture and biological harvesting of forests),
#"static_energy" (energy production and mining), 
#"static_trans" (transportation & service corridors), 
#and "static_intrusion" (human intrusions, natural system modifications, and pollution). 
#The original values of the datasets ranged from 0.0 to 1.0,
#where 0.0 is no human modification and 1.0 is full or complete human modification. 
#These values were represented as 32-bit floating point values, 
#but were converted to a 16-bit integer to reduce file size,#
#by multiplying by 32767. 
#Datasets are in EPSG 3857 coordinates and the extent of longitude/latitude is -180, -75, 180, 85.

# CRS("+init=epsg:3857")

setwd("theobald")

builtup <- raster("gHMv1_1000m_2017_static_bu.tif")
energy <-raster('gHMv1_1000m_2017_static_en.tif')
agriharv <-raster('gHMv1_1000m_2017_static_ag.tif')
trans <- raster('gHMv1_1000m_2017_static_tr.tif')
pollution <- raster('gHMv1_1000m_2017_static_in.tif')

# Check 

builtup
energy
agriharv
trans
pollution

#
setwd(here())
setwd('data')
setwd('region')

#-------------------------------------------------------------------

downr1 <- resample(builtup, r)
downrc <- mask(downr1, target)
downrcc <- crop(downrc, tr)
writeRaster(downrcc, filename = 'builtup.tif', format="GTiff", overwrite=TRUE)
rm(downr, downrc, downrcc)

downr <- resample(energy, r)
downrc <-mask(downr, target)
downrcc <- crop(downrc, tr)
writeRaster(downrcc, filename = 'energy.tif', format="GTiff", overwrite=FALSE)
rm(downr, downrc, downrcc)

downr <- resample(agriharv, r)
downrc <-mask(downr, target)
downrcc <- crop(downrc, tr)
writeRaster(downrcc, filename = 'agriharv.tif', format="GTiff", overwrite=FALSE)
rm(downr, downrc, downrcc)

downr <- resample(trans, r)
downrc <-mask(downr, target)
downrcc <- crop(downrc, tr)
writeRaster(downrcc, filename = 'trans.tif', format="GTiff", overwrite=FALSE)
rm(downr, downrc, downrcc)

downr <- resample(pollution, r)
downrc <-mask(downr, target)
downrcc <- crop(downrc, tr)
writeRaster(downrcc, filename = 'pollution.tif', format="GTiff", overwrite=FALSE)

#-------------------------------------------------------------