# Wildlife layers
library(rnaturalearth)
library(sp)
library(scico)
require(raster)
# Mammal species IUCN

library(here())

setwd(here())

getwd()

setwd('data/IUCN MAMMALS 2021')

m <- raster('Mammals_2021_Richness_mode_wgs84.tif')

m

setwd(here())

setwd('data/region')

list.files()

ref <- raster('hosts_muylaert.tif')

f <- m

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

tr <- raster(target, res=res(ref), crs= crs(ref))

fr <- resample(f, ref)

plot(fr)

frc <-mask(fr, target)
frcc <- crop(frc, tr)
plot(is.na(frcc))

frcc[ is.na(frcc)] <- 0

plot(frcc)


#- second mask to match region

secmask <-mask(frcc, target)

forint <-crop(secmask, tr)

plot(forint)

plot(values(forint), values(ref))

s <- stack(forint, ref)
test <- na.omit(data.frame(rasterToPoints( s)))

cor(test$Mammals_2021_Richness_mode_wgs84, test$hosts_muylaert)

setwd(here())
setwd('data/region/')

writeRaster(forint, filename = 'mammals_iucn_mode.tif', format="GTiff", overwrite=FALSE)
