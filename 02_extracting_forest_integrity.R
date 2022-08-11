# Forest integrity index resample - SLOW

library(here())
require(raster)
library(rnaturalearth)
library(sp)
library(scico)
setwd(here())

setwd('data/region')

list.files()

ref <- raster('hosts_muylaert.tif')

setwd('../')
getwd()

f <- raster('flii_earth.tif')

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

frc <-mask(fr, target)
frcc <- crop(frc, tr)
plot(is.na(frcc))

frcc <- frcc/1000

frcc[ is.na(frcc)] <- 0

plot(frcc)

#- second mask to match region

secmask <-mask(frcc, target)

forint <-crop(secmask, tr)

plot(forint)

plot(fr)

setwd(here())
setwd('data/region/')

writeRaster(forint, filename = 'forest_integrity_grantham.tif', format="GTiff", overwrite=FALSE)


#IMPORTANT: Data has been multiplied by 1000 to store in Integer format; for proper values (Range 0-10), divide by 1000.

#WGS_1984/300m

#NoData:-9999

#The Forest Landscape Integrity Index integrates data on observed and inferred forest pressures and lost forest connectivity to generate the first globally-consistent, continuous index of forest integrity as determined by degree of anthropogenic modification.

#This is the first measure of ecological integrity for all the world’s forests. The study brought together 47 forest experts from across the world to apply recent developments in cloud computing and large new datasets.

#The Google Earth Engine map shown here allows the user to zoom in and out of different locations across the world and overlay other related datasets. There is the option to choose either the continuous index (which provides the most detailed information) or a simpler classified one that divides the world’s forests into illustrative categories of low, medium and high health.

#The Forest Landscape Integrity Index shows that globally, only 17.4 million km2 of forest (40%) can be considered having high integrity and only 27% of this area is found in nationally-designated protected areas. Of all the world’s forests found within protected areas, only 56% can be considered having high integrity.
                                                                                           
                                                                                         #  The results of this study are fundamental to current discussions underway regarding the Convention on Biological Diversity, since the current draft of the post-2020 Global Biodiversity Framework proposes both a goal and an action target relating to ecosystem integrity and there is active discussion amongst Parties about how best this can be quantified and monitored. The results are also highly relevant to the delivery of the Paris Agreement under the UN Framework Convention on Climate Change since special consideration needs to be given to forest carbon reservoirs and sinks with high ecological integrity, both within Nationally Determined Contributions and in international finance mechanisms such as the Green Climate Fund.
                                                                                           
                                                                                          # Article: https://www.nature.com/articles/s41467-020-19493-3
                                                                                           
                                                                                         #  Data: https://forms.gle/mKu5aFZLy4ByBZUJ7
                                                                                           
                                                                                         #  Ancillary data sources: World Database of Protected Areas ; Intact Forest Landscapes ; Primary Humid Tropical Forests