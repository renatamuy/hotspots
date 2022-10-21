# IUCN minus bats

source('00_packages.R')

# Getting shapefile of hosts

setwd('D://OneDrive - Massey University//sars_cov_risk-main//data')

sh <- shapefile('SARSrCoVhostShapefiles.shp')

head(sh) 

plot(sh)

# Open mammmals IUCN mode - reference with all mammals

setwd(here())

setwd('data/region')

list.files()

ref <- raster('D://OneDrive - Massey University//bat_non_bat//prep_mammals_minus_bats//mammals_iucn_mode.tif')

ras_dom <- raster::raster(xmn=68.25, xmx= 141.0, ymn=-10.25, ymx=53.5,
                        crs="+proj=longlat +datum=WGS84 +no_defs ",
                        resolution=res(ref), vals=0)


sh$presence <- rep(1, nrow(sh))

unique(sh$presence)

mmb <- ref

for(b in unique(sh$binomial) ){
 print(b) 

temp = subset(sh, binomial== b)

tempr <- rasterize(temp, ras_dom, field = c("presence"), update = TRUE) 

# mammal minus bat 

mmb <- mmb - tempr

rm(tempr)
}

####

f <- mmb

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

frcc[is.na(frcc)] <- 0

frcc[which(values(frcc < 0 ))] <- 0

#- second mask to match region

secmask <- mask(frcc, target)

forint <- crop(secmask, tr)

plot(forint)

par(mfrow=c(1,3))

plot(sh, main = 'host IUCN polygons')

plot(ref, main = 'all mammals')

plot(forint, main = 'mammals minus bats')


plot(values(forint), values(ref))

s <- stack(forint, ref)

test <- na.omit(data.frame(rasterToPoints( s)))

cor(test$layer, test$mammals_iucn_mode)

###### Export mammal layer minus the bats

setwd(here())

setwd('region/')

names(forint) <- 'mammals_minus_bat_hosts'

writeRaster(forint, filename = 'mammals_minus_bat_hosts.tif', format="GTiff", overwrite=TRUE)

#---------------------------------------------------------------------------