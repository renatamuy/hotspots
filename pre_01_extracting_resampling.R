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

# Define palette -------------------------------
pal <- scico(255, palette = 'lajolla')

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

# Open covariates

setwd('frag')
ed <- brick("Average_Edge_distance.nc")
res(ed$X2015.01.01) #0.1
crs(ed$X2015.01.01)

eds <- stack(ed)

ed_2010 <- eds$X2010.01.01
res(eds)

downr <- resample(ed_2010, r)
downrc <-mask(downr, target)
downrcc <- crop(downrc, tr)

tr[is.na(tr)]<- 0
trc <-mask(tr, target)
trcc <- crop(trc, tr)

#- richness

rc <- mask(r, target)
rcc <- crop(rc, tr)
rccbin <- rcc
rccbin[values(rccbin) > 0] <- 1


# Plot densities for bat versus non-bat points for every covariates

# ------------------- Add landscape division
setwd('frag')

ld <- brick("Landscape_division.nc")
res(ld$X2015.01.01) #0.1
crs(ed$X2015.01.01)

lds <- stack(ld)

ld_2010 <- lds$X2010.01.01

downrl <- resample(ld_2010, r)
downrcl <-mask(downrl, target)
downrccl <- crop(downrcl, tr)

# Plot densities for bat versus non-bat points for every covariates

setwd(here())
setwd('data')
list.files()
p <- raster('ppp_2020_1km_Aggregated.tif')
p

pr <- resample(p, r)
prc <-mask(pr, target)
prcc <- crop(prc, tr)

# ------------------------------------------------------------------------------

# Travel time - walk

setwd(here())
setwd('data')
list.files()

w <- raster('2020_walking_only_travel_time_to_healthcare.geotiff')
wr <- resample(w, r)
wrc <-mask(wr, target)
wrcc <- crop(wrc, tr)

# Plot travel time to healthcare - walk-only
setwd(here())
wrcc_h <- values(wrcc)/60
# Travel time - motorized

setwd(here())
setwd('data')

m <- raster('2020_motorized_travel_time_to_healthcare.geotiff')
mr <- resample(m, r)
mrc <-mask(mr, target)
mrcc <- crop(mrc, tr)

#  Estimated travel time (minutes) to nearest healthcare facility - motorized
mrcc_h <- values(mrcc)/60

# Intermediate hosts -----------------------------------------------------------
# Open AW data from Gilbert's paper for 2010
setwd(here())
setwd('data/livestock')

lv <- stack(list.files(pattern='.tif$'))

lvr <- resample(lv, r)
lvrc <-mask(lvr, target)
lvrcc <- crop(lvrc, tr)

# Sanchez et al. 2022 raster
setwd(here())
setwd('data')

s <- raster('AOH_heatmap.tif')
plot(s)
sr <- resample(s, r)

src <-mask(sr, target)

srcc <- crop(src, tr)

srccpos <- srcc

#- matching background

srccpos[ is.na(srccpos)] <- 0

plot(srccpos)

#- second mask to match region
secmask <-mask(srccpos, target)

hosts_sanchez <-crop(secmask, tr)

plot(hosts_sanchez)

setwd(here())

#-------------------------------------------------------------------------------
# Exporting rasters 

setwd(here())
setwd('data')
dir.create('region')
setwd('region')

trcc # random points
#writeRaster(trcc, filename = 'random.tif', format="GTiff", overwrite=FALSE)

# Export covariates
rcc # host number Muylaert
hosts_sanchez # host number Sanchez
rccbin # host presence
downrcc # edge distance
downrccl #downrccl is forest division index from Jung
prcc # pop size resampled
wrcc # travel time to health care walk only MINUTES
mrcc  # travel time to health care motorized MINUTES
lvrcc # livestock

# Renaming

#writeRaster(lvrcc, filename="ihosts_gilbert.tif", options="INTERLEAVE=BAND", format="GTiff", overwrite=FALSE)

writeRaster(lvrcc$Buffalo, filename="buffalo_gilbert.tif", format="GTiff", overwrite=FALSE)

writeRaster(lvrcc$Chicken, filename="chicken_gilbert.tif", format="GTiff", overwrite=FALSE)

writeRaster(lvrcc$Cattle, filename="cattle_gilbert.tif", format="GTiff", overwrite=FALSE)

writeRaster(lvrcc$Goat, filename="goat_gilbert.tif", format="GTiff", overwrite=FALSE)

writeRaster(lvrcc$Horse, filename="horse_gilbert.tif", format="GTiff", overwrite=FALSE)

writeRaster(lvrcc$Pig, filename="pig_gilbert.tif", format="GTiff", overwrite=FALSE)

writeRaster(lvrcc$Sheep, filename="sheep_gilbert.tif", format="GTiff", overwrite=FALSE)

#--

writeRaster(rcc, filename = 'hosts_muylaert.tif', format="GTiff", overwrite=FALSE)

writeRaster(rccbin, filename = 'hosts_bin_muylaert.tif', format="GTiff", overwrite=FALSE)

writeRaster(hosts_sanchez, filename = 'hosts_sanchez.tif', format="GTiff", overwrite=FALSE)

writeRaster(downrcc, filename = 'edge_distance_jung.tif', format="GTiff", overwrite=FALSE)

writeRaster(downrccl, filename = 'forest_division_jung.tif', format="GTiff", overwrite=FALSE)

writeRaster(prcc, filename = 'pop_2020_worldpop.tif', format="GTiff", overwrite=FALSE)

writeRaster(wrcc, filename = 'walk_travel_time_weiss.tif', format="GTiff", overwrite=FALSE)

writeRaster(mrcc, filename = 'motor_travel_time_weiss.tif', format="GTiff", overwrite=FALSE)

#-------------------------------------------------------------------------------