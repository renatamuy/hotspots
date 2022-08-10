# Workflow for characterizing bat points and background points 
# Extracting data for study region matchin res and extention
library(here)
require(raster)
library(rnaturalearth)
library(sp)
library(scico)
#scico_palette_show()

setwd(here())

# Read estimated bat richness layer

setwd('data')
r <- raster('richnessm.tif')

# Define working extent 

# Define palette -------------------------------
#pal <- scico(255, palette = 'berlin')
#pal <- scico(255, palette = 'bilbao')
#pal <- scico(255, palette = 'hawaii')

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

# Plot 1
setwd(here())

jpeg(filename='Edge_distance_plots.jpg', width = 34, height = 20, units = "cm", res=300)
par(mfrow=c(2,4))

plot(downrcc, main=  'Edge distance (km)')
plot(rcc, main = 'SARSr-Cov bat Hosts (estimated species number)', col = pal)  #rev(heat.colors(16)))
plot(trcc, main = 'Random points')
plot(rccbin, main = 'SARSr-Cov bat Hosts (presence)' ) #, col =  rev(heat.colors(16)))

hist(downrcc, main= '', xlab= 'Edge distance (km)', cex=2)
plot(values(rcc) ~values(downrcc), xlab= 'Edge distance (km)')
plot(values(trcc) ~values(downrcc), xlab= 'Edge distance (km)')
plot(values(rccbin) ~values(downrcc), xlab= 'Edge distance (km)')
dev.off()

# Plot densities for bat versus non-bat points for every covariates


#----


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

# Plot 1
setwd(here())

jpeg(filename='Forest_division_plots.jpg', width = 34, height = 20, units = "cm", res=300)
par(mfrow=c(2,4))

plot(downrccl, main=  'Forest division index')
plot(rcc, main = 'SARSr-Cov bat Hosts (estimated species number)', col = pal)  #rev(heat.colors(16)))
plot(trcc, main = 'Random points')
plot(rccbin, main = 'SARSr-Cov bat Hosts (presence)' ) #, col =  rev(heat.colors(16)))

hist(downrccl, main= '', xlab= 'Forest division index', cex=2)
plot(values(rcc) ~values(downrccl), xlab= 'Forest division index')
plot(values(trcc) ~values(downrccl), xlab= 'Forest division index')
plot(values(rccbin) ~values(downrccl), xlab= 'Forest division index')
dev.off()

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
# Plot population 
setwd(here())
jpeg(filename='Pop_plots.jpg', width = 34, height = 20, units = "cm", res=300 )

par(mfrow=c(2,4))

plot(prcc, main=  'Population size (resampled)')
plot(rcc, main = 'SARSr-Cov bat Hosts (estimated species number)', col =  pal ) #rev(heat.colors(16)))
plot(trcc, main = 'Random points')
plot(rccbin, main = 'SARSr-Cov bat Hosts (presence)') #, col =  rev(heat.colors(16)))

hist(prcc, main= '', xlab= 'Population size (resampled)')
plot(values(rcc) ~values(prcc), xlab= 'Population size (resampled)')
plot(values(trcc) ~values(prcc), xlab= 'Population size (resampled)')
plot(values(rccbin) ~values(prcc), xlab= 'Population size (resampled)')

dev.off()

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
jpeg(filename='Travel_healthcare_walk_plots.jpg', width = 34, height = 20, units = "cm", res=300 )
par(mfrow=c(2,4))

wrcc_h <- values(wrcc)/60
plot(wrcc/60, main=  'Travel time to healthcare (walk-only, hours)', col = rev(bpy.colors(250)))
plot(rcc, main = 'SARSr-Cov bat Hosts (estimated species number)', col = pal)  #rev(heat.colors(16)))
plot(trcc, main = 'Random points')
plot(rccbin, main = 'SARSr-Cov bat Hosts (presence)')#, col = rev(heat.colors(16)))

hist(wrcc_h, main= '', xlab=  'Travel time to healthcare (walk-only, hours)')
plot(values(rcc) ~ wrcc_h, xlab= 'Travel time to healthcare (walk-only, hours)')
plot(values(trcc) ~ wrcc_h , xlab= 'Travel time to healthcare (walk-only, hours)')
plot(values(rccbin) ~ wrcc_h, xlab= 'Travel time to healthcare (walk-only, hours)')

dev.off()

# Travel time - motorized

setwd(here())
setwd('data')

m <- raster('2020_motorized_travel_time_to_healthcare.geotiff')
mr <- resample(m, r)
mrc <-mask(mr, target)
mrcc <- crop(mrc, tr)

#  Estimated travel time (minutes) to nearest healthcare facility - motorized
setwd(here())
jpeg(filename='Travel_healthcare_motor_plots.jpg', width = 34, height = 20, units = "cm", res=300)

par(mfrow=c(2,4))
mrcc_h <- values(mrcc)/60

plot(mrcc/60, main=  'Travel time to healthcare (motorized, hours)', col = rev(bpy.colors(250)))
plot(rcc, main = 'SARSr-Cov bat Hosts (estimated species number)', col = pal) #rev(heat.colors(16)))
plot(trcc, main = 'Random points')
plot(rccbin, main = 'SARSr-Cov bat Hosts (presence)')

hist(mrcc_h, main= '', xlab=  'Travel time to healthcare (motorized, hours)')
plot(values(rcc) ~ mrcc_h, xlab= 'Travel time to healthcare (motorized, hours)')
plot(values(trcc) ~ mrcc_h, xlab= 'Travel time to healthcare (motorized, hours)')
plot(values(rccbin) ~ mrcc_h, xlab= 'Travel time to healthcare (motorized, hours)')

dev.off()

# Intermediate hosts -----------------------------------------------------------

# Open AW data from Gilbert's paper for 2010

setwd(here())
setwd('data/livestock')

lv <- stack(list.files(pattern='.tif$'))

lvr <- resample(lv, r)
lvrc <-mask(lvr, target)
lvrcc <- crop(lvrc, tr)

names(lvrcc) <- c('Buffalo', "Chicken", 'Cattle', "Goat", 'Horse', 'Pig', 'Sheep')

setwd(here())

for(i in names(lvrcc)){
  
  jpeg(filename = paste0(i,'_plots.jpg'), width = 34, height = 20, units = "cm", res=300)
  par(mfrow=c(2,4))
  
  plot(lvrcc[[i]], main = i, col = pal )
  plot(rcc, main = 'SARSr-Cov bat Hosts (estimated species number)', col = pal) #rev(heat.colors(16)))
  plot(trcc, main = 'Random points')
  plot(rccbin, main = 'SARSr-Cov bat Hosts (presence)') #col = pal )#rev(heat.colors(16)))
  
  hist(lvrcc[[i]], main= '', xlab=  i)
  plot(values(rcc) ~ values(lvrcc[[i]]), xlab= i)
  plot(values(trcc) ~ values(lvrcc[[i]]), xlab= i)
  plot(values(rccbin) ~ values(lvrcc[[i]]), xlab= i)
  
  dev.off()
  print(i)
}

# Cattle ---------
# Areal-weighted GLW model (6_Ct_2010_Aw.tif)

# Pig ----------
#Areal-weighted GLW model (6_Pg_2010_Aw.tif)

# Buffalo -------
#Areal-weighted GLW model (6_Bf_2010_Aw.tif)

# Chicken
#Areal-weighted GLW model (6_Ch_2010_Aw.tif)

# Goat
#Areal-weighted GLW model (6_Gt_2010_Aw.tif)

# Horse
#Areal-weighted GLW model (6_Gt_2010_Ho.tif)

# Sheep
#Areal-weighted GLW model (6_Sh_2010_Ho.tif)

#-------------------------------------------------------------------------------
# Compare to Sanchez et al. raster
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

jpeg(filename='bat_host_positive.jpg', width = 32, height = 17, units = "cm", res=300)

par(mfrow=c(1,2), oma=c(1,1,1,1))
plot(rccpos, col= pal, main='SARSr-Cov Host species (Muylaert et al.)')#rev(scico(255, palette = 'berlin')))
plot(srccpos, col= pal, main='SARSr-Cov Host species (Sanchez et al.)')

dev.off()

# Compare our bat-human overlap with Sanchez et al.
# Relative bat-human overlap: bat host species richness multiplied by
# human population count. Values were ln(x + 1) transformed and then normalized to a 0-1 scale

#-muylaert
batpop <- rccpos * log(prcc+1)
batpop_pct <- (batpop- min(na.omit(values(batpop))))  / (max(na.omit(values(batpop))) - min(na.omit(values(batpop))) )

#shanchez
batpops <- srccpos  * log(prcc+1)
batpops_pct <- (batpops- min(na.omit(values(batpops))))  / (max(na.omit(values(batpops))) - min(na.omit(values(batpops))) )

setwd(here())

jpeg(filename='bat_host_human_overlap_positive.jpg', width = 33, height = 18, units = "cm", res=300)
par(mfrow=c(1,2), oma=c(1,1,1,1))
plot(batpop_pct, col= pal, main='Bat-human overlap risk (Muylaert et al.)')#rev(scico(255, palette = 'berlin')))
plot(batpops_pct, col= pal, main='Bat-human overlap risk (Sanchez et al.)')#rev(scico(255, palette = 'berlin')))
dev.off()

jpeg(filename='bat_host_human_overlap_hist_positive.jpg', width = 34, height = 18, units = "cm", res=300)
par(mfrow=c(1,2))
hist(batpop_pct, main='', xlab= 'Bat-human overlap (Muylaert et al.)')
hist(batpops_pct,main='', xlab='Bat-human overlap (Sanchez et al.)')
dev.off()


# Correlation values
difna <- na.omit(srccpos) - na.omit(rccpos )

plot(dif, col= rev(scico(255, palette = 'hawaii')) )

plot(difna, col= rev(scico(255, palette = 'hawaii')) )


hist(difna, xlab='Difference', main='')

plot(dif, col= scico(255, palette = 'berlin'), main='Difference (Sanchez et al. - Muylaert et al.)') 

dev.off()

require(spatialEco)

co <- rasterCorrelation(rccpos, srccpos)

co_overlap <- rasterCorrelation(batpop_pct, batpops_pct)

hist(co_overlap, xlab='Correlation between metrics of bat-human overlap', main='')

plot(co, col=scico(255, palette = 'bilbao'))

plot(co_overlap , col=pal)

#-------------------------------------------------------------------------------
# Exporting ready rasters to plot again

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

# Renamed to meaning

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
#-------------------------------------------------------------------------------

