# Read cronin data
# 284 high-priority conservation and health trade risk

source('00_packages.R')

t <- read.csv('D://OneDrive - Massey University//animal_trade_cronin//csp212818-sup-0002-datas2.csv')

nrow(t)

# Read IUCN data

i <- sf::st_read(dsn = 'D://OneDrive - Massey University//data_0.shp',
             layer = 'data_0')

table(t$Species %in% i$BINOMIAL)

t$Species[t$Species %in% i$BINOMIAL]

t$Species[!t$Species %in% i$BINOMIAL] 

# "Allactaga euphratica" is not in our study region

i %>% left_join(t, by = c('BINOMIAL' = 'Species')) %>% 
  filter(BINOMIAL %in% t$Species[t$Species %in% i$BINOMIAL]) -> ti

#

setwd(here())

setwd('data/region')

list.files()

ref <- raster('D://OneDrive - Massey University//bat_non_bat//prep_mammals_minus_bats//mammals_iucn_mode.tif')

ras_dom <- raster::raster(xmn=68.25, xmx= 141.0, ymn=-10.25, ymx=53.5,
                          crs="+proj=longlat +datum=WGS84 +no_defs ",
                          resolution=res(ref), vals=0)

tir <- fasterize::fasterize(ti, ras_dom, field = NULL, fun = 'count')

plot(tir)

max(na.omit(values(tir)))

plot(ref)

##

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

fr <- resample(tir, ref)

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

plot(values(forint), values(ref))

s <- stack(forint, ref)

test <- na.omit(data.frame(rasterToPoints( s)))

###### Export mammal layer minus the bats

setwd(here())

setwd('region/')

names(forint) <- 'traded_mammals'

summary(forint)

bathostsm <- raster('hosts_muylaert.tif')

bathostsz <- raster('hosts_sanchez.tif')

bathostsmean <- mean(bathostsz, bathostsm )



maskb <-mask(bathostsmean, target)
maskbc <- crop(maskb, tr)
maskbc[is.na(maskbc)] <- 0

secmaskb <- mask(maskbc, target)
batsmean <- crop(secmaskb, tr)


par(mfrow=c(1,3))

plot(ref, main='Mammals minus bat hosts')
plot(batsmean, main='Known bat hosts')
plot(forint, main= 'High-priority traded mammals')


cor(na.omit(values(forint)), na.omit(values(  batsmean)) )


writeRaster(forint, filename = 'traded_mammals.tif', format="GTiff", overwrite=TRUE)

# Known bat hosts comparison

known_hosts <- c('Aselliscus stoliczkanus',
                 
                 'Hipposideros armiger',
                 
                 'Hipposideros galeritus',
                 
                 'Hipposideros larvatus',
                 
                 'Hipposideros pomona',  # (gentilis)
               
                 'Hipposideros pratti',
                 
                 'Hipposideros ruber',
                 
                 'Miniopterus schreibersii',
                 
                 'Chaerephon plicatus',
                 
                 'Tadarida teniotis',
                 
                 'Rhinolophus acuminatus',
                 
                 'Rhinolophus affinis',
                 
                 'Rhinolophus blasii',
                 
                 'Rhinolophus blythi',
                 
                 'Rhinolophus cornutus',
                 
                 'Rhinolophus creaghi',
                 
                 'Rhinolophus euryale',
                 
                 'Rhinolophus ferrumequinum',
                 
                 'Rhinolophus hipposideros',
                 
                 'Rhinolophus luctus',
                 
                 'Rhinolophus macrotis',
                 
                 'Rhinolophus malayanus',
                 
                 'Rhinolophus marshalli',
                 
                 'Rhinolophus mehelyi',
                 
                 'Rhinolophus monoceros',
                 
                 'Rhinolophus pearsonii',
                 
                 'Rhinolophus rex',
                 
                 'Rhinolophus shameli',
                 
                 'Rhinolophus siamensis',
                 
                 'Rhinolophus sinicus',
                 
                 'Rhinolophus stheno',
                 
                 'Rhinolophus thomasi',
                 
                 'Nyctalus leisleri',
                 
                 'Plecotus auritus')

t$Species[t$Species %in% known_hosts]

known_hosts[ known_hosts %in% t$Species]

length(known_hosts)

getwd()
setwd(here())
dir.create('trade_check')
setwd('trade_check')

write.csv(data.frame(traded_mammal_high_priority = t$Species),  'high_priority_traded_mammals.csv', row.names = FALSE)
write.csv(data.frame(known_sarbecovirus_bat_host = known_hosts),  'known_hosts_muylaert.csv', row.names = FALSE)

#-----------------------------------

