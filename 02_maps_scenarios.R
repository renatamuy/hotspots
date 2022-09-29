# -- scenario with no intermediate hosts
# -- scenario with intermediate hosts

require(rnaturalearth)
require(tidyverse)
require(stringr)
require(reshape2)
require(here)

setwd(here())
setwd('results')
dfgplot <- read.csv('gstar.csv' )

head(dfgplot)

setwd(here())
setwd('results')

#----> identifies risk areas for change, surveillance sites in wildlife/communities
#Countries
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


want_landscape <- c('builtup', 'energy',  'agriharv', 
                    'forest_integrity_grantham',
                    'hewson_forest_transition_potential'                  )

want_bat <- c('lincomb_hosts')

want_expo_spread <- c('pop_2020_worldpop')

want_risk1 <- c(want_landscape,want_bat,want_expo_spread)

# hotspot convergence in scenario 1

#Melt
mm <- melt(dfgplot[c('x','y',want_landscape,'lincomb_hosts','pop_2020_worldpop')], id.vars=c('x','y') )

b <- mm %>% filter(variable == 'builtup' )

head(b)


#pal <- scico::scico(length(unique(dfgplot$n_hotspots_landscape)), palette = 'lajolla')
head(dfgplot)

# Same weights for all
setdiff(want_risk1, colnames(dfgplot))

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk1 = rowSums(dfgplot[want_risk1]  > 1.9546, na.rm = TRUE))

go <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk1")))

cats <- length(unique(dfgplot$n_hotspots_risk1))
pal <- scico::scico(  length(unique(dfgplot$n_hotspots_risk1)) , palette = "vik")
pal <-rev(rainbow(length(unique(dfgplot$n_hotspots_risk1)), alpha=0.66))
pal <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))

b <- c(0,2,4, 6, 8,10 )

p1 <- ggplot()+
  geom_tile(data = go, aes(y=y, x=x, fill = n_hotspots_risk1))+ theme_bw() +
  scale_fill_gradientn(colours = pal, breaks = b) +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  #scale_fill_gradientn(colours=c("lightskyblue", "darkmagenta", "darkorange1"), breaks = b, labels=format(b)) +
  #scale_fill_gradientn(colours= c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", 
       title = "Hotspot convergence areas", 
       subtitle = 'Landscape change and known bat hosts'  ) 

p1

ggsave(
  'hotspots.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

# With royal blue pal

p1r <- ggplot()+
  geom_tile(data = go, aes( y=y, x=x, fill = n_hotspots_risk1))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours= c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", 
       title = "Hotspot convergence areas", 
       subtitle = 'Landscape change and known bat hosts'  ) 

p1r

ggsave(
  'hotspots_royal.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

# Scenario with potential intermediate hosts (and surveillance sites)
#----> identifies risk areas for change, surveillance sites in livestock/communities

want_risk2 <- c(want_risk1, 'pig_gilbert', 'cattle_gilbert','mammals_iucn_mode' )

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk2 = rowSums(dfgplot[want_risk2]  > 1.9546, na.rm = TRUE))
length(unique(dfgplot$n_hotspots_risk2))
unique(dfgplot$n_hotspots_risk2)

cats <- length(unique(dfgplot$n_hotspots_risk2))
pal2 <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))
b <- c(0,2,4,6,8,10 )

go2 <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk2")))

# Spectral 
p2 <- ggplot()+
  geom_tile(data = go2, aes( y=y, x=x, fill = n_hotspots_risk2))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal2, breaks = b) +
  #scale_fill_gradientn(colours = c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Landscape change, known bat hosts and \n potential  secondary hosts'  ) 

p2

ggsave(
  'hotspots_sec.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)


p2r <- ggplot()+
  geom_tile(data = go2, aes( y=y, x=x, fill = n_hotspots_risk2))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  #scale_fill_gradientn(colours = pal2, breaks = b) +
  scale_fill_gradientn(colours = c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Equal weights considering potential secondary hosts'  ) 
p2r

ggsave(
  'hotspots_sec_royal.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

# Difference map:
go2$dif = go2$n_hotspots_risk2 - go$n_hotspots_risk1 

hist(go2$dif)

p2dif <- ggplot()+
  geom_tile(data = go2, aes( y=y, x=x, fill = dif))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal2) + #, breaks = b
  #scale_fill_gradientn(colours = c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map (secondary - primary scenario)", 
       subtitle = ''  ) 

p2dif

#
ggsave(
  'hotspots_sec_minus_first.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

# Scenario 2 + spread 

want_risk3 <- c(want_risk2, 'motor_travel_time_weiss' )

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk3 = rowSums(dfgplot[want_risk3]  > 1.9546, na.rm = TRUE))

go3 <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk3")))

cats <- length(unique(dfgplot$n_hotspots_risk3))
pal3 <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))

p3 <- ggplot(data = go3, aes( y=y, x=x, fill = n_hotspots_risk3))+
  geom_tile()+ theme_bw() +
  scale_fill_gradientn(colours = pal, breaks = b) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Equal weights considering all factors + spread'  ) 
p3 

ggsave(
  'hotspots_spread.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)


p3r <- ggplot(data = go3, aes( y=y, x=x, fill = n_hotspots_risk3))+
  geom_tile()+ theme_bw() +
  scale_fill_gradientn(colours= c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Equal weights considering all factors + spread'  ) 

p3r

ggsave(
  'hotspots_spread_royal.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)
                       
library(gridExtra)
library(grid)

ggsave(
  'hotspots_all.png',
  plot = grid.arrange(p1, p2,p3, nrow = 1),
  dpi = 400,
  width = 15,
  height = 6,
  limitsize = TRUE)

ggsave(
  'hotspots_all_royal.png',
  plot = grid.arrange(p1r, p2r,p3r, nrow = 1),
  dpi = 400,
  width = 15,
  height = 6,
  limitsize = TRUE)

# Bivariate map
require(bivariatemaps)
library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)

# Check location
setwd('C://Users//Renata//OneDrive - Massey University//bat_non_bat//data//region//')

raster_access <-raster('motor_travel_time_weiss.tif')

ras_dom <-raster::raster(xmn=68.25, xmx= 141.0, ymn=-10.25, ymx=53.5,
                         crs="+proj=longlat +datum=WGS84 +no_defs ",
                         resolution=res(raster_access), vals=NA)

db <- dfgplot[c('x', 'y', 'n_hotspots_risk2')]
db$risk2 <- (db$n_hotspots_risk2-min(db$n_hotspots_risk2))/
  (max(db$n_hotspots_risk2)-min(db$n_hotspots_risk2))

                                                           
hist(db$risk2 )

coordinates(db) <- ~ x + y 

crs(db) <- "+proj=longlat +datum=WGS84 +no_defs "

# Scenario 2

raster2 <- rasterize(db, ras_dom, 
                     field = c("risk2"),
                     update = TRUE)

col.matrix<-colmat(nquantiles=3,
                   upperleft="blue", 
                   upperright="red", 
                   bottomleft="yellow",
                   bottomright="pink",
                   xlab="Access to healthcare", 
                   ylab="Hazard: Scenario 2")

bivmap<-bivariate.map(raster1,raster2, colormatrix=col.matrix,
                      nquantiles=3)

plot(bivmap,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix))

map(interior=T,add=T)

#----------------------------------------------------------------------