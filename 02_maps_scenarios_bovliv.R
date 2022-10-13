# -- scenario with no intermediate hosts
# -- scenario with intermediate hosts

require(stringr)
require(rnaturalearth)
require(tidyverse)
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

# Jaccard difference map
load('jaccard.RData')

myjacs[is.nan(myjacs)] <- 0

go2$jac <- myjacs

p2jac <- ggplot()+
  geom_tile(data = go2, aes( y=y, x=x, fill = jac))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal2) + #, breaks = b
  #scale_fill_gradientn(colours = c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Jaccard dissimilarity between scenarios", 
       subtitle = ''  ) 

p2jac

ggsave(
  'hotspots_jaccard.png',
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
setwd('../region')

raster_access <-raster('motor_travel_time_weiss.tif')

mini <-  min(na.omit(values(raster_access)))
maxi <-  max(na.omit(values(raster_access)))
values(raster_access) <-( values(raster_access) - mini)/ (maxi-mini)

ras_dom <-raster::raster(xmn=68.25, xmx= 141.0, ymn=-10.25, ymx=53.5,
                         crs="+proj=longlat +datum=WGS84 +no_defs ",
                         resolution=res(raster_access), vals=NA)

db <- dfgplot[c('x', 'y', 'n_hotspots_risk1','n_hotspots_risk2')]

db$risk2 <- (db$n_hotspots_risk2-min(db$n_hotspots_risk2))/
  (max(db$n_hotspots_risk2)-min(db$n_hotspots_risk2))

hist(db$risk2)
quantile(db$risk2)
quantile(raster_access )

coordinates(db) <- ~ x + y 

crs(db) <- "+proj=longlat +datum=WGS84 +no_defs "

# Scenario 2

raster2 <- rasterize(db, ras_dom, 
                     field = c("risk2"),
                     update = TRUE)

#https://encycolorpedia.com/b22222

col.matrix <- bivariatemaps::colmat(nquantiles=3,
                   upperleft= 'khaki',#'khaki', #, ##4DDDDD
                   upperright= '#ff0000',#"#B22222",#"violetred4", 
                   bottomleft= 'azure2',#'#d8d8d8',#'#1B9E77',  #"azure2",
                   bottomright= '#141414',# "#141414",
                   xlab="Time to reach healthcare", 
                   ylab="Hazard: Scenario 2")

#Green zones are graph nodes with the strongest influence on the total risk of pandemic spread

bivmap2 <-bivariate.map(raster_access,raster2, 
colormatrix=col.matrix, nquantiles=3)

plot(bivmap2,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix))

map(interior=T, add=T)

# Risk scenario 1 (just bat hosts) -----------------------------------

db$risk1 <- (db$n_hotspots_risk1-min(db$n_hotspots_risk1))/
  (max(db$n_hotspots_risk1)-min(db$n_hotspots_risk1))

hist(db$risk1)
quantile(db$risk1)
quantile(raster_access )

raster1 <- rasterize(db, ras_dom, 
                     field = c("risk1"),
                     update = TRUE)

# Change labels!
col.matrix <- bivariatemaps::colmat(nquantiles=3,
                                    upperleft= 'khaki',#'khaki', #, ##4DDDDD
                                    upperright= '#ff0000',#"#B22222",#"violetred4", 
                                    bottomleft= 'azure2',#'#d8d8d8',#'#1B9E77',  #"azure2",
                                    bottomright= '#141414',# "#141414",
                                    xlab="Time to reach healthcare", 
                                    ylab="Hazard: Scenario 1")

#Green zones are graph nodes with the strongest influence on the total risk of pandemic spread

bivmap1 <- bivariate.map(raster_access, raster1, 
                       colormatrix=col.matrix, nquantiles=3)

plot(bivmap1,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix))

map(interior=T, add=T)

# Difference in risk

plot(raster2-raster1)

bivmapdif <-bivariate.map(raster_access, raster2-raster1, 
                       colormatrix=col.matrix, nquantiles=3)

plot(bivmapdif,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix))

#
setwd(here())
setwd('results/')

dfg <- read.csv("gstar.csv")

d <- read.csv('prepdf.csv')

colnames(dfg)
colnames(d)

# Minutes to hours

length(d$motor_travel_time_weiss/60)
tt <- d$motor_travel_time_weiss/60
dfg$tt <- tt

tcont <- (d$motor_travel_time_weiss-min(d$motor_travel_time_weiss))/
  (max(d$motor_travel_time_weiss)-min(d$motor_travel_time_weiss))
popcont <- (d$pop_2020_worldpop-min(d$pop_2020_worldpop))/
  (max(d$pop_2020_worldpop)-min(d$pop_2020_worldpop))

plot(bivmap1, col = as.vector(col.matrix))
unique(values(bivmap1))

as.vector(col.matrix)

bivmap1m <- na.omit(values(bivmap1) )

# khaki is #F0E68C

col.matrix[4] # 3, tricky

col.matrix[8] # 3, tricky

col.matrix[16] # red ->  12 #FF0000 

col.matrix[14]  # black -> 10 #141414 

listacor <- c()
for ( i in unique(bivmap1m) ) {
print(i+4)
coratual <- as.vector(col.matrix)[i+4]
listacor <- c(listacor, coratual)
}
unique(bivmap1m)

col.matrix
listacor
# Colors assigned from bottom to up (row), left to right (col)
# Last color in legend is red =  "#FF0000"
 
cores1 <- ifelse(bivmap1m == 11, "#890A0A",
                 ifelse(bivmap1m == 8, "#F77346",
                        ifelse(bivmap1m == 7, "#B87963" ,
                               ifelse(bivmap1m == 10, "#141414",
                                      ifelse(bivmap1m == 1, "#E0EEEE",
                                             ifelse(bivmap1m == 9, "#798181",
                                                    ifelse(bivmap1m == 2, "#E8EABD",
                                                           ifelse(bivmap1m == 3,"#F0E68C",
                                                                  ifelse(bivmap1m == 12,"#FF0000", NA)))))))))
  
   


table(cores1)

d$cores1 <- cores1
#5344  #E8EABD
#d1 <- d %>%  filter( cores1 != "#141414") 

plot(log10(d$motor_travel_time_weiss), 
     log10(d$pop_2020_worldpop), 
     col = alpha(d$cores1, 0.4), 
     pch=16, 
     cex=0.6,
     ylab='Human population (log)',
     xlab='Time to reach healthcare (log)')

d$travel_log <- log10(d$motor_travel_time_weiss)
d$poplog <-log10(d$pop_2020_worldpop)

plot_quantiles_scenario1 <- ggplot(d, 
                                   aes(x = travel_log,
   y = poplog) )+ 
  geom_point(color=d$cores1, alpha=0.2)+
  facet_wrap(~d$cores1 )+
  #scale_color_manual(aes(color=d$cores1)) +
  labs(x='Time to reach healthcare (log10)',
       y='Human population (log10)')+
  theme_minimal()  +
  theme(strip.text.x = element_blank())
  #ylim(-4, 3.5) # weird points only in ggplot

plot_quantiles_scenario1
                                    

# Cores scenario 2 ----
bivmap2m <- na.omit(values(bivmap2) )
unique(bivmap2m)

cores2 <- ifelse(bivmap2m == 11, "#890A0A",
                 ifelse(bivmap2m == 8, "#F77346",
                        ifelse(bivmap2m == 7, "#B87963" ,
                               ifelse(bivmap2m == 10, "#141414",
                                      ifelse(bivmap2m == 1, "#E0EEEE",
                                             ifelse(bivmap2m == 9, "#798181",
                                                    ifelse(bivmap2m == 2, "#E8EABD",
                                                           ifelse(bivmap2m == 3,"#F0E68C",
                                                                  ifelse(bivmap2m == 12,"#FF0000", NA)))))))))




d$cores2 <- cores2

plot(log10(d$motor_travel_time_weiss), 
     log10(d$pop_2020_worldpop), 
     col = alpha(d$cores2, 0.4), 
     pch=16, 
     cex=0.6,
     ylab='Human population (log)',
     xlab='Time to reach healthcare (log)')

d$travel_log <- log10(d$motor_travel_time_weiss)
d$poplog <-log10(d$pop_2020_worldpop)

plot_quantiles_scenario2 <- ggplot(d, 
                                   aes(x = travel_log,
                                       y = poplog) )+ 
  geom_point(color=d$cores2, alpha=0.2)+
  facet_wrap(~d$cores2)+
  labs(x='Time to reach healthcare (log10)',
       y='Human population (log10)')+
  theme_minimal()  +
  theme(strip.text.x = element_blank())

plot_quantiles_scenario2


#----------------------------------------------------------------------