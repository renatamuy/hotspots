# -- scenario with no intermediate hosts
# -- scenario with intermediate hosts
# -- scenario with intermediate hosts and no bats
# Figure 02
# Figure 04

require(stringr)
require(rnaturalearth)
require(tidyverse)
require(reshape2)
require(here)
library(gridExtra)
library(grid)
require(bivariatemaps)
library(classInt)
library(raster)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)

setwd(here())
setwd('results')
dfgplot <- read.csv('gstar.csv' )


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


head(dfgplot)

setwd(here())
setwd('results')

setwd('skater_optimal_cluster_size_19/')

target <- raster::shapefile('s19s.shp')

setwd(here())
setwd('results/scenarios')

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

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk1 = rowSums(dfgplot[want_risk1]  > 1.645, na.rm = TRUE))

go <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk1")))

cats <- length(unique(dfgplot$n_hotspots_risk1))
pal <- scico::scico(  length(unique(dfgplot$n_hotspots_risk1)) , palette = "vik")
pal <-rev(rainbow(length(unique(dfgplot$n_hotspots_risk1)), alpha=0.66))
pal <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))

b <- c(0,2,4, 6, 8,10 )


target@data$x<- coordinates(target)[,1] 
target@data$y<- coordinates(target)[,2] 

positions <- target@data %>%
  group_by(layer) %>% 
  summarise(mx = median(x), my = median(y))

p1 <- ggplot()+
  geom_tile(data = go, aes(y=y, x=x, fill = n_hotspots_risk1))+ theme_bw() +
  scale_fill_gradientn(colours = pal, breaks = b) +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  theme(legend.title=element_blank(), legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", 
       title = "Hotspot convergence areas", 
       subtitle = 'Landscape change and known bat hosts \n                          '  ) 

p1


# Scenario with potential intermediate hosts (and surveillance sites)
#----> identifies risk areas for change, surveillance sites in livestock/communities

want_risk2 <- c(want_risk1, 'pig_gilbert', 'cattle_gilbert','mmb' )

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk2 = rowSums(dfgplot[want_risk2]  > 1.645, na.rm = TRUE))
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
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  scale_fill_gradientn(colours = pal2, breaks = b) +
  #scale_fill_gradientn(colours = c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Landscape change, known bat hosts and \n potential  secondary hosts'  ) 

p2


# bovliv ---------------------

want_risk3 <- c(want_risk1, 'pig_gilbert', 'bovliv','mmb' )

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk3 = rowSums(dfgplot[want_risk3]  > 1.645, na.rm = TRUE))
length(unique(dfgplot$n_hotspots_risk3))

unique(dfgplot$n_hotspots_risk2)

cats <- length(unique(dfgplot$n_hotspots_risk2))
pal3 <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))
b <- c(0,2,4,6,8,10 )

go3 <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk3")))

p3 <- ggplot()+
  geom_tile(data = go3, aes( y=y, x=x, fill = n_hotspots_risk3))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  scale_fill_gradientn(colours = pal2, breaks = b) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Landscape change, known bat hosts and \n potential  secondary hosts (all bovidae)'  ) 

p3

# Difference map:
go2$dif = go2$n_hotspots_risk2 - go$n_hotspots_risk1 

hist(go2$dif)

p2dif <- ggplot()+
  geom_tile(data = go2, aes( y=y, x=x, fill = dif))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  scale_fill_gradientn(colours = pal2) + #, breaks = b
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map", 
       subtitle = ''  ) 

p2dif

ggsave(
  'Figure_02_clusters.png',
  plot = grid.arrange(p1, p2, p2dif, nrow = 1), #last_plot()
  dpi = 400,
  width = 15,
  height = 6,
  limitsize = TRUE)

# Dif 2 and bovliv (risk3) --------------------
head(go3)
go3$difbovliv = go3$n_hotspots_risk3 -  go$n_hotspots_risk1 

hist(go2$dif)
summary(go2$dif)

p2difb <- ggplot()+
  geom_tile(data = go3, aes( y=y, x=x, fill = difbovliv))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  scale_fill_gradientn(colours = pal2) + #, breaks = b
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map (all bovidae)", 
       subtitle = ''  ) 

grid.arrange(p1, p3, p2difb, nrow = 1)

ggsave(
  'Figure_02_bovliv_clusters.png',
  plot = grid.arrange(p1, p3, p2difb, nrow = 1),
  dpi = 400,
  width = 15,
  height = 6,
  limitsize = TRUE)

# Scenario 2 with no bats ----------

#----> identifies risk areas for change, surveillance sites in livestock/communities

want_risk2_nobats <- c('pig_gilbert', 'cattle_gilbert','mmb',
                       "builtup"     ,   
                        "energy",                     
   "agriharv", 
   "forest_integrity_grantham" ,        
  "hewson_forest_transition_potential",
  "pop_2020_worldpop"  )

dfgplot <- dplyr::mutate(dfgplot, 
                         n_hotspots_risk2_nobats = rowSums(dfgplot[want_risk2_nobats]  > 1.645, 
                                                           na.rm = TRUE))
length(unique(dfgplot$n_hotspots_risk2_nobats))

unique(dfgplot$n_hotspots_risk2_nobats)

cats <- length(unique(dfgplot$n_hotspots_risk2_nobats))
pal2 <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))
b <- c(0,2,4,6,8,10 )

go2nb <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk2_nobats")))

# Spectral 
p2nb <- ggplot()+
  geom_tile(data = go2nb, aes( y=y, x=x, fill = n_hotspots_risk2_nobats))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  scale_fill_gradientn(colours = pal2, breaks = b) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Landscape change, \n potential  (non-bat hosts) secondary hosts'  ) 

p2nb

# Difference with nb scenario 2

dfgplot$difnb = dfgplot$n_hotspots_risk2_nobats -  dfgplot$n_hotspots_risk1 

head(go2nb)
head(dfgplot)

summary(go2$dif)

p2difnb <- ggplot()+
  geom_tile(data = dfgplot, aes( y=y, x=x, fill = difnb))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  scale_fill_gradientn(colours = pal2) + #, breaks = b
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map", 
       subtitle = ''  ) 

grid.arrange(p1, p2nb, p2difnb, nrow = 1)

ggsave(
  'Figure_02_mmb_clusters.png',
  plot = grid.arrange(p1, p2nb, p2difnb, nrow = 1), #last_plot()
  dpi = 400,
  width = 15,
  height = 6,
  limitsize = TRUE)


# ------------- bovliv no bats in scenario 2 (all bovidae) --------------------

want_risk2_nobats_bovliv <- c('pig_gilbert', 'bovliv','mmb',
                       "builtup"     ,   
                       "energy",                     
                       "agriharv", 
                       "forest_integrity_grantham" ,        
                       "hewson_forest_transition_potential",
                       "pop_2020_worldpop"  )

dfgplot <- dplyr::mutate(dfgplot, 
                         n_hotspots_risk2_nobats_bovliv = rowSums(dfgplot[want_risk2_nobats_bovliv]  > 1.645, 
                                                           na.rm = TRUE))
length(unique(dfgplot$n_hotspots_risk2_nobats_bovliv))

unique(dfgplot$n_hotspots_risk2_nobats_bovliv)

cats <- length(unique(dfgplot$n_hotspots_risk2_nobats_bovliv))
pal2 <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))
b <- c(0,2,4,6,8,10 )

go2nb <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk2_nobats_bovliv")))

# Spectral 
p2nbb <- ggplot()+
  geom_tile(data = go2nb, aes( y=y, x=x, fill = n_hotspots_risk2_nobats_bovliv))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  scale_fill_gradientn(colours = pal2, breaks = b) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Landscape change, \n potential  (non-bat hosts) secondary hosts (all bovidae)'  ) 

p2nbb

# Difference with nb all bovidae scenario 2

dfgplot$difnbb = dfgplot$n_hotspots_risk2_nobats_bovliv -  dfgplot$n_hotspots_risk1 

summary(dfgplot$difnbb)

summary(dfgplot$difnb)

table(dfgplot$n_hotspots_risk2 -dfgplot$n_hotspots_risk3)


p2difnbb <- ggplot()+
  geom_tile(data = dfgplot, aes( y=y, x=x, fill = difnbb))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(layer) ), label.size = 0.1, alpha = 0.7)+
  scale_fill_gradientn(colours = pal2) + #, breaks = b
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map", 
       subtitle = ''  ) 

p2difnb

ggsave(
  'Figure_02_mmb_bovliv_clusters.png',
  plot = grid.arrange(p1, p2nbb, p2difnbb, nrow = 1), #last_plot()
  dpi = 400,
  width = 15,
  height = 6,
  limitsize = TRUE)

#--------------------------------