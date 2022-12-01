# Spider graphs from univariate hotspots and multivariate clusters  ---------------------------------

require(tidyverse)
require(ggplot2)
library(here)
require(raster)

#remotes::install_github("AllanCameron/geomtextpath")

setwd(here())

setwd('results/')

dfg <- read.csv("gstar.csv")

#18.24306 to 52.33333 and longitude from 75.98951 to 134.28917

dfradar <- dfg

dfg$tile <- row.names(dfg)

library(reshape2)
dfm <- melt(dfg[,c(todo,"tile", 'x', 'y')], 
            id.vars= c("tile", 'x', 'y'))

# Define selected tiles
dfmplot <- dfm

# Selection or regions for display ---------------------------------------------------------------------------

require(rnaturalearth)
worldmap <- ne_countries(scale = 'medium', returnclass = 'sf')
asia <- worldmap[worldmap$continent == 'Asia',]

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

hosts_muylaert <- raster::raster('D:/OneDrive - Massey University/bat_non_bat/data/region/hosts_muylaert.tif')

bbox(hosts_muylaert)

ras_dom<-raster::raster(xmn=68.25, xmx= 141.0, ymn=-10.25, ymx=53.5,
                crs="+proj=longlat +datum=WGS84 +no_defs ",
                resolution=res(hosts_muylaert), vals=NA)

tofocus <- colnames(   dfradar %>% dplyr::select(!c( 'ID', contains("95")))    )

dfgsub <- dfradar[tofocus]

str(dfgsub)

colnames(dfgsub[3:ncol(dfgsub)])

coordinates(dfgsub) <- ~ x + y 

crs(dfgsub) <- "+proj=longlat +datum=WGS84 +no_defs "
  
colnames(dfgsub@data)

# G star values rasterized

pigg <- rasterize(dfgsub, ras_dom, field = c("pig_gilbert"), update = TRUE) 
cattleg <- rasterize(dfgsub, ras_dom, field = c('cattle_gilbert'), update = TRUE) 
hostsg <-  rasterize(dfgsub, ras_dom, field = c('lincomb_hosts'), update = TRUE) 
mammalg <- rasterize(dfgsub, ras_dom, field = c("mmb"), update = TRUE)

forest_transitiong <- rasterize(dfgsub, ras_dom, field = c('hewson_forest_transition_potential'), update = TRUE) 
forest_qualityg <- rasterize(dfgsub, ras_dom, field = c('forest_integrity_grantham'), update = TRUE) 

builtupg <- rasterize(dfgsub, ras_dom, field = c('builtup'), update = TRUE) 
transg <- rasterize(dfgsub, ras_dom, field = c('trans'), update = TRUE) 
pollutiong <- rasterize(dfgsub, ras_dom, field = c('pollution'), update = TRUE) 
energyg <- rasterize(dfgsub, ras_dom, field = c('energy'), update = TRUE) 
agriharvg <- rasterize(dfgsub, ras_dom, field = c('agriharv'), update = TRUE) 
popg <- rasterize(dfgsub, ras_dom, field = c('pop_2020_worldpop'), update = TRUE) 
traveltimeg <- rasterize(dfgsub, ras_dom, field = c('motor_travel_time_weiss'), update = TRUE) 

namesg <- c('pigg', 'cattleg', 'hostsg', 'mammalg',
            'forest_transitiong', 'forest_qualityg',
            'builtupg','transg',  'pollutiong','energyg','agriharvg',
            'popg','traveltimeg'  )

stackg <- stack(pigg, cattleg, hostsg, mammalg,
                forest_transitiong, forest_qualityg,
                builtupg,transg,  pollutiong, energyg, agriharvg,
                popg, traveltimeg )

names(stackg) <- namesg

##############################################################################################################
# Zonal countries

testando <- raster::extract( stackg, target,  fun=mean, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 
head(testando)

require(sf)

test1 <- sf::st_as_sf(testando)
sf::st_crs(crs(test1)) 

tomeltg <- as.data.frame(test1)

# Removing travel time, pollution and transportation

namesg <-  namesg[!namesg %in% c('transg','pollutiong','traveltimeg')]

mgg <- melt(tomeltg[,c(namesg, 'name')], 
            id.vars= c('name'))

mgg

# Radar plots

circlefull <- length(unique(mgg$variable)) + 1

unique(mgg$variable)

mggp <- mgg %>% mutate(variablelab = fct_recode(variable, 
                                                 "Bat \n hosts" = "hostsg",
                                                 'Cattle' ="cattleg" ,
                                                 'Forest \n loss risk' = 'forest_transitiong',
                                                'Forest \n quality' = 'forest_qualityg',
                                                 'Population' = 'popg',
                                                'Access \n to healthcare' = 'traveltimeg',
                                                 'Mammals' = "mammalg",
                                                 'Pigs \n'="pigg",
                                                'Energy \n'="energyg",
                                                'Builtup \n'="builtupg",
                                                'Pollution \n'="pollutiong",
                                                'Transportation \n'="transg",
                                                'Agriculture \n and harvest'="agriharvg"  )) 



max(mggp$value)

mggp %>% filter(name == 'Bangladesh') 
# Straight

basez <- mggp %>% #filter(name != 'Bangladesh') %>% 
  ggplot(aes(x = variablelab, y = value, group = name)) + 
  facet_wrap(~name)+ #ncol
  #geom_polygon(size = 1, alpha= 1, colour = 'gray15', fill = NA, lty = 3) +
  geom_point(size = 2,  shape = 18) + # colour='gray15'
  ylim(-16.0, 20.0) + ggtitle("")  + 
  annotate("rect", alpha = 0.4, ymin = -1.645, ymax = 1.645, xmin = 0, xmax = circlefull,
           fill = "khaki")+
  annotate("rect", alpha = 0.4, ymin = -Inf, ymax = -1.645, xmin = 0, xmax = circlefull,
           fill = "royalblue3")+
  annotate("rect", alpha = 0.4, ymin = 1.645, ymax = 20, xmin = 0, xmax = circlefull,
           fill = "violetred4",
           colour = NA)+
  #geomtextpath::coord_curvedpolar()+                   ####coord_polar() +
  theme_light()+
  labs(x = 'Components', y = 'G*i', fill = 'Region', colour = 'Region' ) +
  theme(strip.background = element_rect(fill = "grey32"),
        legend.position = "right",
        text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size=9)) 

basez

# Hotspot variation plot (could be radar plot or flat)

baseradar <- mggp %>% #filter(name != 'Bangladesh') %>% 
  ggplot(aes(x = variablelab, y = value, group = name))+ #, colour = tile, fill = tile)) + 
  facet_wrap(~name)+ #ncol
  geom_polygon(size = 1, alpha= 1, colour = 'gray15', fill = NA, lty = 3) +
  geom_point(size = 2,  shape = 18) + # colour='gray15'
  ylim(-16.0, 20.0) + ggtitle("")  + 
  annotate("rect", alpha = 0.4, ymin = -1.645, ymax = 1.645, xmin = 0, xmax = circlefull,
           fill = "khaki")+
  annotate("rect", alpha = 0.4, ymin = -Inf, ymax = -1.645, xmin = 0, xmax = circlefull,
           fill = "royalblue3")+
  annotate("rect", alpha = 0.4, ymin = 1.645, ymax = 20, xmin = 0, xmax = circlefull,
           fill = "violetred4",
           colour = NA)+
  coord_polar() + #geomtextpath::coord_curvedpolar()+      # For curving labels           
  theme_light()+
  labs(x = 'Components', y = 'G*i', fill = 'Region', colour = 'Region' ) +
  theme(strip.background = element_rect(fill = "grey32"),
        legend.position = "right",
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 0.99, size=9)) 

baseradar

# Exporting
ggsave('hotspots_coord_polar.png',
       plot = baseradar,
       dpi = 400,
       width = 13,
       height = 14,
       limitsize = TRUE)

#-------------------------------------------------------------------------
# Final plot
setwd('skater_optimal_cluster_size_19/')

clusters <- read.csv("clusters_rgeoda_c19.csv")
clusters$bovliv <- NULL
clusters$mammals_iucn_mode <- NULL

setwd(here())
setwd('results/')

# -------------------------------------------------------------------------------
# Zonal with multivariate clusters from skater and univariate hotspot rasters
# Making raster from cluster 

dcc <- clusters %>% dplyr::select(c('x', 'y', 'cluster')) 

coordinates(dcc) <- ~ x + y 

crs(dcc) <- "+proj=longlat +datum=WGS84 +no_defs "

dcc

craster <- rasterize(dcc, ras_dom, field = c('cluster'), update = TRUE) 

plot(craster)

zcluster <- as.data.frame(raster::zonal(stackg, craster, 'mean'))

head(zcluster)

zclusterm <- melt(zcluster , id.vars= c('zone'))

head(zclusterm)

zclustermlab <- zclusterm %>% mutate(variablelab = fct_recode(variable, 
                                                "Bat hosts" = "hostsg", #\n
                                                'Cattle' ="cattleg" ,
                                                'Forest loss risk' = 'forest_transitiong', #\n 
                                                'Forest quality' = 'forest_qualityg', #\n 
                                                'Population' = 'popg',
                                                'Access to healthcare' = 'traveltimeg', #\n 
                                                'Mammals' = "mammalg",
                                                'Pigs'="pigg",
                                                'Energy'="energyg",
                                                'Builtup'="builtupg",
                                                'Pollution'="pollutiong",
                                                'Transportation'="transg",
                                                'Agriculture and harvest'="agriharvg"  )) #\n 

pal <- wesanderson::wes_palette("Moonrise3",
                                length(unique(zclustermlab$zone)),
                                type = "continuous")

# Removing trans, pollution and travel time
zclustermlab <- zclustermlab[!zclustermlab$variable %in% c("transg", "pollutiong", "traveltimeg"),]


baseradarz <- zclustermlab %>% 
  ggplot(aes(x = variablelab, y = value, group = zone))+ #, colour = tile, fill = tile)) + 
  facet_wrap(~zone, nrow = 5, ncol=4)+
  #geom_polygon(size = 1, alpha= 1, colour = 'gray15', fill = NA, lty = 3) +
  geom_point(size = 2  ) + # colour='gray15' #shape = 18
  ylim(-20.0, 20.0) + ggtitle("")  + 
  annotate("rect", alpha = 0.4, ymin = -1.645, ymax = 1.645, xmin = 0, xmax = circlefull,
           fill = "khaki")+
  annotate("rect", alpha = 0.4, ymin = -20 , ymax = -1.645, xmin = 0, xmax = circlefull,#-Inf
           fill = "royalblue3")+
  annotate("rect", alpha = 0.4, ymin = 1.645, ymax = 20, xmin = 0, xmax = circlefull,
           fill = "violetred4",         colour = NA)+
  theme_light()+
  coord_flip()+
  labs(x = 'Clusters', y = 'G*i', fill = 'Region', colour = 'Region' ) +
  theme(strip.background = element_rect(fill ="grey32"),
        strip.text.x = element_text(size=16),
        legend.position = "right",
        text = element_text(size = 14),  #panel.spacing = unit(0.5, "lines"),
        axis.text.x = element_text(angle = 45, hjust = 1.0, size=13))

baseradarz

##------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
library(ggridges)

joyc <- melt(clusters , id.vars= c('x','y','cluster'))

joyc <- joyc[!joyc$variable %in% c("motor_travel_time_weiss",
                                   "pollution", "trans"),]
unique(joyc$variable )

myorder <- rev(c('lincomb_hosts', 
             'mmb',
             'pig_gilbert',
             'cattle_gilbert',
             'hewson_forest_transition_potential',
             'forest_integrity_grantham',
             'energy',
             "builtup",
             "agriharv", 
             'pop_2020_worldpop'))

joyc$variable <- factor(joyc$variable, 
                          levels = myorder)



pal <- wesanderson::wes_palette("Moonrise3",
                                length(unique(zclustermlab$zone)),
                                type = "continuous")

cridges <- joyc %>% 
  mutate(variablelab = fct_recode(variable, 
                                      "Bat hosts" = "lincomb_hosts", #\n
                                      'Cattle' ="cattle_gilbert" ,
                                      'Forest loss risk' = 'hewson_forest_transition_potential', #\n 
                                      'Forest quality' = 'forest_integrity_grantham', #\n 
                                      'Population' = 'pop_2020_worldpop',
                                      'Mammals' = "mmb",
                                      'Pigs'="pig_gilbert",
                                      'Energy'="energy",
                                      'Builtup'="builtup",
                                      'Agriculture and harvest'="agriharv"  )) %>% 
  filter(variable != 'hosts_sanchez' & variable != 'hosts_muylaert') %>% 
  ggplot(aes(x =value , y = variablelab)) +
  facet_wrap(~cluster)+ 
  geom_density_ridges(scale=2, quantile_lines = TRUE, quantiles = 2) +
  annotate("rect", alpha = 0.4, xmin = -1.645, xmax = 1.645, ymin = 0, ymax = circlefull,
           fill = "khaki")+
  annotate("rect", alpha = 0.4, xmin = -20 , xmax = -1.645, ymin = 0, ymax = circlefull,
           fill = "royalblue3")+
  annotate("rect", alpha = 0.4, xmin = 1.645, xmax = 20, ymin = 0, ymax = circlefull,
           fill = "violetred4",         colour = NA)+
    coord_cartesian(xlim = c(-7, 20))+
  ylab('')+ xlab('G*i')+
  theme(strip.background = element_rect(fill = "snow3"),
        legend.position = "right",
        text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, hjust = 0.99, size=9)) 

cridges

gr <- ggplot_gtable(ggplot_build(cridges))

# Understanding grob slots - this must adapt according to the optimal number of clusters from maxp

gr$grobs[[121]]$grobs[[1]]$children[[1]]$gp$fill 
gr$grobs[[102]]$grobs[[1]]$children[[1]]$gp$fill 
102+19

strip_bothr <- c(118,119,120,121,
                 113,114,115,116,117, 
          108, 109,  110 ,111 ,112,
          102,103,104,105,106,107)

strip_bothr <- c(117, 118,119,120,121,
                112, 113,114,115,116, 
                107, 108, 109,  110 ,111 ,
                 102,103,104,105,106)

fillsr <- unique(pal)

k <- 1

for (i in strip_bothr) {
  j <- which(grepl('rect', gr$grobs[[i]]$grobs[[1]]$childrenOrder))
  gr$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fillsr[k]
  k <- k+1
}

# SLOW
grid::grid.draw(gr)

setwd(here())
setwd('results/skater_optimal_cluster_size_19/')

png("facet_clusters_ridges.png",
    width = 28,
    height = 25, unit='cm', res=600)
print(grid::grid.draw(gr))
dev.off()

#---------------------------------------------------------------------------------------------
