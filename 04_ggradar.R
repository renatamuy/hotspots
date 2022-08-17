# Spider graphs from maps

require(tidyverse)
require(ggplot2)
library(here())

#https://xl0418.github.io/2018/12/05/2018-12-05-ggradar2/
#devtools::install_github("xl0418/ggradar2",dependencies=TRUE)

#remotes::install_github("AllanCameron/geomtextpath")

setwd(here())
setwd('results/')

dfg <- read.csv("gstar.csv")

#18.24306 to 52.33333 and longitude from 75.98951 to 134.28917

dfradar <- dfg
dfradar <- subset(dfradar, y > 19 & y <40  )
dfradar <- subset(dfradar, x > 80 & y <120  )

nrow(dfradar)
12486/6
dfradar$mock_cluster <- c(rep('Pequim', nrow(dfradar)/6, ) , rep('Bla', nrow(dfradar)/6),
                          rep('Blu', nrow(dfradar)/6),
                          rep('Ble', nrow(dfradar)/6),
                          rep('Bli', nrow(dfradar)/6), rep('Blo', nrow(dfradar)/6))

#% Extract the group names. Otherwise, the first column will be chosen as the group names.
#% The radar chart is not a nice presentation if you want to compare too many groups. Thus here 
#% we only focus on 4 groups.
dftest = dfradar[c(1,9000),]

#% To better distinguish two different styles, 6 groups are selected for illustration.
# Get values for trajectories
dfgplot$lincomb_hosts
dfgplot$lincomb_shosts
dfgplot$lincomb_human_vulnerability
dfgplot$lincomb_habitat_quality


todo <- c('lincomb_hosts', 'lincomb_shosts',  'lincomb_human_vulnerability', 'lincomb_habitat_quality'  )
dftest = dftest[,todo]

row.names(dftest) <- dftest$mock_cluster
dftest$mock_cluster <- NULL
ggradar2(dftest, webtype = 'lux')

fullscore <- c(100,10,300,150,10,10)

#devtools::install_github("ricardo-bion/ggradar", 
#                         dependencies = TRUE)


dfg$tile <- row.names(dfg)

library(reshape2)
dfm <- melt(dfg[,c(todo,"tile", 'x', 'y')], 
            id.vars= c("tile", 'x', 'y'))


library(ggplot2)

# Define selected tiles

addref <- data.frame(tile= rep('Neutral-Hotspot shift',4), 
                     x=c(NA, NA, NA, NA),
                     y=c(NA, NA, NA, NA), variable=
                       c("lincomb_hosts", 
                         "lincomb_shosts",
                         'lincomb_habitat_quality' ,
                         'lincomb_human_vulnerability')
                     , value =c(1.96,1.96,1.96,1.96)  )

dfmplot <- dfm

# Selection or regions for display ---------------------------------------------------------------------------
# Gaps show possibilities for change between hotspots
selection <- sample( 1:24000, 6)

base <- dfmplot %>% filter(tile %in% selection) %>% 
  mutate(variablelab = fct_recode(variable, "Bat \n hosts" = "lincomb_hosts",
                                            'Secondary \n hosts'="lincomb_shosts",
                                            'Habitat \n modification' = 'lincomb_habitat_quality',
                                            'Human \n vulnerability' = 'lincomb_human_vulnerability' )) %>% 
  ggplot(aes(x = variablelab, y = value, group = tile))+ #, colour = tile, fill = tile)) + 
  facet_wrap(~tile)+
  geom_polygon(size = 1, alpha= 1, colour = 'gray15', fill = NA, lty = 3) +
  geom_point(size = 2,  shape = 18) + # colour='gray15'
  geomtextpath::coord_curvedpolar()+ #coord_polar() +
  ylim(-6.0, 6.0) + ggtitle("")  + 
  annotate("rect", alpha = 0.4, ymin = -1.96, ymax = 1.96, xmin = 0, xmax = 5,
       fill = "khaki")+
  annotate("rect", alpha = 0.4, ymin = -Inf, ymax = -1.959, xmin = 0, xmax = 5,
           fill = "royalblue3")+
  annotate("rect", alpha = 0.4, ymin = 1.969, ymax = 6, xmin = 0, xmax = 5,
           fill = "violetred4",
           colour = NA)+
  theme_light()+
  labs(x = 'Components', y = 'G*i', fill = 'Region', colour = 'Region' ) +
  theme(strip.background = element_rect(fill = "grey32"),
                       legend.position = "right",
                       text = element_text(size = 16)) 
#+  theme(plot.margin = unit(c(-.8,-.8,-.8,-.8),"cm")   )

base

# Export 
ggsave('hotspots_trajectories.png',
  plot = base,
  dpi = 400,
  width = 13,
  height = 8,
  limitsize = TRUE)


# Separate

todoa <- c( 'lincomb_hosts', "cattle_gilbert", 
           "chicken_gilbert", 
           'lincomb_habitat_quality',
           'lincomb_human_vulnerability', 
           "pig_gilbert",
           "mammals_iucn_mode"        )

dfga <- melt(dfg[,c(todoa,"tile", 'x', 'y')], 
            id.vars= c("tile", 'x', 'y'))


dfga <- dfga %>% mutate(variablelab = fct_recode(variable, 
                                "Bat \n hosts" = "lincomb_hosts",
                                'Cattle' ="cattle_gilbert" ,
                                'Habitat \n modification' = 'lincomb_habitat_quality',
                                'Human \n vulnerability' = 'lincomb_human_vulnerability',
                                'Mammals' = "mammals_iucn_mode",
                                'Pigs \n'="pig_gilbert",
                                'Poultry' = "chicken_gilbert"              )) 

levels(dfga$variablelab)

circlefull <- length(unique(dfga$variablelab)) + 1

selection <- sample( 1:24000, 6)

basea <- dfga %>% filter(tile %in% selection) %>% 
  ggplot(aes(x = variablelab, y = value, group = tile))+ #, colour = tile, fill = tile)) + 
  facet_wrap(~tile)+
  geom_polygon(size = 1, alpha= 1, colour = 'gray15', fill = NA, lty = 3) +
  geom_point(size = 2,  shape = 18) + # colour='gray15'
   ylim(-16.0, 16.0) + ggtitle("")  + 
  annotate("rect", alpha = 0.4, ymin = -1.96, ymax = 1.96, xmin = 0, xmax = circlefull,
           fill = "khaki")+
  annotate("rect", alpha = 0.4, ymin = -Inf, ymax = -1.959, xmin = 0, xmax = circlefull,
           fill = "royalblue3")+
  annotate("rect", alpha = 0.4, ymin = 1.969, ymax = 16, xmin = 0, xmax = circlefull,
           fill = "violetred4",
           colour = NA)+
  geomtextpath::coord_curvedpolar()+ #coord_polar() +
  theme_light()+
  labs(x = 'Components', y = 'G*i', fill = 'Region', colour = 'Region' ) +
  theme(strip.background = element_rect(fill = "grey32"),
        legend.position = "right",
        text = element_text(size = 14)) 

basea

# Select regions and zonal this

dfga

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


require(raster)
hosts_muylaert <- raster::raster( 'D:/OneDrive - Massey University/bat_non_bat/data/region/hosts_muylaert.tif')

res(hosts_muylaert)
bbox(hosts_muylaert)
ras_dom<-raster::raster(xmn=68.25, xmx= 141.0, ymn=-10.25, ymx=53.5,
                crs="+proj=longlat +datum=WGS84 +no_defs ",
                resolution=res(hosts_muylaert), vals=NA)

coordinates(dfradar) <- ~ x + y 
crs(dfradar) <- "+proj=longlat +datum=WGS84 +no_defs "
  
colnames(dfradar@data)
# G star values rasterized

pigg <- rasterize(dfgsub, ras_dom, field = c("pig_gilbert"), update = TRUE) 
cattleg <- rasterize(dfgsub, ras_dom, field = c('cattle_gilbert'), update = TRUE) 
chickeng <- rasterize(dfgsub, ras_dom, field = c('chicken_gilbert'), update = TRUE) 
hostsg <-  rasterize(dfgsub, ras_dom, field = c('lincomb_hosts'), update = TRUE) 
mammalg <- rasterize(dfgsub, ras_dom, field = c("mammals_iucn_mode"), update = TRUE)
human_vulnerabilityg <- rasterize(dfgsub, ras_dom, field = c('lincomb_human_vulnerability'), update = TRUE) 
# think high foest quality and high forest division
habitatmodg <- rasterize(dfgsub, ras_dom, field = c("lincomb_habitat_quality"), update = TRUE) 

namesg <- c('pigg', 'cattleg', 'chickeng', 'hostsg', 'mammalg', 'human_vulnerabilityg', 'habitatmodg')

stackg <- stack(pigg,cattleg, chickeng, hostsg,mammalg,human_vulnerabilityg,habitatmodg )
names(stackg) <- namesg
plot(stackg)

# Zonal

testando <- raster::extract( stackg, target,  fun=mean, na.rm=TRUE, df=TRUE, weights = FALSE, sp=TRUE) 

head(testando)
##############################################################################################################
# Check
test1 <- sf::st_as_sf(testando)
sf::st_crs(crs(test1)) 
require(sf)
head(test1)

maptheme <- theme_classic() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5, size=14),
        legend.title = element_text(size=10), 
        strip.background = element_blank(),
        strip.text = element_text(size=16))


# Zonal 
ggplot() +
  geom_sf(data=test1, aes(fill=pigg), col="black", size=0.3, alpha=0.8)  +
   coord_sf(crs = st_crs(crs(target)) )+ #ylim = c(-30, 80), xlim = c(-20, 181))+
  scale_fill_viridis_c( na.value= NA, alpha = .4)+  #trans = "sqrt"
  ggtitle( 'Hosts' )  + 
  maptheme+
  theme(legend.title=element_blank(), plot.title=element_text(size=16, face = "italic"))

# Continuous with countries
ggplot() +
  geom_tile(data= as.data.frame(pigg, xy=TRUE) %>%  drop_na(), aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_c( na.value= NA, alpha = .9)+  #trans = "sqrt"
  geom_sf(data=test1, fill= 'transparent', col="white", size=0.3)  + # alpha=0.8
  coord_sf(crs = st_crs(crs(target)))+ #, ylim = c(-30, 80), xlim = c(-20, 181))+
  ggtitle( 'Pig' )  + 
  maptheme+
  theme(legend.title=element_blank(), plot.title=element_text(size=16, face = "italic"))+  ylab('Latitude') + xlab('Longitude')
#


tomeltg <- as.data.frame(test1)


mgg <- melt(tomeltg[,c(namesg, 'name')], 
            id.vars= c('name'))

mgg

# Radar with plots

circlefull <- length(unique(mgg$variable)) + 1

#selection <- sample( 1:24000, 6)
unique(mgg$variable)

mggp <- mgg %>% mutate(variablelab = fct_recode(variable, 
                                                 "Bat \n hosts" = "hostsg",
                                                 'Cattle' ="cattleg" ,
                                                 'Habitat \n modification' = 'habitatmodg',
                                                 'Human \n vulnerability' = 'human_vulnerabilityg',
                                                 'Mammals' = "mammalg",
                                                 'Pigs \n'="pigg",
                                                 'Poultry' = "chickeng"              )) 


max(mggp$value)

mggp %>% filter(name == 'Bangladesh') 

basez <- mggp %>% #filter(name != 'Bangladesh') %>% 
  ggplot(aes(x = variablelab, y = value, group = name))+ #, colour = tile, fill = tile)) + 
  facet_wrap(~name)+ #ncol
  geom_polygon(size = 1, alpha= 1, colour = 'gray15', fill = NA, lty = 3) +
  geom_point(size = 2,  shape = 18) + # colour='gray15'
  ylim(-16.0, 19.0) + ggtitle("")  + 
  annotate("rect", alpha = 0.4, ymin = -1.96, ymax = 1.96, xmin = 0, xmax = circlefull,
           fill = "khaki")+
  annotate("rect", alpha = 0.4, ymin = -Inf, ymax = -1.959, xmin = 0, xmax = circlefull,
           fill = "royalblue3")+
  annotate("rect", alpha = 0.4, ymin = 1.969, ymax = 16, xmin = 0, xmax = circlefull,
           fill = "violetred4",
           colour = NA)+
  geomtextpath::coord_curvedpolar()+ #coord_polar() +
  theme_light()+
  labs(x = 'Components', y = 'G*i', fill = 'Region', colour = 'Region' ) +
  theme(strip.background = element_rect(fill = "grey32"),
        legend.position = "right",
        text = element_text(size = 14)) 

basez

# Exporting
ggsave('hotspots_trajectories_countries_mean.png',
       plot = basez,
       dpi = 400,
       width = 13,
       height = 14,
       limitsize = TRUE)



##------------------------------------------------------------------------------------------------------------
#Optional annotations for plotx
#scale_fill_brewer(palette = 'Greys')+
#scale_colour_brewer(palette = 'Greys')+
#annotate("text", x = 1, y = 1.90,
#         label = "Neutral", parse = TRUE, colour= "khaki") +
#Dashed line
#geom_hline(yintercept= 1.96 , lty=2, size = 1, alpha= 0.2, colour = 'black') +

#-------------------------------------------------------------------------------------------------------------