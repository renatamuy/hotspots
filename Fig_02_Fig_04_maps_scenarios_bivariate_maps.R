#------------------------------------------
# Muylaert et al.
# Figure - Hotspot overlap areas (cattle only and supplemental all bovidae livestock)
# Figure -  bivariate maps (cattle only)

library(here)

setwd(here())

source('00_packages.R')

setwd('results')

dfgplot <- read.csv('gstar.csv')

head(dfgplot)

setwd(here())

setwd('results')

dir.create('scenarios')

setwd('scenarios')

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


# Scenario 1
#
#
#

want_landscape <- c('builtup', 'energy',  'agriharv', 
                    'forest_integrity_grantham',
                    'hewson_forest_transition_potential'                  )

want_bat <- c('lincomb_hosts')

want_expo_spread <- c('pop_2020_worldpop')

want_risk1 <- c(want_landscape, want_bat, want_expo_spread)

mm <- reshape2::melt(dfgplot[c('x','y',want_landscape,'lincomb_hosts','pop_2020_worldpop')], id.vars=c('x','y') )

b <- mm %>% filter(variable == 'builtup' )

head(b)
head(dfgplot)

setdiff(want_risk1, colnames(dfgplot))

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk1 = rowSums(dfgplot[want_risk1]  > 1.645, na.rm = TRUE))

go <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk1")))

cats <- length(unique(dfgplot$n_hotspots_risk1))
pal <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))
b <- c(0,2,4, 6, 8,10 )

p1 <- ggplot()+
  geom_tile(data = go, aes(y=y, x=x, fill = n_hotspots_risk1))+ theme_bw() +
  scale_fill_gradientn(colours = pal, breaks = b) +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  theme(legend.title=element_blank(), legend.position = 'bottom',  
        title = element_text(size=16, face='bold')) +
  labs(x='Longitude', y="Latitude", 
       title = "Scenario 1") 
       #subtitle = 'Landscape change and known bat hosts \n                          '  ) 

p1

# Scenario 2
#
#

want_risk2 <- c(want_risk1, 'pig_gilbert', 'cattle_gilbert' )

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk2 = rowSums(dfgplot[want_risk2]  > 1.645, na.rm = TRUE))
length(unique(dfgplot$n_hotspots_risk2))
unique(dfgplot$n_hotspots_risk2)

go2 <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk2")))

# Spectral 
p2 <- ggplot()+
  geom_tile(data = go2, aes( y=y, x=x, fill = n_hotspots_risk2))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal, breaks = b) +
  theme(legend.title=element_blank(), 
        legend.position = 'bottom',  title = element_text(size=16, face='bold')) +
  labs(x='Longitude', y="Latitude", 
       title = "Scenario 2") 
       #subtitle = 'Landscape change, known bat hosts and \n potential domestic secondary hosts (livestock)'  ) 

p2

# Difference
go2$dif = go2$n_hotspots_risk2 - go$n_hotspots_risk1 

hist(go2$dif)

p2dif <- ggplot()+
  geom_tile(data = go2, aes( y=y, x=x, fill = factor(dif )  ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_manual(values = pal)+
  #scale_fill_gradientn(colours = pal, breaks = bdif) + #, 
  theme(legend.title=element_blank(), legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map") 
       #subtitle = ''  ) 

p2dif

# One-panel plots just in case
#ggsave(
 # 'Figure_02_scenarios_0102.png',
 # plot = grid.arrange(p1, p2, p2dif, nrow = 1), #last_plot()
 # dpi = 400,
 # width = 15,
  #height = 6,
  #limitsize = TRUE)

# Scenario 2 bovliv 
#
#

want_risk2_bovliv <- c(want_risk1, 'pig_gilbert', 'bovliv')

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk2_bovliv = rowSums(dfgplot[want_risk2_bovliv]  > 1.645, na.rm = TRUE))

unique(dfgplot$n_hotspots_risk2)

go3 <- dfgplot %>% dplyr::select(c('x','y', starts_with("n_hotspots_risk2_bovliv")))

p2b <- ggplot()+
  geom_tile(data = go3, aes( y=y, x=x, fill =n_hotspots_risk2_bovliv))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal, breaks = b) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", 
       title = "Scenario 2")
       #subtitle = 'Landscape change, known bat hosts and \n potential domestic secondary hosts (all bovidae)'  ) 

p2b

# Dif 2 and bovliv

head(go3)
go3$difbovliv = go3$n_hotspots_risk2_bovliv -  go$n_hotspots_risk1 

hist(go2$dif)
summary(go2$dif)

p2difb <- ggplot()+
  geom_tile(data = go3, aes( y=y, x=x, fill = factor(difbovliv) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_manual(values = pal)+
  #scale_fill_gradientn(colours = pal, breaks = bdif) + 
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map"       ) #subtitle = ''

p2difb

# Scenario 3 with bat hosts, other wild mammals but no livestock ----------
#
#
want_risk3 <- c('mmb', want_risk1 )

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk3 = rowSums(dfgplot[want_risk3]  > 1.645, 
                                                           na.rm = TRUE))

go2nb <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk3")))

p3 <- ggplot()+
  geom_tile(data = go2nb, aes( y=y, x=x, fill = n_hotspots_risk3))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal, breaks = b) +
  theme(legend.title=element_blank(), legend.position = 'bottom', 
        title = element_text(size=16, face='bold')) +
  labs(x='Longitude', y="Latitude", title = "Scenario 3")
       #, 
       #subtitle = 'Landscape change, potential secondary hosts \n (mammal wildlife including bat hosts)'  ) 

p3

# Difference 

dfgplot$dif3 = dfgplot$n_hotspots_risk3 -  dfgplot$n_hotspots_risk1 

summary(go2$dif)


p3dif <- ggplot()+
  geom_tile(data = dfgplot, aes( y=y, x=x, fill = factor(dif3) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent', col="black", size=0.50)+
  scale_fill_manual(values = pal)+
  theme(legend.title=element_blank(), legend.position = 'bottom', 
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map") #, 
       #subtitle = ''  ) 

p3dif

# Scenario 4 ---- all components
#
#
#
#
want_risk4 <- c('mmb',
                 'cattle_gilbert',
                 'pig_gilbert',
                 want_risk1 )

dfgplot <- dplyr::mutate(dfgplot, 
                         n_hotspots_risk4 = rowSums(dfgplot[want_risk4]  > 1.645, 
                                                     na.rm = TRUE))
length(unique(dfgplot$n_hotspots_risk4))

unique(dfgplot$n_hotspots_risk4)

go4 <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk4")))

p4 <- ggplot()+
  geom_tile(data = go4, aes( y=y, x=x, fill = n_hotspots_risk4))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal, breaks = b) +
  theme(legend.title = element_blank(), legend.position = 'bottom',
        title = element_text(size=16, face='bold')) +
  labs(x='Longitude', y="Latitude", 
       title = "Scenario 4") 
       #subtitle = 'Bat hosts, mammal wildlife, livestock'  ) 

p4

# Difference

dfgplot$dif4 = dfgplot$n_hotspots_risk4 -  dfgplot$n_hotspots_risk1 

p4dif <- ggplot()+
  geom_tile(data = dfgplot, aes( y=y, x=x, fill = factor(dif4) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_manual(values = pal)+
  #scale_fill_gradientn(colours = pal, breaks = bdif) + 
  theme(legend.title=element_blank(), legend.position = 'bottom', 
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map") 
       #subtitle = ''  ) 

p4dif

# Scenario 4 (all components, all bovidae)

want_risk4b <- c('mmb',
                 'bovliv',
                 'pig_gilbert',
                 want_risk1 )

dfgplot <- dplyr::mutate(dfgplot, 
                         n_hotspots_risk4b = rowSums(dfgplot[want_risk4b]  > 1.645, 
                                                             na.rm = TRUE))
length(unique(dfgplot$n_hotspots_risk4b))

unique(dfgplot$n_hotspots_risk4b)

go4b <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk4b")))

# Spectral 
p4b <- ggplot()+
  geom_tile(data = go4b, aes( y=y, x=x, fill = n_hotspots_risk4b))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal, breaks = b) +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Scenario 4")
       #subtitle = 'Bat hosts, mammal wildlife, livestock (all bovidae)'  ) 

p4b

# Difference
dfgplot$dif4b = dfgplot$n_hotspots_risk4b -  dfgplot$n_hotspots_risk1 

p4difb <- ggplot()+
  geom_tile(data = dfgplot, aes( y=y, x=x, fill = factor(dif4b) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_manual(values = pal)+
  #scale_fill_gradientn(colours = pal,  breaks = bdif) + #, breaks = b
  theme(legend.title=element_blank(), legend.position = 'bottom', 
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map"        ) # subtitle = ''

p4difb

# Plots ------------------------------
# Scenarios cattle-only

ggsave( 'Figure_02.png',
  plot = grid.arrange(p1, p2, p3, p4, nrow = 1), #last_plot()
  dpi = 400,
  width = 17,
  height = 6,
  limitsize = TRUE)

# 2 rows
ggsave( 'Figure_02_2rows.png',
        plot = grid.arrange(p1, p2, p3, p4, nrow = 2), #last_plot()
        dpi = 400,
        width = 11,
        height = 11,
        limitsize = TRUE)


#---- Bovliv
ggsave( 'Figure_02_bovliv.png',
        plot = grid.arrange(p1, p2b, p3, p4b, nrow = 1), #last_plot()
        dpi = 400,
        width = 17,
        height = 6,
        limitsize = TRUE)

# Dif cattle-only
ggsave('Figure_02_dif.png',
  plot = grid.arrange(p1, p2, p3, p4,
                      p1, p2dif, p3dif, p4dif,
                      nrow = 2), #last_plot()
  dpi = 400,
  width = 17,
  height = 12,
  limitsize = TRUE)


#---- Dif Bovliv
ggsave('Figure_02_dif_bovliv.png',
       plot = grid.arrange(p1, p2b, p3, p4b,
                           p1, p2difb, p3dif, p4difb,
                           nrow = 2), #last_plot()
       dpi = 400,
       width = 17,
       height = 12,
       limitsize = TRUE)

#########################################
# Bivariate maps ---------------------------------------------------------
setwd(here())
setwd('region')

raster_access <- raster('motor_travel_time_weiss.tif')

mini <- min(na.omit(values(raster_access)))
maxi <- max(na.omit(values(raster_access)))
values(raster_access) <-( values(raster_access) - mini)/ (maxi-mini)

ras_dom <-raster::raster(xmn=68.25, xmx= 141.0, ymn=-10.25, ymx=53.5,
                         crs="+proj=longlat +datum=WGS84 +no_defs ",
                         resolution=res(raster_access), vals=NA)

db <- dfgplot[c('x', 'y', 
                'n_hotspots_risk1',
                'n_hotspots_risk2', 
                'n_hotspots_risk3', 
                'n_hotspots_risk4')]

# Risk scenario 1 (just bat hosts) -----------------------------------
db$risk1 <- (db$n_hotspots_risk1-min(db$n_hotspots_risk1))/
  (max(db$n_hotspots_risk1)-min(db$n_hotspots_risk1))

hist(db$risk2)
hist(db$risk1)
quantile(db$risk1)
quantile(raster_access )
coordinates(db) <- ~ x + y 

crs(db) <- "+proj=longlat +datum=WGS84 +no_defs "

raster1 <- rasterize(db, ras_dom, 
                     field = c("risk1"),
                     update = TRUE)

col.matrix <- bivariatemaps::colmat(nquantiles=3,
                                    upperleft= 'khaki',
                                    upperright= '#ff0000',
                                    bottomleft= 'azure2',#'#d8d8d8',#'#1B9E77',  #"azure2",
                                    bottomright='blue', #'#141414',# "#141414",
                                    xlab="Time to reach healthcare", 
                                    ylab="Scenario 1")

bivmap1 <- bivariate.map(raster_access, raster1, 
                         colormatrix=col.matrix, nquantiles=3)

# Bivariate scenario 1

setwd(here())
setwd('results')
dir.create('Figure_04')
setwd('Figure_04')

plot(bivmap1,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main='Scenario 1')

map(interior=T, add=T)

dev.off()

# Scenario 2 ---------------------------------------------

db$risk2 <- (db$n_hotspots_risk2-min(db$n_hotspots_risk2))/
  (max(db$n_hotspots_risk2)-min(db$n_hotspots_risk2))

hist(db$risk2)
quantile(db$risk3)

raster2 <- rasterize(db, ras_dom, 
                     field = c("risk2"),
                     update = TRUE)

#Color check: https://encycolorpedia.com/b22222
col.matrix <- bivariatemaps::colmat(nquantiles=3,
                   upperleft= 'khaki',
                   upperright= '#ff0000', 
                   bottomleft= 'azure2',
                   bottomright= 'blue',
                   xlab="Time to reach healthcare", 
                   ylab="Scenario 2")

bivmap2 <- bivariate.map(raster_access, raster2, 
colormatrix=col.matrix, nquantiles=3)

# Bivariate scenario 2
plot(bivmap2,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main='Scenario 2')

map(interior=T, add=T)
dev.off()
# Scenario 3
db$risk3 <- (db$n_hotspots_risk3-min(db$n_hotspots_risk3))/
  (max(db$n_hotspots_risk3)-min(db$n_hotspots_risk3))

plot(db$risk3 , db$risk2)

raster3 <- rasterize(db, ras_dom, field = c("risk3"),
                     update = TRUE)

#https://encycolorpedia.com/b22222

col.matrix <- bivariatemaps::colmat(nquantiles=3,
                                    upperleft= 'khaki',
                                    upperright= '#ff0000',
                                    bottomleft= 'azure2',
                                    bottomright= 'blue',
                                    xlab="Time to reach healthcare", 
                                    ylab="Scenario 3")

bivmap3 <- bivariate.map(raster_access, raster3, 
                         colormatrix=col.matrix, nquantiles=3)

# Bivariate scenario 3
png(filename = 'scenario_03.png', res = 400, units = 'cm', width = 20, height = 20      )
plot(bivmap3,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main='Scenario 3')

map(interior=T, add=T)
dev.off()
# --- Scenario 4

db$risk4 <- (db$n_hotspots_risk4-min(db$n_hotspots_risk4))/
  (max(db$n_hotspots_risk4)-min(db$n_hotspots_risk4))

raster4 <- rasterize(db, ras_dom, 
                     field = c("risk4"),
                     update = TRUE)


#https://encycolorpedia.com/b22222

col.matrix <- bivariatemaps::colmat(nquantiles=3,
                                    upperleft= 'khaki',#'khaki', #, ##4DDDDD
                                    upperright= '#ff0000',#"#B22222",#"violetred4", 
                                    bottomleft= 'azure2',#'#d8d8d8',#'#1B9E77',  #"azure2",
                                    bottomright= 'blue',#'#141414',# "#141414",
                                    xlab="Time to reach healthcare", 
                                    ylab="Scenario 4")

png(filename = 'scenario_04.png', res = 400, units = 'cm', width = 20, height = 20      )
bivmap4 <- bivariate.map(raster_access, raster4, 
                         colormatrix=col.matrix, nquantiles=3)

plot(bivmap4,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main= 'Scenario 4')

map(interior=T, add=T)
dev.off()

# 4 bivariate maps together
#par(mfrow=c(2,2))
png(filename = 'scenario_01.png', res = 400, units = 'cm', width = 20, height = 20      )
plot(bivmap1,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main= 'Scenario 1')
plot(s19s, add=TRUE)
dev.off()

png(filename = 'scenario_02.png', res = 400, units = 'cm', width = 20, height = 20      )

plot(bivmap2,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main= 'Scenario 2')
plot(s19s, add=TRUE)
dev.off()

png(filename = 'scenario_03.png', res = 400, units = 'cm', width = 20, height = 20      )
plot(bivmap3,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main= 'Scenario 3')
plot(s19s, add=TRUE)
dev.off()

png(filename = 'scenario_04.png', res = 400, units = 'cm', width = 20, height = 20      )
plot(bivmap4,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main= 'Scenario 4')
plot(s19s, add=TRUE)
dev.off()

#--

library(png)
setwd(here())

setwd('results/Figure_04')

pa1 <-  readPNG('scenario_01.png')
pa2 <-  readPNG('scenario_02.png')
pa3 <-  readPNG('scenario_03.png')
pa4 <-  readPNG('scenario_04.png')


grid.arrange(rasterGrob(pa4))

dev.off()

#heights=c(3/4, 1/4, 1/4, 1/4)
# Check risk values
db %>% skim()

# Stack 
bivs <- stack(bivmap1, bivmap2, bivmap3, bivmap4)

names(bivs) <- c('bivmap1', 'bivmap2', 'bivmap3', 'bivmap4')

plot(bivs >11, col=c('snow3', 'red'))

bivshigh <- stack(bivs >11)

table(values(bivshigh$bivmap1))
table(values(bivshigh$bivmap2))
table(values(bivshigh$bivmap3))
table(values(bivshigh$bivmap4))


plot(bivs == 3|bivs == 9|bivs == 12, col=c('snow3', 'red'))

setwd(here())
setwd('results/skater_optimal_cluster_size_19/')
s19s <- raster::shapefile('s19s.shp')

bivsplot <- bivs
names(bivsplot) <- c('Scenario 1', 'Scenario 2', 
                     'Scenario 3', 'Scenario 4')


#Close to healthcare, high hostspot overlap
# Figure S7
png(filename = 'Figure_S07.png', res = 400, units = 'cm', width = 20, height = 12     )
par(mfrow=c(2,4))

plot(bivs$bivmap1 == 3, col=c('snow3', 'yellow'), 
     legend=FALSE,
     main= c('Scenario 1', table(values(bivs$bivmap1== 3))[2] ))
plot(s19s, add=TRUE)
plot(bivs$bivmap2 == 3, col=c('snow3', 'yellow'), 
     legend=FALSE,
     main= c('Scenario 2', table(values(bivs$bivmap2 == 3))[2]) )
plot(s19s, add=TRUE)
plot(bivs$bivmap3 == 3, col=c('snow3', 'yellow'), 
     legend=FALSE,
     main= c('Scenario 3',  table(values(bivs$bivmap3 == 3))[2])) 
plot(s19s, add=TRUE)
plot(bivs$bivmap4 == 3, col=c('snow3', 'yellow'), 
     legend=FALSE,
     main= c('Scenario 4',table(values(bivs$bivmap4 == 3))[2] ))
plot(s19s, add=TRUE)

#'Far from healthcare, high hostspot overlap'
#par(mfrow=c(2,2))

plot(bivs$bivmap1 == 12, col=c('snow3', 'red'), 
     legend=FALSE,
     main= c('Scenario 1', table(values(bivs$bivmap1== 12))[2] ))
plot(s19s, add=TRUE)

plot(bivs$bivmap2 == 12, col=c('snow3', 'red'), 
     legend=FALSE,
     main= c('Scenario 2', table(values(bivs$bivmap2 == 12))[2]) )
plot(s19s, add=TRUE)
plot(bivs$bivmap3 == 12, col=c('snow3', 'red'), 
     legend=FALSE,
     main= c('Scenario 3',  table(values(bivs$bivmap3 == 12))[2])) 
plot(s19s, add=TRUE)
plot(bivs$bivmap4 == 12, col=c('snow3', 'red'), 
     legend=FALSE,
     main= c('Scenario 4',table(values(bivs$bivmap4 == 12))[2] ))
plot(s19s, add=TRUE)

dev.off()


values(bivs) %>% skim()

# 60.58% of the total area

100 * (table(values(bivs$bivmap1 == 3))[2] / 25796 )
100 * (table(values(bivs$bivmap2 == 3))[2]/ 25796 )
100 * (table(values(bivs$bivmap3 == 3))[2] / 25796 )
100 * (table(values(bivs$bivmap4 == 3))[2] / 25796 ) 


100 * (table(values(bivs$bivmap1 == 12))[2] / 25796 )
100 * (table(values(bivs$bivmap2 == 12))[2]/ 25796 )
100 * (table(values(bivs$bivmap3 == 12))[2] / 25796 )
100 * (table(values(bivs$bivmap4 == 12))[2] / 25796 )

# Conditionally safer zones

100 * (table(values(bivs$bivmap1 == 1))[2] / 25796 )
100 * (table(values(bivs$bivmap2 == 1))[2]/ 25796 )
100 * (table(values(bivs$bivmap3 == 1))[2] / 25796 )
100 * (table(values(bivs$bivmap4 == 1))[2] / 25796 )

plot(bivs$bivmap4 == 1, col=c('snow3', 'black'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 1))[2] ))

unique(values(bivs$bivmap4))

# Color codes in bivariate
# Red 12
plot(bivs$bivmap4 == 12, 
     col=c('snow3', 'red'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 12))[2] ))

# Orange 9
plot(bivs$bivmap4 == 9, 
     col=c('snow3', 'orange'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 9))[2] ))

# Yellow 3

plot(bivs$bivmap4 == 3, 
     col=c('snow3', 'khaki'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 3))[2] ))

# light blue-khaki
plot(bivs$bivmap4 == 1 , 
     col=c('black', 'azure2'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 1 | bivs$bivmap4 == 2))[2] ))

table(values(bivs$bivmap4))

# light blue
plot(bivs$bivmap4 == 7, 
     col=c('snow3', 'black'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 7))[2] ))

#dark blue
plot(bivs$bivmap4 == 10, 
     col=c('snow3', 'black'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 10))[2] ))

# dark purple
plot(bivs$bivmap4 == 11, 
     col=c('snow3', 'black'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 11))[2] ))

# light purple
plot(bivs$bivmap4 == 8, 
     col=c('snow3', 'black'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 8))[2] ))

# orange 
plot(bivs$bivmap4 == 9, 
     col=c('snow3', 'orange'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 9))[2] ))

#  

plot(bivs$bivmap4 == 1 | bivs$bivmap4 == 7 | bivs$bivmap4 == 10, 
     col=c('snow3', 'blue'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 1  | bivs$bivmap4 == 7 | bivs$bivmap4 == 10))[2] ))

plot(bivs$bivmap4 == 2 | bivs$bivmap4 == 8 | bivs$bivmap4 == 11, 
     col=c('snow3', 'khaki'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 2  | bivs$bivmap4 == 8 | bivs$bivmap4 == 11))[2] ))


plot(bivs$bivmap4 == 3 | bivs$bivmap4 == 9 | bivs$bivmap4 == 12, 
     col=c('snow3', 'red'), 
     legend=FALSE,
     main= c('',table(values(bivs$bivmap4 == 3  | bivs$bivmap4 == 9 | bivs$bivmap4 == 128))[2] ))

# bivs

setwd(here())
setwd('region')
#motorized minutes

raster_access <- raster('motor_travel_time_weiss.tif')

raster_access_hours <- raster_access/60

plot(raster1, raster2, pch=19, cex=2)
plot(raster1, raster4, pch=19, cex=2)

dev.off()

# Average time to healthcare (h) in areas of higher risk (third quantiles)

#Table 1

summary(raster_access_hours[which(values(bivs$bivmap1) %in% c (12))])
summary(raster_access_hours[which(values(bivs$bivmap2) %in% c (12))])
summary(raster_access_hours[which(values(bivs$bivmap3) %in% c (12))])
summary(raster_access_hours[which(values(bivs$bivmap4) %in% c (12))])

sd(raster_access_hours[which(values(bivs$bivmap1) %in% c (12))])
sd(raster_access_hours[which(values(bivs$bivmap2) %in% c (12))])
sd(raster_access_hours[which(values(bivs$bivmap3) %in% c (12))])
sd(raster_access_hours[which(values(bivs$bivmap4) %in% c (12))])

boxplot(raster_access_hours[which(values(bivs$bivmap1) %in% c (12))])

http://127.0.0.1:44513/graphics/plot_zoom_png?width=2099&height=1129
part1 <- data.frame(Scenario = 'Scenario 1', Time = raster_access_hours[which(values(bivs$bivmap1) %in% c (12))] )

part2 <- data.frame(Scenario = 'Scenario 2', Time = raster_access_hours[which(values(bivs$bivmap2) %in% c (12))] )
http://127.0.0.1:44513/graphics/plot_zoom_png?width=2099&height=1129
part3 <- data.frame(Scenario = 'Scenario 3', Time = raster_access_hours[which(values(bivs$bivmap3) %in% c (12))] )

part4 <- data.frame(Scenario = 'Scenario 4', Time = raster_access_hours[which(values(bivs$bivmap4) %in% c (12))] )

allbox <- rbind(part1, part2,part3,part4)

# Violin plot
p <- ggplot(allbox, aes(Scenario, Time))

p + geom_violin(fill="snow2") + #geom_dotplot(bin axis='y', stackdir='center', dotsize=0.2)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, fill='red') + 
  labs(y= 'Time to reach healthcare (hours)', x='')+ 
  theme_bw() +theme( axis.title = element_text(size = 20),  axis.text = element_text(size = 18))


#install.packages("violinplotter")
#library(violinplotter)
#violinplotter(Time ~ Scenario, data = allbox)

library(ggpubr)


# ---------------
bartlett.test(Time ~ Scenario, data = allbox) # variance not homogeneous

# variance homogeneity test for non normal data
fligner.test(Time ~ Scenario, data = allbox)

## Wilcoxon test for multiple groups of different sample sizes: two.sided (default)
my_comparisons <- list( c("Scenario 1", "Scenario 3"), c("Scenario 2", "Scenario 3"),
                        c("Scenario 4", "Scenario 3") )


viof <- ggplot(allbox, aes(Scenario, Time)) + geom_violin(fill="snow2") + 
  stat_compare_means(method = "wilcox.test", comparisons = my_comparisons)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, fill='red') + 
  labs(y= 'Time to reach healthcare (hours)', x='')+ 
  theme_bw() +theme( axis.title = element_text(size = 20),  axis.text = element_text(size = 18))

viof
table(allbox$Scenario)

# Export figure 4 B
setwd(here())
setwd('results/scenarios')

ggsave( 'Figure_04B.png',
        plot = viof, #last_plot()
        dpi = 400,
        width = 8,
        height = 8,
        limitsize = TRUE)

#------------------------------------------------
# Yellow areas - high risk - close to healthcare

part1y <- data.frame(Scenario = 'Scenario 1', Time = raster_access_hours[which(values(bivs$bivmap1) %in% c (3))] )
part2y <- data.frame(Scenario = 'Scenario 2', Time = raster_access_hours[which(values(bivs$bivmap2) %in% c (3))] )
part3y <- data.frame(Scenario = 'Scenario 3', Time = raster_access_hours[which(values(bivs$bivmap3) %in% c (3))] )
part4y <- data.frame(Scenario = 'Scenario 4', Time = raster_access_hours[which(values(bivs$bivmap4) %in% c (3))] )

allboxy <- rbind(part1y, part2y,part3y,part4y)

table(allboxy$Scenario)

my_comparisons <- list( c("Scenario 1", "Scenario 3"), c("Scenario 2", "Scenario 3"),
                        c("Scenario 4", "Scenario 3") )

viofy <- ggplot(allboxy, aes(Scenario, Time)) + geom_violin(fill="snow2") + 
  stat_compare_means(method = "wilcox.test", comparisons = my_comparisons)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, fill='khaki') + 
  labs(y= 'Time to reach healthcare (hours)', x='')+ 
  theme_bw() +theme( axis.title = element_text(size = 20),  axis.text = element_text(size = 18))

viofy

summary(raster_access_hours[which(values(bivs$bivmap1) %in% c (3))])
summary(raster_access_hours[which(values(bivs$bivmap2) %in% c (3))])
summary(raster_access_hours[which(values(bivs$bivmap3) %in% c (3))])
summary(raster_access_hours[which(values(bivs$bivmap4) %in% c (3))])

# Orange - high risk, average time to health care

part1o <- data.frame(Scenario = 'Scenario 1', Time = raster_access_hours[which(values(bivs$bivmap1) %in% c (9))] )
part2o <- data.frame(Scenario = 'Scenario 2', Time = raster_access_hours[which(values(bivs$bivmap2) %in% c (9))] )
part3o <- data.frame(Scenario = 'Scenario 3', Time = raster_access_hours[which(values(bivs$bivmap3) %in% c (9))] )
part4o <- data.frame(Scenario = 'Scenario 4', Time = raster_access_hours[which(values(bivs$bivmap4) %in% c (9))] )

allboxo <- rbind(part1o, part2o,part3o,part4o)

my_comparisons <- list( c("Scenario 1", "Scenario 3"), c("Scenario 2", "Scenario 3"),
                        c("Scenario 4", "Scenario 3") )


viofo <- ggplot(allboxo, aes(Scenario, Time)) + geom_violin(fill="snow2") + 
  stat_compare_means(method = "wilcox.test", comparisons = my_comparisons)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, fill='orange') + 
  labs(y= 'Time to reach healthcare (hours)', x='')+ 
  theme_bw() +theme( axis.title = element_text(size = 20),  axis.text = element_text(size = 18))

table(allboxo$Scenario)

summary(raster_access_hours[which(values(bivs$bivmap1) %in% c (9))])
summary(raster_access_hours[which(values(bivs$bivmap2) %in% c (9))])
summary(raster_access_hours[which(values(bivs$bivmap3) %in% c (9))])
summary(raster_access_hours[which(values(bivs$bivmap4) %in% c (9))])


#------------------------------------------