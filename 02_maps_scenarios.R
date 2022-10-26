# Figure 02 - Hotspot convergence areas
# Figure 04 - bivariate maps

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

#pal <- scico::scico(length(unique(dfgplot$n_hotspots_landscape)), palette = 'lajolla')

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
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", 
       title = "Scenario 1", 
       subtitle = 'Landscape change and known bat hosts \n                          '  ) 

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
  #scale_fill_gradientn(colours = c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", 
       title = "Scenario 2", 
       subtitle = 'Landscape change, known bat hosts and \n potential domestic secondary hosts (livestock)'  ) 

p2

# Difference map:
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
       title = "Difference map", 
       subtitle = ''  ) 

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
       title = "Scenario 2", 
       subtitle = 'Landscape change, known bat hosts and \n potential domestic secondary hosts (all bovidae)'  ) 

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
       title = "Difference map", 
       subtitle = ''  ) 

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
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Scenario 3", 
       subtitle = 'Landscape change, potential secondary hosts \n (mammal wildlife including bat hosts)'  ) 

p3

# Difference 

dfgplot$dif3 = dfgplot$n_hotspots_risk3 -  dfgplot$n_hotspots_risk1 

summary(go2$dif)


p3dif <- ggplot()+
  geom_tile(data = dfgplot, aes( y=y, x=x, fill = factor(dif3) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent', col="black", size=0.50)+
  scale_fill_manual(values = pal)+
  #scale_fill_gradientn(colours = pal, breaks = bdif) + 
  theme(legend.title=element_blank(), legend.position = 'bottom', 
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map", 
       subtitle = ''  ) 

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

# Spectral 
p4 <- ggplot()+
  geom_tile(data = go4, aes( y=y, x=x, fill = n_hotspots_risk4))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal, breaks = b) +
  theme(legend.title = element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", 
       title = "Scenario 4", 
       subtitle = 'Bat hosts, mammal wildlife, livestock'  ) 

p4

# Difference

dfgplot$dif4 = dfgplot$n_hotspots_risk4 -  dfgplot$n_hotspots_risk1 

head(go2nb)
head(dfgplot)

p4dif <- ggplot()+
  geom_tile(data = dfgplot, aes( y=y, x=x, fill = factor(dif4) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_manual(values = pal)+
  #scale_fill_gradientn(colours = pal, breaks = bdif) + 
  theme(legend.title=element_blank(), legend.position = 'bottom', 
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map", 
       subtitle = ''  ) 

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
  labs(x='Longitude', y="Latitude", title = "Scenario 4", 
       subtitle = 'Bat hosts, mammal wildlife, livestock (all bovidae)'  ) 

p4b

# Difference

dfgplot$dif4b = dfgplot$n_hotspots_risk4b -  dfgplot$n_hotspots_risk1 

head(go2nb)
head(dfgplot)

p4difb <- ggplot()+
  geom_tile(data = dfgplot, aes( y=y, x=x, fill = factor(dif4b) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_manual(values = pal)+
  #scale_fill_gradientn(colours = pal,  breaks = bdif) + #, breaks = b
  theme(legend.title=element_blank(), legend.position = 'bottom', 
        strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Difference map", 
       subtitle = ''  ) 

p4difb

# Plots ------------------------------

# Scenarios cattle-only
ggsave( 'Figure_02.png',
  plot = grid.arrange(p1, p2, p3, p4, nrow = 1), #last_plot()
  dpi = 400,
  width = 17,
  height = 6,
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

db %>% skim()

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

# Change labels!
col.matrix <- bivariatemaps::colmat(nquantiles=3,
                                    upperleft= 'khaki',#'khaki', #, ##4DDDDD
                                    upperright= '#ff0000',#"#B22222",#"violetred4", 
                                    bottomleft= 'azure2',#'#d8d8d8',#'#1B9E77',  #"azure2",
                                    bottomright='blue', #'#141414',# "#141414",
                                    xlab="Time to reach healthcare", 
                                    ylab="Scenario 1")

bivmap1 <- bivariate.map(raster_access, raster1, 
                         colormatrix=col.matrix, nquantiles=3)

# Bivariate scenario 1

plot(bivmap1,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main='Scenario 1')

map(interior=T, add=T)

# Scenario 2 ---------------------------------------------
# Bat Hosts + mammals mmb + pig + cattle + everything else

db$risk2 <- (db$n_hotspots_risk2-min(db$n_hotspots_risk2))/
  (max(db$n_hotspots_risk2)-min(db$n_hotspots_risk2))

hist(db$risk2)
quantile(db$risk3)

raster2 <- rasterize(db, ras_dom, 
                     field = c("risk2"),
                     update = TRUE)

#https://encycolorpedia.com/b22222

col.matrix <- bivariatemaps::colmat(nquantiles=3,
                   upperleft= 'khaki',#'khaki', #, ##4DDDDD
                   upperright= '#ff0000',#"#B22222",#"violetred4", 
                   bottomleft= 'azure2',#'#d8d8d8',#'#1B9E77',  #"azure2",
                   bottomright= 'blue',#'#141414',# "#141414",
                   xlab="Time to reach healthcare", 
                   ylab="Scenario 2")

bivmap2 <- bivariate.map(raster_access, raster2, 
colormatrix=col.matrix, nquantiles=3)

# Bivariate scenario 2

plot(bivmap2,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main='Scenario 2')

map(interior=T, add=T)

# Scenario 3

db$risk3 <- (db$n_hotspots_risk3-min(db$n_hotspots_risk3))/
  (max(db$n_hotspots_risk3)-min(db$n_hotspots_risk3))

plot(db$risk3 , db$risk2)

raster3 <- rasterize(db, ras_dom, field = c("risk3"),
                     update = TRUE)

#https://encycolorpedia.com/b22222

col.matrix <- bivariatemaps::colmat(nquantiles=3,
                                    upperleft= 'khaki',#'khaki', #, ##4DDDDD
                                    upperright= '#ff0000',#"#B22222",#"violetred4", 
                                    bottomleft= 'azure2',#'#d8d8d8',#'#1B9E77',  #"azure2",
                                    bottomright= 'blue',#'#141414',# "#141414",
                                    xlab="Time to reach healthcare", 
                                    ylab="Hazard: Scenario 3")

bivmap3 <- bivariate.map(raster_access, raster3, 
                         colormatrix=col.matrix, nquantiles=3)

# Bivariate scenario 3

plot(bivmap3,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main='Scenario 3')

map(interior=T, add=T)

# --- Scenario 4

db$risk4 <- (db$n_hotspots_risk4-min(db$n_hotspots_risk4))/
  (max(db$n_hotspots_risk4)-min(db$n_hotspots_risk4))


head(db)
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

#Green zones are graph nodes with the strongest influence on the total risk of pandemic spread

bivmap4 <- bivariate.map(raster_access, raster2nb, 
                         colormatrix=col.matrix, nquantiles=3)

plot(bivmap4,frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix), main= 'Scenario 4')

map(interior=T, add=T)


db %>% skim()


# Stack with all of them 
bivs <- stack(bivmap1, bivmap2, bivmap3, bivmap4)
names(bivs) <- c('bivmap1', 'bivmap2', 'bivmap3', 'bivmap4')

plot(bivs >11, col=c('snow3', 'red'))

col.matrix

bivshigh <- stack(bivs >11)

table(values(bivshigh$bivmap1))
table(values(bivshigh$bivmap2))
table(values(bivshigh$bivmap3))
table(values(bivshigh$bivmap4))

# Difference in risk

bivmapdif <-bivariate.map(raster_access, raster4-raster1, 
                       colormatrix=col.matrix, nquantiles=3)

plot(bivmapdif, frame.plot=F,axes=F,box=F,add=F,legend=F,
     col=as.vector(col.matrix))

#----------------------------------------------
# Pop plots
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
                               ifelse(bivmap1m == 10,'blue', #"#141414",
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

# Ggplot giving weird points on plot 

d$travel_log <- log10(d$motor_travel_time_weiss)
d$poplog <-log10(d$pop_2020_worldpop)

plot_quantiles_scenario1 <- ggplot(d, 
                                   aes(x = travel_log,
   y = poplog) )+ 
  geom_point(color=d$cores1, alpha=0.2)+
  facet_wrap(~d$cores1 )+
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
                               ifelse(bivmap2m == 10, 'blue', #"#141414",
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