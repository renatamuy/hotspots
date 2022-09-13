# -- current 'risk' with no intermediate hosts

# risk from zero to one from z-scores
#Selection of higher-level indicators
# Risk surface considering all values
batpop <- rccpos * log(prcc+1)
batpop_pct <- (batpop- min(na.omit(values(batpop))))  / (max(na.omit(values(batpop))) - min(na.omit(values(batpop))) )

require(tidyverse)
require(stringr)
require(reshape2)

setwd(here())
setwd('results')
dfgplot <- read.csv('gstar.csv' )

head(dfgplot)

setwd(here())
setwd('results')

#----> identifies risk areas for change, surveillance sites in wildlife/communities
#Uses #s 1-3; 7 & 8

want_landscape <- c('builtup', 'energy',  'agriharv', 
                    'forest_integrity_grantham',
                    'hewson_forest_transition_potential'                  )

want_bat <- c('lincomb_hosts')

want_expo_spread <- c('pop_2020_worldpop')

want_risk1 <- c(want_landscape,want_bat,want_expo_spread)

# risk from hotspot convergence

#Melt
mm <- melt(dfgplot[c('x','y',want_landscape,'lincomb_hosts','pop_2020_worldpop')], id.vars=c('x','y') )

b <- mm %>% filter(variable == 'builtup' )

head(b)

plot(data = b, y~x, col= color95, pch=19)

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

p1 <- ggplot(data = go, aes( y=y, x=x, fill = n_hotspots_risk1))+
  geom_tile()+ theme_bw() +
  scale_fill_gradientn(colours = pal, breaks = b) +
  #scale_fill_gradientn(colours=c("lightskyblue", "darkmagenta", "darkorange1"), breaks = b, labels=format(b)) +
  #scale_fill_gradientn(colours= c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", subtitle = 'Equal weights from univariate hotspots'  ) 

p1
ggsave(
  'hotspots.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

# With royal blue pal

p1r <- ggplot(data = go, aes( y=y, x=x, fill = n_hotspots_risk1))+
  geom_tile()+ theme_bw() +
  scale_fill_gradientn(colours= c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", subtitle = 'Equal weights from univariate hotspots'  ) 

p1r

ggsave(
  'hotspots_royal.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

# Risk from 
#-- risks with intermediate hosts (and surveillance sites)
#----> identifies risk areas for change, surveillance sites in livestock/communities
#Uses #s 1-8

want_risk2 <- c(want_risk1, 'pig_gilbert', 'cattle_gilbert','mammals_iucn_mode' )

dfgplot <- dplyr::mutate(dfgplot, n_hotspots_risk2 = rowSums(dfgplot[want_risk2]  > 1.9546, na.rm = TRUE))
length(unique(dfgplot$n_hotspots_risk2))
unique(dfgplot$n_hotspots_risk2)

cats <- length(unique(dfgplot$n_hotspots_risk2))
pal2 <- rev(RColorBrewer::brewer.pal(cats,"Spectral"))
b <- c(0,2,4,6,8,10 )

go2 <- dfgplot %>% dplyr::select(c('x','y',starts_with("n_hotspots_risk2")))

# Spectral 
p2 <- ggplot(data = go2, aes( y=y, x=x, fill = n_hotspots_risk2))+
  geom_tile()+ theme_bw() +
  scale_fill_gradientn(colours = pal2, breaks = b) +
  #scale_fill_gradientn(colours = c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude", title = "Hotspot convergence areas", 
       subtitle = 'Equal weights considering potential secondary hosts'  ) 

p2

ggsave(
  'hotspots_sec.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)


p2r <- ggplot(data = go2, aes( y=y, x=x, fill = n_hotspots_risk2))+
  geom_tile()+ theme_bw() +
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

# Risk 3 + spread 

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

#----------------------------------------------------------------------