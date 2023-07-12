# Muylaert et al. 
# Univariate Hotspots 
# Loading libraries 

rm(list=ls())

library(proj4)
library(rgdal)
library(spdep)
library(maptools)
require(here)
require(ggplot2)

# Define a list of neighbors for each location, using the k nearest neighbors 
# Steps
# knn2nb
# include.self
# ifelse
# LocalG score for each point
# round
# GlobalG test generate expected value, variance, 1 p-value

###########################################

setwd(here())
setwd('results')

p <- read.csv('prepdf.csv')

p$X <-NULL

coords <-cbind(p$x, p$y)

IDs <- row.names(p)

# Creating a list of neighbors for each location, using the k nearest neighbors 

n = 25

# Takes a while to run

knn <- knn2nb(knearneigh(coords, k = n, longlat = TRUE), row.names = IDs)

# knearneigh() Use longlat = TRUE if point coordinates are longitude-latitude decimal degrees, 
#in which case distances are measured in kilometers; if x is a SpatialPoints object, the value is taken from the object itself

knn <- include.self(knn)

todo <- colnames(p)[3:ncol(p)]

dfg <- data.frame(ID = IDs, x = p$x, y = p$y)

# univariate localG

for(t in todo){
  
  print(t)
  
  pt <- as.numeric(unlist(p[t]))
  
  # default NULL, use global option value
  #if TRUE assign zero to the lagged value of zones without neighbours, 
  #if FALSE assign NA
  
  templocalGvalues <- localG(x = pt, listw = nb2listw(knn, style = "B"), 
                             zero.policy = FALSE, alternative="two-sided")
  
  dfg[t] <- templocalGvalues

}

# Checking hot spots from z-scores ---------------------------------------------
# Host hotspots

critical <- 2.3238 

dfgplot <- dfg

tofocus_hosts <- c("hosts_muylaert", "hosts_sanchez")

cor(dfg$hosts_muylaert,dfg$hosts_sanchez, method = 'spearman')

dfgplot$lincomb_hosts <- apply(dfg[tofocus_hosts], FUN=mean, MARGIN=1)

dfgplot$p99_hosts <- ifelse(dfgplot$lincomb_hosts > critical, "<0.01",
                          ifelse(dfgplot$lincomb_hosts  < -critical, "<0.01", "ns") )

dfgplot$col99_hosts <- ifelse(dfgplot$lincomb_hosts > critical, "violetred4",
                         ifelse(dfgplot$lincomb_hosts  < -critical, "royalblue3", "khaki") )

dfgplot$hot99_hosts <-  ifelse(dfgplot$lincomb_hosts > critical, "Hotspot",
                                ifelse(dfgplot$lincomb_hosts  < -critical, "Coldspot", "Neutral") )

# End hosts-------------------------------------------

# Wild mammal hosts

dfgplot$p99_mammals <- ifelse(dfgplot$mammals_iucn_mode > critical, "<0.01",
                                 ifelse(dfgplot$lincomb_hosts  < -critical, "<0.01", "ns") )

dfgplot$col99_mammals <- ifelse(dfgplot$mammals_iucn_mode > critical, "violetred4",
                                ifelse(dfgplot$mammals_iucn_mode < -critical, "royalblue3", "khaki") )

dfgplot$hot99_mammals <-  ifelse(dfgplot$mammals_iucn_mode > critical, "Hotspot",
                           ifelse(dfgplot$mammals_iucn_mode  < -critical, "Coldspot", "Neutral") )


# Potential domestic sec hosts

# Pig - p
cor(dfgplot$cattle_gilbert, dfgplot$pig_gilbert, method = 'spearman')

dfgplot$p99_pig <- ifelse(dfgplot$pig_gilbert > critical, "<0.01",
                                  ifelse(dfgplot$pig_gilbert < -critical, "<0.01", "ns") )

dfgplot$col99_pig <- ifelse(dfgplot$pig_gilbert > critical, "violetred4",
                                 ifelse(dfgplot$pig_gilbert  < -critical, "royalblue3", "khaki") )

dfgplot$hot99_pig <-  ifelse(dfgplot$pig_gilbert > critical, "Hotspot",
                            ifelse(dfgplot$pig_gilbert  < -critical, "Coldspot", "Neutral") )

# Cattle - 

dfgplot$p99_cattle <- ifelse(dfg$cattle_gilbert > critical, "<0.01",
                          ifelse(dfg$cattle_gilbert < -critical, "<0.01", "ns") )

dfgplot$col99_cattle <- ifelse(dfg$cattle_gilbert > critical, "violetred4",
                            ifelse(dfg$cattle_gilbert  < -critical, "royalblue3", "khaki") )

dfgplot$hot99_cattle <-  ifelse(dfg$cattle_gilbert > critical, "Hotspot",
                         ifelse(dfg$cattle_gilbert  < -critical, "Coldspot", "Neutral") )


# bovliv Buffalo, cattle, goat, sheep -------------------------

dfgplot$p99_bovliv <- ifelse(dfg$buffalo_cattle_goat_sheep > critical, "<0.01",
                             ifelse(dfg$buffalo_cattle_goat_sheep < -critical, "<0.01", "ns") )

dfgplot$col99_bovliv <- ifelse(dfg$buffalo_cattle_goat_sheep > critical, "violetred4",
                               ifelse(dfg$buffalo_cattle_goat_sheep  < -critical, "royalblue3", "khaki") )

dfgplot$hot99_bovliv <-  ifelse(dfg$buffalo_cattle_goat_sheep > critical, "Hotspot",
                                ifelse(dfg$buffalo_cattle_goat_sheep  < -critical, "Coldspot", "Neutral") )

# "mammals_minus_bat_hosts"

dfgplot$p99_mmb <- ifelse(dfg$mammals_minus_bat_hosts > critical, "<0.01",
                             ifelse(dfg$mammals_minus_bat_hosts < -critical, "<0.01", "ns") )

dfgplot$col99_mmb <- ifelse(dfg$mammals_minus_bat_hosts > critical, "violetred4",
                               ifelse(dfg$mammals_minus_bat_hosts  < -critical, "royalblue3", "khaki") )

dfgplot$hot99_mmb <-  ifelse(dfg$mammals_minus_bat_hosts > critical, "Hotspot",
                                ifelse(dfg$mammals_minus_bat_hosts < -critical, "Coldspot", "Neutral") )


#------------------------------------------------------------

#par(mfrow=c(2,1))

#plot(data= dfgplot, y~x, col= col99_pig, pch=19, main='Pig')

#plot(data= dfgplot, y~x, col= col99_cattle, pch=19, main='Cattle')

dev.off()
#----------------------------------------------------------------------------------
# Selection of higher-level indicators

wanted <- c('builtup', 
            'energy', 
            'trans', 
            'agriharv', 
            'pollution',
                    'forest_integrity_grantham',
                    'hewson_forest_transition_potential' ,
            "pop_2020_worldpop", 
            "motor_travel_time_weiss"         )

for (w in wanted){
  print(w)
  
  pcol <- ifelse(dfg[w] > critical, "<0.01",
                 ifelse(dfg[w]  < -critical, "<0.01", "ns") )
  
  colcol <- ifelse(dfg[w]  > critical, "violetred4",
                   ifelse(dfg[w] < -critical, "royalblue3", "khaki") )
    
  hotcol <-  ifelse(dfg[w]  > critical, "Hotspot",
                    ifelse(dfg[w]  < -critical, "Coldspot", "Neutral") )
  
  colnamep <- paste0('p99_', w)
  colnamec <- paste0('col99_', w)
  colnameh <- paste0('hot99_', w)
  
  dftemp <- data.frame( colnamep = as.vector(pcol),   
                        colnamec = as.vector(colcol),
                        colnameh =  as.vector(hotcol) ) 
  
  colnames(dftemp) <- c(colnamep, colnamec, colnameh)
  
  dfgplot <- cbind(dfgplot, dftemp)
  
  }

colnames(dfgplot)

plot(p$builtup, dfgplot$builtup)

# Results creation -------------------------------------------------------------------------------------------
# Export data
setwd(here())
setwd('results')

# Export data for multivariate-cluster

fname <- paste0('gstar_099.csv')

colnames(dfgplot)
colnames(dfgplot)[14] <-  'mmb'  #'mammals_minus_bat_hosts'
colnames(dfgplot)[15] <- 'bovliv' # "buffalo_cattle_goat_sheep"

write.csv(dfgplot, fname, row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------