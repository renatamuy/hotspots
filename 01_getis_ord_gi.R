# Loading libraries 

library(proj4)
library(rgdal)
library(spdep)
library(maptools)
require(here())
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
  
  templocalGvalues <- localG(x = pt, listw = nb2listw(knn, style = "B"), 
                             zero.policy = FALSE, alternative="two-sided")
  
  dfg[t] <- templocalGvalues

}

# Checking hot spots from z-scores ---------------------------------------------
# Host hotspots

critical <- 1.645 #1.645 #1.9546

dfgplot <- dfg

tofocus_hosts <- c("hosts_muylaert", "hosts_sanchez")

cor(dfg$hosts_muylaert,dfg$hosts_sanchez, method = 'spearman')

dfg$lincomb_hosts <- apply(dfg[tofocus_hosts], FUN=mean, MARGIN=1)

dfgplot$p95_hosts <- ifelse(dfg$lincomb_hosts > critical, "<0.05",
                          ifelse(dfg$lincomb_hosts  < -critical, "<0.05", "ns") )

dfgplot$col95_hosts <- ifelse(dfg$lincomb_hosts > critical, "violetred4",
                         ifelse(dfg$lincomb_hosts  < -critical, "royalblue3", "khaki") )

dfgplot$hot95_hosts <-  ifelse(dfg$lincomb_hosts > critical, "Hotspot",
                                ifelse(dfg$lincomb_hosts  < -critical, "Coldspot", "Neutral") )

# End hosts-------------------------------------------

# Wild mammal hosts

dfgplot$p95_mammals <- ifelse(dfg$mammals_iucn_mode > critical, "<0.05",
                                 ifelse(dfg$lincomb_hosts  < -critical, "<0.05", "ns") )

dfgplot$col95_mammals <- ifelse(dfg$mammals_iucn_mode > critical, "violetred4",
                                ifelse(dfg$mammals_iucn_mode < -critical, "royalblue3", "khaki") )

dfgplot$hot95_mammals <-  ifelse(dfg$mammals_iucn_mode > critical, "Hotspot",
                           ifelse(dfg$mammals_iucn_mode  < -critical, "Coldspot", "Neutral") )

#par(mfrow=c(2,1))

#plot(data= dfgplot, y~x, col= col95_hosts, pch=19)

#plot(data= dfgplot, y~x, col= col95_mammals, pch=19)

cor(dfg$lincomb_hosts, dfg$mammals_iucn_mode, method = 'spearman')

# Potential domestic sec hosts

cor(dfg$cattle_gilbert, dfg$pig_gilbert, method = 'spearman')

dfgplot$p95_pig <- ifelse(dfg$pig_gilbert > critical, "<0.05",
                                  ifelse(dfg$pig_gilbert < -critical, "<0.05", "ns") )

dfgplot$col95_pig <- ifelse(dfg$pig_gilbert > critical, "violetred4",
                                 ifelse(dfg$pig_gilbert  < -critical, "royalblue3", "khaki") )

dfgplot$hot95_pig <-  ifelse(dfg$pig_gilbert > critical, "Hotspot",
                            ifelse(dfg$pig_gilbert  < -critical, "Coldspot", "Neutral") )


# Cattle

dfgplot$p95_cattle <- ifelse(dfg$cattle_gilbert > critical, "<0.05",
                          ifelse(dfg$cattle_gilbert < -critical, "<0.05", "ns") )

dfgplot$col95_cattle <- ifelse(dfg$cattle_gilbert > critical, "violetred4",
                            ifelse(dfg$cattle_gilbert  < -critical, "royalblue3", "khaki") )

dfgplot$hot95_cattle <-  ifelse(dfg$cattle_gilbert > critical, "Hotspot",
                         ifelse(dfg$cattle_gilbert  < -critical, "Coldspot", "Neutral") )

#par(mfrow=c(2,1))

#plot(data= dfgplot, y~x, col= col95_pig, pch=19, main='Pig')

#plot(data= dfgplot, y~x, col= col95_cattle, pch=19, main='Cattle')

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
  
  pcol <- ifelse(dfg[w] > critical, "<0.05",
                 ifelse(dfg[w]  < -critical, "<0.05", "ns") )
  
  colcol <- ifelse(dfg[w]  > critical, "violetred4",
                   ifelse(dfg[w] < -critical, "royalblue3", "khaki") )
    
  hotcol <-  ifelse(dfg[w]  > critical, "Hotspot",
                    ifelse(dfg[w]  < -critical, "Coldspot", "Neutral") )
  
  colnamep <- paste0('p95_', w)
  colnamec <- paste0('col95_', w)
  colnameh <- paste0('hot95_', w)
  
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

# Export data for multivariate-cluster and spider plots 

fname <- paste0('gstar_', critical, '.csv' )
write.csv(dfgplot, fname, row.names = FALSE)

# SLOW
#Conditional permutations added for comparative purposes; 
#permutations are over the whole data vector omitting the observation itself.
# Permutation

#localGvalues_sim <- localG_perm(info_spatial,  nsim=499, nb2listw(knn, style="B"), 
#zero.policy = FALSE, alternative="two-sided")
#---------

#---------------------
# Choose coding style for nb2listw() in localG() calculation
#style="B" ensures symmetry of the neighbourhood matrix
#"W", that is row-standardised weights, 
# default "M", unknown style; if not "M", passed to nb2listw to re-build the object; 
# "M", meaning matrix style, underlying style unknown, or assigned style argument in rebuilt object
# Different coding styles can be used to minimize topology-induced heterogeneity on heatmaps
#. "B" is the basic binary coding, "W" is row standardised (sums
#over all links to n, 
#"W" style assigns higher leverage to spatial objects with few connections, such as those on the periphery of a study region, or islands), 
# C is globally standardised (sums over all links to n, results show that the "C" style emphasizes spatial objects with relatively
#large numbers of connections, such as those in the interior of a study region), 
#U is equal to C divided by the number of neighbours (sums over all links to unity), 
# while "S" is the variance-stabilizing coding scheme proposed by Tiefelsdorf et al. 1999, p. 167-168 (sums over all links to n).
#For the global test,  recommended that the weights style be binary (one of c("B","C","U")).
################################################################################
# If zero policy is set to TRUE, weights vectors of zero length are inserted for regions without neighbour
#in the neighbours list. These will in turn generate lag values of zero, equivalent to the sum of
#products of the zero row t(rep(0, length=length(neighbours))) %*% x, for arbitraty numerical
#vector x of length length(neighbours). The spatially lagged value of x for the zero-neighbour
#region will then be zero, which may (or may not) be a sensible choice.
#If the sum of the glist vector for one or more observations is zero, a warning message is issued. The
#consequence for later operations will be the same as if no-neighbour observations were present and
#the zero.policy argument set to true.
# Creating the localG statistic for each of counties, with a k-nearest neighbor value of k, and round this to 3 decimal places
#The local spatial statistic G is calculated for each zone based on the spatial weights object used. The
#value returned is a Z-value, and may be used as a diagnostic tool.
# High positive values indicate the
#posibility of a local cluster of high values of the variable being analysed, very low relative values
#a similar cluster of low values. For inference, a Bonferroni-type test is suggested in the references,
#where tables of critical values may be found (see also details below).
#Deafut alternative="greater"
#Binary weights recommended (sepecially for distance bands)
#499
#https://search.r-project.org/CRAN/refmans/spdep/html/localG.html
#If the neighbours member of listw has a "self.included" attribute set to TRUE, the Gstar variant, 
# including the self-weight w_{ii} > 0, is calculated and returned. The returned vector will have a "gstari" attribute set to TRUE. 
#The value returned from getis ord g is a Z-value, and may be used as a diagnostic tool.
# High positive values indicate the posibility of a local cluster of high values of the variable being analysed, very low relative values a similar cluster of low values. For inference, 
# a Bonferroni-type test is suggested in the references, 
# where tables of critical values may be found 
#(see also details below).
#The critical values of the statistic under assumptions given in the references for the 95th percentile are for 
# n=1: 1.645, n=50: 3.083, n=100: 3.289, n=1000: 3.886.
#https://www.rdocumentation.org/packages/spdep/versions/1.2-4/topics/localG
# Create a new data frame that only includes the county fips codes and the G scores
# Binary weights recommended (especially for distance bands)
#zero policies: use global option value; if TRUE assign zero to the lagged value of zones without neighbours, 
# if FALSE assign NA
#Starting from a binary neighbours list, in which regions are either listed as neighbours or are absent
#(thus not in the set of neighbours for some deﬁnition), the function adds a weights list with values
#given by the coding scheme style chosen. B is the basic binary coding, W is row standardised (sums
#  over all links to n), C is globally standardised (sums over all links to n), U is equal to C divided by
#the number of neighbours (sums over all links to unity), while S is the variance-stabilizing coding
#scheme proposed by Tiefelsdorf et al. 1999, p. 167-168 (sums over all links to n)



