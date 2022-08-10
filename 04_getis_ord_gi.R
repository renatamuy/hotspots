# Loading libraries 
#install.packages("proj4")
library(proj4)
library(rgdal)
library(spdep)
library(maptools)
require(here())

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
setwd('results/hotspots_region')

p <- read.csv('prepdf.csv')

coords <-cbind(p$x, p$y)

IDs <- row.names(p)

# Creating a list of neighbors for each location, using the k nearest neighbors 

n = 25

# Same coordinates for all

knn <- knn2nb(knearneigh(coords, k = n, longlat = TRUE), row.names = IDs)

# knearneigh() Use longlat = TRUE if point coordinates are longitude-latitude decimal degrees, 
#in which case distances are measured in kilometers; if x is a SpatialPoints object, the value is taken from the object itself

knn <- include.self(knn)

todo <- colnames(p)[3:ncol(p)]

dfg <- data.frame(ID = IDs, x = p$x, y = p$y)

for(t in todo){
  
  print(t)
  
  pt <- as.numeric(unlist(p[t]))
  
  templocalGvalues <- localG(x = pt, listw = nb2listw(knn, style = "B"), zero.policy = TRUE, alternative="two-sided")
  
  dfg[t] <- templocalGvalues
  
  # testing - Binary weights recommended (sepecially for distance bands)
  #test <- globalG.test(x = pt, listw = nb2listw(knn, style = "B"), zero.policy = TRUE, alternative='two.sided')
  
  
}

# Creating average scores

dfg$lincomb_all <- apply(dfg[todo], FUN=mean, MARGIN=1)

colnames(dfg)

# Components
tofocus_hosts <- c("hosts_muylaert", "hosts_sanchez")

tofocus_sechosts <- c('mammals_iucn_mode', "pig_gilbert",
                                             "cattle_gilbert",
                                             "chicken_gilbert",
                                             "horse_gilbert",
                                             "sheep_gilbert" ,
                                             "buffalo_gilbert")



tofocus_habitat_quality <- c("forest_integrity_grantham", "forest_division_jung")
# Assumptions

tofocus_human_vulnerability <- c("pop_2020_worldpop", "motor_travel_time_weiss" )

# Lincomb
dfg$lincomb_hosts <- apply(dfg[tofocus_hosts], FUN=mean, MARGIN=1)

dfg$lincomb_sechosts <- apply(dfg[tofocus_sechosts], FUN=mean, MARGIN=1)

# High high here would be a highly fragmented area with a high integrity (so maybe also novel pathogens to 
#be discovered?)

dfg$lincomb_habitat_quality <- apply(dfg[tofocus_habitat_quality], FUN=mean, MARGIN=1)

dfg$lincomb_human_vulnerability <- apply(dfg[tofocus_human_vulnerability], FUN=mean, MARGIN=1)

#####---------------------------------------------------------------------------
# Lincomb to focus ------------------------------------------------------------ 

# Hosts hotspots

dfgplot <- dfg

dfgplot$pvalue95_hosts <- ifelse(dfg$lincomb_hosts > 1.96, "<0.05",
                          ifelse(dfg$lincomb_hosts  < -1.96, "<0.05", "ns") )


dfgplot$color95_hosts <- ifelse(dfg$lincomb_hosts > 1.96, "violetred4",
                         ifelse(dfg$lincomb_hosts  < -1.96, "royalblue3", "khaki") )

dfgplot$G_hosts <-  ifelse(dfg$lincomb_hosts > 1.96, "Hotspot",
                                ifelse(dfg$lincomb_hosts  < -1.96, "Coldspot", "Neutral") )


plot(data= dfgplot, y~x, col= color95_hosts, pch=19)

# Restricting effect-size >3.88

dfgplot$pvalue95_1000_hosts <- ifelse(dfgplot$lincomb_hosts > 3.886, "<0.01",
                               ifelse(dfgplot$lincomb_hosts  < -3.886, "<0.01", "ns") )

dfgplot$color95_1000_hosts <- ifelse(dfgplot$lincomb_hosts >3.886, "violetred4",
                              ifelse(dfgplot$lincomb_hosts  < -3.886, "royalblue3", "khaki") )


plot(data= dfgplot, y~x, col= color95_1000_hosts, pch=19)


#0.01
dfgplot$pvalue99_hosts <- ifelse(dfgplot$lincomb_hosts > 2.58, "<0.01",
                          ifelse(dfgplot$lincomb_hosts  < -2.58, "<0.01", "ns") )

dfgplot$color99_hosts <- ifelse(dfgplot$lincomb_hosts > 2.58, "violetred4",
                         ifelse(dfgplot$lincomb_hosts  < -2.58, "royalblue3", "khaki") )


plot(data= dfgplot, y~x, col= color99_hosts, pch=19)

# End hosts-------------------------------------------

# Start sec hosts hotspots --- n=7 so at 95%, critical value is 2.44
# https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1538-4632.1995.tb00912.x

dfg$lincomb_shosts <- dfg$lincomb_sechosts 

dfgplot$lincomb_sechosts <- NULL

dfgplot$pvalue95_shosts <- ifelse(dfg$lincomb_shosts > 2.44, "<0.05",
                                 ifelse(dfg$lincomb_shosts  < -1.96, "<0.05", "ns") )


dfgplot$color95_shosts <- ifelse(dfg$lincomb_shosts > 2.44, "violetred4",
                                ifelse(dfg$lincomb_shosts  < -1.96, "royalblue3", "khaki") )

dfgplot$G_shosts <-  ifelse(dfg$lincomb_shosts > 2.44, "Hotspot",
                           ifelse(dfg$lincomb_shosts  < -1.96, "Coldspot", "Neutral") )


plot(data= dfgplot, y~x, col= color95_shosts, pch=19)

# Restricting effect-size >3.88

dfgplot$pvalue95_1000_shosts <- ifelse(dfgplot$lincomb_shosts > 3.886, "<0.01",
                                      ifelse(dfgplot$lincomb_shosts  < -3.886, "<0.01", "ns") )

dfgplot$color95_1000_shosts <- ifelse(dfgplot$lincomb_shosts >3.886, "violetred4",
                                     ifelse(dfgplot$lincomb_shosts  < -3.886, "royalblue3", "khaki") )


plot(data= dfgplot, y~x, col= color95_1000_shosts, pch=19)


#0.01
dfgplot$pvalue99_shosts <- ifelse(dfgplot$lincomb_shosts > 2.58, "<0.01",
                                 ifelse(dfgplot$lincomb_shosts  < -2.58, "<0.01", "ns") )

dfgplot$color99_shosts <- ifelse(dfgplot$lincomb_shosts > 2.58, "violetred4",
                                ifelse(dfgplot$lincomb_shosts  < -2.58, "royalblue3", "khaki") )


plot(data= dfgplot, y~x, col= color99_shosts, pch=19)

# End shosts------

# Start habitat quality -------------------------------------------------------------------------------------

dfgplot$pvalue95_habitat_quality <- ifelse(dfg$lincomb_habitat_quality > 1.96, "<0.05",
                                  ifelse(dfg$lincomb_habitat_quality  < -1.96, "<0.05", "ns") )


dfgplot$color95_habitat_quality <- ifelse(dfg$lincomb_habitat_quality > 1.96, "violetred4",
                                 ifelse(dfg$lincomb_habitat_quality  < -1.96, "royalblue3", "khaki") )

dfgplot$G_habitat_quality <-  ifelse(dfg$lincomb_habitat_quality > 1.96, "Hotspot",
                            ifelse(dfg$lincomb_habitat_quality  < -1.96, "Coldspot", "Neutral") )


plot(data= dfgplot, y~x, col= color95_habitat_quality, pch=19)

# Restricting effect-size >3.88

dfgplot$pvalue95_1000_habitat_quality <- ifelse(dfgplot$lincomb_habitat_quality > 3.886, "<0.01",
                                       ifelse(dfgplot$lincomb_habitat_quality  < -3.886, "<0.01", "ns") )

dfgplot$color95_1000_habitat_quality <- ifelse(dfgplot$lincomb_habitat_quality >3.886, "violetred4",
                                      ifelse(dfgplot$lincomb_habitat_quality  < -3.886, "royalblue3", "khaki") )


plot(data= dfgplot, y~x, col= color95_1000_habitat_quality, pch=19)

#0.01

dfgplot$pvalue99_habitat_quality <- ifelse(dfgplot$lincomb_habitat_quality > 2.58, "<0.01",
                                  ifelse(dfgplot$lincomb_habitat_quality  < -2.58, "<0.01", "ns") )

dfgplot$color99_habitat_quality <- ifelse(dfgplot$lincomb_habitat_quality > 2.58, "violetred4",
                                 ifelse(dfgplot$lincomb_habitat_quality  < -2.58, "royalblue3", "khaki") )


plot(data = dfgplot, y~x, col = color99_habitat_quality, pch = 19)

# end habitat modification

# Start human vulnerability ---------------

dfgplot$pvalue95_human_vulnerability <- ifelse(dfg$lincomb_human_vulnerability > 1.96, "<0.05",
                                           ifelse(dfg$lincomb_human_vulnerability  < -1.96, "<0.05", "ns") )


dfgplot$color95_human_vulnerability <- ifelse(dfg$lincomb_human_vulnerability > 1.96, "violetred4",
                                          ifelse(dfg$lincomb_human_vulnerability  < -1.96, "royalblue3", "khaki") )

dfgplot$G_human_vulnerability <-  ifelse(dfg$lincomb_human_vulnerability > 1.96, "Hotspot",
                                     ifelse(dfg$lincomb_human_vulnerability  < -1.96, "Coldspot", "Neutral") )


plot(data= dfgplot, y~x, col= color95_human_vulnerability, pch=19)

# Restricting effect-size >3.88

dfgplot$pvalue95_1000_human_vulnerability <- ifelse(dfgplot$lincomb_human_vulnerability > 3.886, "<0.01",
                                                ifelse(dfgplot$lincomb_human_vulnerability  < -3.886, "<0.01", "ns") )

dfgplot$color95_1000_human_vulnerability <- ifelse(dfgplot$lincomb_human_vulnerability >3.886, "violetred4",
                                               ifelse(dfgplot$lincomb_human_vulnerability  < -3.886, "royalblue3", "khaki") )


plot(data= dfgplot, y~x, col= color95_1000_human_vulnerability, pch=19)

#0.01

dfgplot$pvalue99_human_vulnerability <- ifelse(dfgplot$lincomb_human_vulnerability > 2.58, "<0.01",
                                           ifelse(dfgplot$lincomb_human_vulnerability  < -2.58, "<0.01", "ns") )

dfgplot$color99_human_vulnerability <- ifelse(dfgplot$lincomb_human_vulnerability > 2.58, "violetred4",
                                          ifelse(dfgplot$lincomb_human_vulnerability  < -2.58, "royalblue3", "khaki") )


plot(data = dfgplot, y~x, col = color99_human_vulnerability, pch = 19)

# End human vulnerability ------------------------------------------------------------------------------------

# Plot melt panels

su <- dfgplot[, c('x','y', 'G_hosts','G_shosts','G_habitat_quality', 'G_human_vulnerability' )]

head(su)
colnames(su) <- c("x"  ,                   "y",
                  "Bat hosts"           ,    "Secondary hosts",             
                  "Habitat modification" ,    "Human vulnerability"                  )
require(reshape2)

# Results creation -------------------------------------------------------------------------------------------
# Export data

setwd('results/hotspots_region')

# Export data for cluster and spider plots (trajectory analysis)

write.csv(dfgplot, 'gstar.csv')

head(dfgplot)

sumelt <- melt(su, id=c("x", "y"))

colnames(sumelt)
unique(dfgplot$color95_shosts)


ggplot(data = sumelt, aes( y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(~variable, ncol=2)+
  scale_fill_manual(values =c("royalblue3", "violetred4","khaki" )  ) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude") 

ggsave('hotspots_facets.png',
  plot = last_plot(),
  dpi = 400,
  width = 8,
  height = 8,
  limitsize = TRUE)

write.table(file='table_pct.txt', row.names = FALSE,
round(table(sumelt$variable, sumelt$value)/ nrow(dfgplot)*100, digits=2) )

# Percentages

sumelt %>%
  ggplot(aes(variable)) + 
  geom_bar(aes(fill=value), alpha=0.8,position="fill") +
  scale_fill_manual(values =c("royalblue3", "violetred4","khaki" )  )+
  theme_minimal() + geom_rug() +
  theme(legend.title = element_blank()) + coord_flip() + 
  labs( title = "", x = "", y = "Area (%)", caption = "") 

ggsave(
  'hotspots_pct.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 5,
  limitsize = TRUE)


# Individual values

sumelt$tile <- rep(1:nrow(su))

png(filename= "stripes_hotspots.png", res=400, width = 22, height = 18, unit="cm")
ggplot(sumelt,aes(x=variable,y=tile,fill=value))+
  facet_grid(~variable, scales="free")+
  geom_tile(color=NA)+ labs(x="Component",y="Region ID") + theme_bw()+
  scale_fill_manual(values = c("royalblue3", "violetred4","khaki" ))+
  theme(strip.background =element_rect(fill="snow2"),legend.position = "bottom",
        text=element_text(size=16))+
  labs(fill = "")
dev.off()

# Latitude variation 
png(filename= "stripes_hotspots_lat.png", res = 400, width = 22, height = 18, unit="cm")
ggplot(sumelt,aes(x=variable,y=y,fill=value))+
  facet_grid(~variable, scales="free")+
  geom_tile(color=NA)+ labs(x="Component",y="Latitude") + theme_bw()+
  scale_fill_manual(values = c("royalblue3", "violetred4","khaki" ))+
  theme(strip.background =element_rect(fill="snow2"),legend.position = "bottom",
        text=element_text(size=16))+
  labs(fill = "")
dev.off()

# Hotspot convergence
# Few areas 

allhot <- dplyr::filter(su, grepl('Hotspot', su$`Bat hosts` ) & 
                          grepl('Hotspot', su$`Secondary hosts` )  &
                          grepl('Hotspot', su$`Human vulnerability`) & 
                          grepl('Hotspot', su$`Habitat modification` ))




table(su$`Bat hosts`, su$`Secondary hosts`)
table(su$`Bat hosts`, su$`Habitat modification`)
table(su$`Bat hosts`, su$`Human vulnerability`)
table(su$`Human vulnerability`, su$`Secondary hosts`)
table(su$`Human vulnerability`,su$`Habitat modification`)
table(su$`Secondary hosts`, su$`Habitat modification`)
table(su$`Secondary hosts`, su$`Human vulnerability`)

sumelt[sumelt$tile== allhot$tile, ]

nrow(allhot)

max(su$tile)

# Longitude variation 
png(filename = "stripes_hotspots_long.png", res=400, width = 22, height = 18, unit="cm")
ggplot(sumelt,aes(x=variable,y=x,fill=value))+
  facet_grid(~variable, scales="free")+
  geom_tile(color=NA)+ labs(x="Component",y="Longitude") + theme_bw()+
  scale_fill_manual(values = c("royalblue3", "violetred4","khaki" ))+
  theme(strip.background =element_rect(fill="snow2"),legend.position = "bottom",
        text=element_text(size=16))+
  labs(fill = "")
dev.off()

# Convergence of hotspots

su$`Human vulnerability` <- as.character(su$`Human vulnerability`)
su$`Bat hosts` <- as.character(su$`Bat hosts`)
su$`Secondary hosts` <- as.character(su$`Secondary hosts`)
su$`Habitat modification` <- as.character(su$`Habitat modification`)

su %>% filter("Bat hosts" == "Hotspot" &
                         "Secondary hosts" == "Hotspot" &
                         "Habitat modification" == "Hotspot"  &
                       "Human vulnerability" == "Neutral" )
  


write.table(file = 'hotspot_convergence.txt', allhot, row.names = FALSE)
#---------------------

###
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

# SLOW
#Conditional permutations added for comparative purposes; 
#permutations are over the whole data vector omitting the observation itself.

# Permutation
#localGvalues_sim <- localG_perm(info_spatial,  nsim=499, nb2listw(knn, style="B"), zero.policy = TRUE, alternative="two-sided")
#---------
par(mfrow=c(2,1))
hist(localGvalues)
hist(localGvalues_sim)

localGvalues <- round(localGvalues,3)

info_spatial <-  as.numeric(p[c])

localGvalues <- localG(x = info_spatial, listw = nb2listw(knn, style = "B"), zero.policy = TRUE, alternative="two-sided")
plot(localGvalues_sim ~ localGvalues)

#The value returned is a Z-value, and may be used as a diagnostic tool.
# High positive values indicate the posibility of a local cluster of high values of the variable being analysed, very low relative values a similar cluster of low values. For inference, 
# a Bonferroni-type test is suggested in the references, 
# where tables of critical values may be found 
#(see also details below).
#The critical values of the statistic under assumptions given in the references for the 95th percentile are for 
#n=1: 1.645, n=50: 3.083, n=100: 3.289, n=1000: 3.886.
#https://www.rdocumentation.org/packages/spdep/versions/1.2-4/topics/localG
# Create a new data frame that only includes the county fips codes and the G scores
