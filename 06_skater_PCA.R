# Supplements
# Plot biplots with spatial clusters

library(tidyverse)
library(factoextra)
library(FactoMineR)

setwd(here())
setwd('results')
setwd('skater_optimal_cluster_size_19')

g <- read.csv('clusters_rgeoda_c19.csv')

dfsub <- g %>% dplyr::select(  !c('x','y', contains("95")) )

justpca <- dfsub %>% dplyr::select( !c( 'hosts_muylaert',
                                        'hosts_sanchez',
                                        'bovliv',
                                        "mammals_iucn_mode",
                                               'trans',
                                               'pollution',
                                 'motor_travel_time_weiss' ,
                                 'cluster'  ))    
colnames(justpca)

colnames(justpca) <- c('Builtup',
                       'Mining and energy',
                       'Agriculture and harvest',
                       'Forest quality',
                       'Forest loss risk',
                       'Pigs', 
                       'Cattle',
                       'Mammals',
                       'Human population',
                       'Bat hosts') 


gpca <- PCA(justpca, graph = FALSE)

pal <- wesanderson::wes_palette("Moonrise3", 
                                length(unique(g$cluster)), 
                                type = "continuous")[1:length(unique(g$cluster))]

# Plots
fviz_pca_ind(gpca,
             col.var="contrib",
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = factor(g$cluster), # color by groups
             palette = pal,
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups") #+

fviz_pca_var(gpca, col.var="contrib")+
  scale_color_gradient2(low="gray", mid="blue",
                        high="red", midpoint=96) +
  theme_minimal()

# non-spatial 10
fviz_pca_biplot(gpca,
             label='var',
             habillage = factor(g$cluster), # color by groups
             palette = pal,
             #addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups") 

# skater 
fviz_pca_biplot(gpca,
                label='var',
                habillage = factor(g$cluster), # color by groups
                palette = pal,
                #addEllipses = TRUE, # Concentration ellipses
                legend.title = "Groups") 



# Ternary map

require(ggtern)

nrow(gpca$eig)

summary(gpca$ind$contrib)
summary(gpca$ind$coord)


g$time_access_healthcare <- 100*(g$motor_travel_time_weiss-min(g$motor_travel_time_weiss))/
  (max(g$motor_travel_time_weiss)-min(g$motor_travel_time_weiss))

g$Dim1_anthropogenic <- 100*( gpca$ind$coord[,1]-min( gpca$ind$coord[,1]))/
  (max( gpca$ind$coord[,1])-min( gpca$ind$coord[,1]))

g$Dim2_natural <- 100*( gpca$ind$coord[,2]-min( gpca$ind$coord[,2]))/
  (max( gpca$ind$coord[,2])-min( gpca$ind$coord[,2]))

summary(g$Dim2_natural)*100

gt <- g

ggtern(gt,aes( time_access_healthcare, Dim2_natural, Dim1_anthropogenic)) +
  #stat_confidence_tern(geom='polygon',aes(fill=..level..), color='white') +
  scale_color_gradient(low='green',high='red') +
  geom_mask() +  
  geom_point()+
  theme_showarrows() + 
  facet_wrap(~cluster)+
  #limit_tern(.5,1,.5)+
  #geom_point_swap(aes(colour=Dim1_anthropogenic, shape=cluster),fill='black',size=5) +
  #scale_shape_manual(values=c(21)) +
  labs(title="") #color="Temperature",fill='Confidence')

#
#
#

#-----------------------------bovliv


setwd(here())
setwd('results/skater_optimal_cluster_size_19_bovliv')

g <- read.csv('clusters_rgeoda_c19.csv')

dfsub <- g %>% dplyr::select(  !c('x','y', contains("95")) )

justpca <- dfsub %>% dplyr::select( !c( 'ID', 'hosts_muylaert',
                                        'hosts_sanchez',
                                        'cattle_gilbert',
                                        "mammals_iucn_mode",
                                        'trans',
                                        'pollution',
                                        'motor_travel_time_weiss' ,
                                        'cluster'  ))    
colnames(justpca)

colnames(justpca) <- c('Builtup',
                       'Mining and energy',
                       'Agriculture and harvest',
                       'Forest quality',
                       'Forest loss risk',
                       'Pigs', 
                       'Mammals',
                       'Cattle',
                       'Human population',
                       'Bat hosts') 


gpca <- PCA(justpca, graph = FALSE)

pal <- wesanderson::wes_palette("Moonrise3", 
                                length(unique(g$cluster)), 
                                type = "continuous")[1:length(unique(g$cluster))]

# Plots
fviz_pca_ind(gpca,
             col.var="contrib",
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = factor(g$cluster), # color by groups
             palette = pal,
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups") #+

fviz_pca_var(gpca, col.var="contrib")+
  scale_color_gradient2(low="gray", mid="blue",
                        high="red", midpoint=96) +
  theme_minimal()

# non-spatial 10
fviz_pca_biplot(gpca,
                label='var',
                habillage = factor(g$cluster), # color by groups
                palette = pal,
                #addEllipses = TRUE, # Concentration ellipses
                legend.title = "Groups") 

# skater 12

fviz_pca_biplot(gpca,
                label='var',
                habillage = factor(g$cluster), # color by groups
                palette = pal,
                #addEllipses = TRUE, # Concentration ellipses
                legend.title = "Groups") 

#-----------------------------


