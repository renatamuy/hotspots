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


#-----------------------------

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
