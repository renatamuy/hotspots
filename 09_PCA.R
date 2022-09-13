# Plot biplots with spatial and non-spatial clusters

library(tidyverse)
library(factoextra)
library(FactoMineR)

setwd(here())
setwd('results')
setwd('trees_ward')

load('g_ward_clusters.RData')

dfsub <- g %>% dplyr::select(  !c('x','y', contains("95")) )
dfsub <- dfsub %>% dplyr::select(  !c('ID', contains("tree")) )
colnames(dfsub)
justpca <- dfsub %>% dplyr::select( !c( 'hosts_muylaert',
                                        'hosts_sanchez',
                                               'trans',
                                               'pollution',
                                 'motor_travel_time_weiss'   ))    
colnames(justpca)

gpca <- PCA(justpca, graph = FALSE)


pal <- wesanderson::wes_palette("Moonrise3", 
                                length(unique(g$tree10)), type = "continuous")[1:length(unique(g$tree10))]

plot(gpca)

fviz_pca_ind(gpca,
             col.var="contrib",
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = factor(g$tree10), # color by groups
             palette = pal,
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups") #+
  #geom_text(aes(label = g$tree10))

fviz_pca_var(gpca, col.var="contrib")+
  scale_color_gradient2(low="gray", mid="blue",
                        high="red", midpoint=96) +
  theme_minimal()

# non-spatial 10
fviz_pca_biplot(gpca,
             label='var',
             habillage = factor(g$tree10), # color by groups
             palette = pal,
             #addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups") 

# spatial 10
fviz_pca_biplot(gpca,
                label='var',
                habillage = factor(g$treeg10), # color by groups
                palette = pal,
                #addEllipses = TRUE, # Concentration ellipses
                legend.title = "Groups") 

pal <- wesanderson::wes_palette("Moonrise3", 
                                length(unique(g$tree12)), type = "continuous")[1:length(unique(g$tree12))]


# non-spatial 12
fviz_pca_biplot(gpca,
                label='var',
                habillage = factor(g$tree12), # color by groups
                palette = pal,
                #addEllipses = TRUE, # Concentration ellipses
                legend.title = "Groups") 

# spatial 12
fviz_pca_biplot(gpca,
                label='var',
                habillage = factor(g$treeg12), # color by groups
                palette = pal,
                #addEllipses = TRUE, # Concentration ellipses
                legend.title = "Groups") 

# 

plot(g$tree10, g$tree12, pch=19)
cor.test(g$tree10, g$tree12, method= 'spearman')

plot(g$treeg10, g$treeg12, pch=19)
cor.test(g$treeg10, g$treeg12, method= 'spearman')

#-----------------------------



