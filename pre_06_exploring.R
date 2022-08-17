# Data exploring

require('DataExplorer')
require(raster)
require(here)
require(tidyverse)
require(xlsx)
# read prepared rasters

setwd(here())
setwd('data/region/')

# Read all prepared rasters for the region of intest

quero <- list.files(pattern = '.tif')

all <- stack(list.files(pattern = '.tif')) # TEST SCRIPT

d <- data.frame(rasterToPoints(all) )

introduce(d)

head(d)

plot_correlation(na.omit(d), maxcat = 5L)

create_report(d, y = "hosts_muylaert")

# raster correlation 

ENMTools::raster.cor.matrix(all, method = "pearson")
ENMTools::raster.cor.plot(all)

# vif
env_li_vif <- usdm::vif(env_li)
env_li_vif

# vifstep
env_li_vifstep <- usdm::vifstep(env_li, th = 2)
env_li_vifstep

# vifcor
env_li_vifcor <- usdm::vifcor(env_li, th = .7)
env_li_vifcor

# select
env_li_vif <- usdm::exclude(env_li, env_li_vifstep)
env_li_vif

env_li_cor <- usdm::exclude(env_li, env_li_vifcor)
env_li_cor

# scale ----
env_li_vif_scale <- raster::scale(env_li_vif)
env_li_vif_scale

# plot
plot(env_li_vif, col = viridis::viridis(100))
plot(env_li_vif_scale, col = viridis::viridis(100))

# export ----

raster::writeRaster(x = env_li_vif_scale,
                    filename = paste0("03_dados/02_variaveis/", names(env_li_vif_scale)),
                    bylayer = TRUE,
                    format = "GTiff",
                    overwrite = TRUE)

# end ---------------------------------------------------------------------




