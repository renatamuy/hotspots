#' ----
# Muylaert et al. 
# General package set
#' ----

# package list
pkg_list_cran <- c('rgeoda',
                    'sp', 
                    'spdep',
                    "here",
                    'gridExtra', 
                   "tidyverse",
                   "xlsx", 
                   "vroom",
                   "data.table",
                   "janitor",
                   "stringi",
                   "reshape2",
                   "DataExplorer",
                   "skimr",
                   "XML",
                   "rnaturalearth",
                   "rnaturalearthdata",
                   "spData",
                   "sf",
                   "raster",
                   "terra",
                   "ncdf4",
                   "tmap",
                   "ggmap",
                   "ggspatial",
                   "ggsn",
                   "ggbump",
                   "gghighlight",
                   "ggraph",
                   "ggridges",
                   "maps",
                   "mapdata",
                   "legendMap",
                   "rasterVis",
                   "leaflet",
                   "leafem",
                   "leaflet.opacity",
                   "htmlwidgets",
                   "htmltools",
                   "lattice",
                   "ggpubr",
                   "corrplot",
                   "classInt",
                   "graphlayouts",
                   "RColorBrewer",
                   "randomcoloR",
                   "viridis",
                   "wesanderson"       )

# install 
lapply(X = pkg_list_cran, 
       FUN = function(x) if(!require(x, character.only = TRUE)) install.packages(x, dep = TRUE, quiet = TRUE))

# packages from github
if(!require(scico)) devtools::install_github("thomasp85/scico")
if(!require(platexpress)) devtools::install_github("raim/platexpress")

# load 

require(stringr)
require(reshape2)
require(gridExtra)
require(tidyverse)
library(grid)
require(here)
require(ncdf4)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
require(RColorBrewer)
library(randomcoloR)
require(raster)
require(stringr)
require(rnaturalearth)
library(sf)
library(fasterize)
require(tidyverse)
library(gridExtra)
require(bivariatemaps)
library(classInt)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)




# end ---------------------------------------------------------------------