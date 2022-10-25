#' ----
# General package set
#' ----

# extra 
require(ncdf4)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
require(RColorBrewer)
library(randomcoloR)
require(raster)
require(stringr)
require(rnaturalearth)
require(tidyverse)
require(reshape2)
require(here)
library(gridExtra)
library(grid)
require(bivariatemaps)
library(classInt)
library(rgdal)
library(dismo)
library(XML)
library(maps)
library(sp)

# package list
pkg_list_cran <- c('rgeoda','ClustGeo',
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
if(!require(Manu)) devtools::install_github("G-Thomson/Manu")

# end ---------------------------------------------------------------------