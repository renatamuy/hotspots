#-----------------------------------------------------------------------------------------
# Script maps in R 

library(here)

source('00_packages.R')

# Set working dir
#setwd('data/China_raw/spatial_match/')

c <- shapefile('spatial_match.shp')

worldmap <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Map building

map.fort <- fortify(c, region = "ADM2_EN") 

c@data$id <- c@data$ADM2_EN

map.fort <- left_join(map.fort, c@data,  by = "id")

# Base plot 
m <- ggplot(data = c, aes(x = long, y = lat, group = group))

# Selection for label 

muns <- 'Shanghai Municipality'

label.selected <- map.fort[ map.fort$'ADM2_EN' %in% muns, ] 

# get just one lat long per id in fortified shp

label.selectedd <- distinct(label.selected,id, .keep_all = TRUE)

# Creating fih

fig <- m + geom_polygon(aes(fill = pop_2020), color = 'gray', size = 0.06) + # fill is pop in this case
    theme_bw() +
    scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") + ##col.range = c(0, 12)  # only if you want the palettes to be the same range of values
    coord_fixed(1.1) + # proportionality
    theme(legend.justification=c(0,0), 
          legend.position= c(0.02,0.02),
          legend.key.size = unit(0.3, 'cm'))+ 
    guides(fill=guide_colorbar(title=c("Population"))) +
    ggsn::scalebar(map.fort, dist = 500,location = "bottomright", 
                   transform = TRUE, dist_unit = "km",
                   st.dist = 0.02, st.size = 2, model = 'WGS84') +
    ggsn::north(map.fort, scale = .08) +
  ylab('Latitude') +xlab('Longitude') +
    ggrepel::geom_text_repel(data=label.selectedd, aes(x=long, y=lat, label=ADM2_EN), size=2) 

fig

# Exporting figure -----------------------------------------------------

figname <- paste0("Fig_example",".png")

ggsave(filename = figname, 
       width = 18, 
       height = 14, 
       units = "cm", 
       dpi = 600, 
       gridExtra::grid.arrange( fig ))

#---------------------------------------------------------------------------------------------