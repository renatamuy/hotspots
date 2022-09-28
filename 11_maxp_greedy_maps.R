#--------------------------------------------------------------
# Maps


setwd(here())
setwd('results/maxp')

require(tidyverse)
require(stringr)
require(reshape2)
require(rnaturalearth)
require(here)

load('prepdf_maxp.RData')

length(unique(d$maxp5pct))

c <- d

target <- ne_countries(type = "countries", country = c('Bangladesh',
                                                       'Bhutan',
                                                       'Brunei',
                                                       'Cambodia',
                                                       'China', 
                                                       'India',
                                                       'Indonesia',
                                                       'Laos',
                                                       'Malaysia',
                                                       'Myanmar',
                                                       'Nepal',
                                                       'Philippines',
                                                       'Singapore', 
                                                       'Sri Lanka',
                                                       'Thailand',
                                                       'Timor-Leste',
                                                       'Vietnam'))




pal <- wesanderson::wes_palette("Moonrise3", length(unique(c$maxp5pct)), type = "continuous")
pal <- topo.colors(19)
pc <- ggplot()+
  geom_tile(data = c, aes(y=y, x=x, fill = factor(maxp5pct) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent', col="black", size=0.50)  +
  theme(legend.title=element_blank(), legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  scale_fill_manual(values = pal ) +
  labs(x='Longitude', y="Latitude", title = "Clusters", subtitle = ''  ) 

pc


# Export
ggsave(
  'clusters_maxp5pct.png',
  plot = pc,
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)


#
require(RColorBrewer)
pal <- brewer.pal(9, 'Spectral')

pal <- wesanderson::wes_palette("Moonrise3", length(unique(c$maxp10pct)), type = "continuous")

pc10 <- ggplot()+
  geom_tile(data = c, aes(y=y, x=x, fill = factor(maxp10pct) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent', col="black", size=0.50)  +
  theme(legend.title=element_blank(), legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  scale_fill_manual(values = pal ) +
  labs(x='Longitude', y="Latitude", title = "Clusters", subtitle = ''  ) 

pc10

length(unique(d$maxp10pct))

ggsave(
  'clusters_maxp10pct.png',
  plot = pc10,
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)


#-----------------------------------------