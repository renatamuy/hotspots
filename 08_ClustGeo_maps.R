#ClustGeo Maps

require(tidyverse)
require(stringr)
require(reshape2)
require(rnaturalearth)
require(here)

setwd(here())
setwd('results')
setwd('trees_ward')
load('g_ward_clusters.RData')
c <- g
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




pal <- wesanderson::wes_palette("Moonrise3", length(unique(c$treeg12)), type = "continuous")

pc <- ggplot()+
  geom_tile(data = c, aes(y=y, x=x, fill = factor(treeg12) ))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent', col="black", size=0.50)  +
  theme(legend.title=element_blank(), legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  scale_fill_manual(values = pal ) +
  labs(x='Longitude', y="Latitude", title = "Clusters", subtitle = ''  ) 

pc

# Export
ggsave(
  'clusters_treeg12.png',
  plot = pc,
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)
