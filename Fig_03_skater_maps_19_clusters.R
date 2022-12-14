# Maps from skater

require(tidyverse)
require(stringr)
require(reshape2)
require(rnaturalearth)
require(here)

setwd(here())
setwd('results/skater_optimal_cluster_size_19')

c <- read.csv('clusters_rgeoda_c19.csv')

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

pal <- wesanderson::wes_palette("Moonrise3", length(unique(c$cluster)), type = "continuous")

pc <- ggplot()+
  geom_tile(data = c, aes(y=y, x=x, fill = factor(cluster) ))+
  theme_bw() +
  facet_wrap(~cluster, nrow = 4, ncol=5)+
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent', col="black", size=0.50)  +
     theme(legend.title=element_blank(), 
           legend.position = 'right',  
        strip.text = element_text(size = 14)) +
  scale_fill_manual(values = pal ) +
  labs(x='Longitude', y="Latitude", title = "Clusters", subtitle = ''  ) 

pc

ggsave(
  'clusters_countries.png',
  plot = pc,
  dpi = 400,
  width = 16,
  height = 13,
  limitsize = TRUE)

# All in one panel -----------------------------------------------------------

positions <- c %>%
  group_by(cluster) %>% 
  #summarise(mx = sample(x,1), my = sample(y,1))
  summarise(mx = median(x), my = median(y))# + 1

pcone <- ggplot()+
  geom_tile(data = c, aes(y=y, x=x, fill = factor(cluster)))+
  theme_bw() +
  #facet_wrap(~cluster, nrow = 4, ncol=5)+
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent', col="black", size=0.50)  +
  #geom_text(data = positions, aes(y=my, x=mx, label=factor(cluster) ))+
  #geom_label(data = positions, aes(y=my, x=mx, label=factor(cluster) ), label.size = 0.4, alpha = 1)+
  ggrepel::geom_label_repel(data = positions, aes(y=my, x=mx, label=factor(cluster) ), label.size = 0.1, alpha = 0.7)+
  theme(legend.title=element_blank(), 
        legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  scale_fill_manual(values = pal ) +
  labs(x='Longitude', y="Latitude", title = "Clusters", subtitle = ''  ) 

pcone

ggsave(
  'Figure_03a.png',
  plot = pcone,
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

# Other Colour schemes:
#BottleRocket1, BottleRocket2, Rushmore1, Royal1, Royal2, Zissou1, Darjeeling1, Darjeeling2, Chevalier1 , FantasticFox1 ,
#Moonrise1, Moonrise2, Moonrise3, Cavalcanti1, GrandBudapest1, GrandBudapest2, IsleofDogs1, IsleofDogs2
#--------------------------------------------------------------------------------------------