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


coordinates_countries <- data.frame(admin = target$admin,
                                    x=coordinates(target)[,1], 
                                    y= coordinates(target)[,2])

coordinates_countries[15,'x'] <- 107

timor <- data.frame( admin ='Timor East', x=125, y=-8.55)

mala <-  data.frame(admin ='Malaysia',x=102.87, y=4.18)

indo <-  data.frame(admin ='Indonesia',x=102.6, y=-1.26)
indo2 <-  data.frame(admin ='Indonesia',x=121.74, y=-2.77)

country_positions <- rbind(coordinates_countries, timor, mala, indo)

country_positions_sf <- st_as_sf(country_positions, 
                                 coords = c("x", "y"), 
                                 crs= crs(target))
country_positions$admin

nudge_x_list <- c(0,
                  6,
                  0, #Bhutan
                  0,
                  2, #Indonesia
                  0,
                  7, #Camboja
                  -1, #Laos
                  0,
                  -1,
                  -4, #Malasia k
                  -7, # Nepal
                  7, # Phillipines
                  -6,
                  9, #Vietnam
                  3,
                  3, # Malasia c
                  -2) # c(rep(6, 18))

nudge_y_list <- c(-10,
                  0,
                  2,
                  0, 
                  -6,
                  1,
                  9, # Camboja
                  2, #Laos
                  0,
                  -3,
                  6, # Malasia
                  -3, #Nepal
                  -6,#c(rep(-6, 18))
                  -6,
                  -7, #Vietnam
                  -9,
                  -4, # Malasia c
                  -6) 

pcone <- ggplot()+
  geom_tile(data = c, aes(y=y, x=x, fill = factor(cluster)))+
  theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent', col="black", linewidth=0.50)  +
  ggrepel::geom_text_repel(data = country_positions_sf, 
                           aes(x=country_positions[,2] , 
                               y= country_positions[,3], label = admin), 
                  size = 2,         
                  fontface = "italic",
                  force = 20,
                  box.padding = 0.3,
                  max.overlaps = 30,
                  point.padding = NA,
                  alpha = 0.7,
                  min.segment.length = 0.05,
                  segment.color = "black",
                  segment.size = 0.6,
                  seed = 1000,
                  nudge_x = nudge_x_list, 
                  nudge_y = nudge_y_list) +
    ggrepel::geom_label_repel(data = positions, 
                            aes(y=my, x=mx, label=factor(cluster) ), 
                            label.size = 0.1, alpha = 0.7)+
    theme(legend.title=element_blank(), 
        legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  scale_fill_manual(values = pal ) +
  labs(x='Longitude', y="Latitude", title = "Clusters", subtitle = ''  ) 

pcone


# Export
ggsave(
  'Figure_03a_Supplementary_Figure_08.png',
  plot = pcone,
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

#--------------------------------------------------------------------------------------------