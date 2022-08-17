# Maps from skater

require(tidyverse)
require(stringr)
require(reshape2)

setwd(here())
setwd('results')

c <- read.csv('clusters_rgeoda_c20_random25796.csv')

pal <- wesanderson::wes_palette("FantasticFox1", length(unique(c$cluster)), type = "continuous")

pc <- ggplot(data = c, aes(y=y, x=x, fill = factor(cluster) ))+
  geom_tile()+ theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',  
        strip.text = element_text(size = 14)) +
  scale_fill_manual(values = pal ) +
  labs(x='Longitude', y="Latitude", title = "Clusters", subtitle = ''  ) 

ggsave(
  'clusters.png',
  plot = pc,
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

# Other Colour schemes:
#BottleRocket1, BottleRocket2, Rushmore1, Royal1, Royal2, Zissou1, 
#Darjeeling1, Darjeeling2, Chevalier1 , FantasticFox1 ,
#Moonrise1, Moonrise2, Moonrise3, Cavalcanti1, GrandBudapest1, GrandBudapest2, 
#IsleofDogs1, IsleofDogs2
