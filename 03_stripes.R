### Stripes for characterizing hotspot gradients across space

require(tidyverse)
require(stringr)
require(reshape2)
require(here)

setwd(here())
setwd('results/hotspots_region')
dfgplot <- read.csv('gstar_1.645.csv' )

colnames(dfgplot)

# Plot melt panels

su <- dfgplot %>% select(c('x','y',starts_with("col95")))

head(su)

colnames(su) <- sub('col95_', '', colnames(su))

sumelt <- melt(su, id=c("x", "y"))

colnames(sumelt)

#-------------------------------------------------------------------

# Wildlife

pw <- sumelt %>% filter(variable == 'mammals' | variable == 'hosts'  ) %>% 
ggplot(aes( y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(case_when(variable == "hosts" ~ "Bat hosts",
                       variable == "mammals" ~ "All mammals") ~ . , ncol=3)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4"), 
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude",
      
                                                                                                    title = "Wildlife indicators") 
pw

# Potential domestic secondary hosts
pdom <- sumelt %>% filter(variable == 'pig' | variable == 'cattle'  ) %>% 
  ggplot(aes(y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(case_when(variable == "pig" ~ "Pig",
                       variable == "cattle" ~ "Cattle") ~ . , ncol=3)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4") ,
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude",
                                                     title = "Potential domestic secondary hosts") 

pdom

# Human dimension


phum <- sumelt %>% filter(variable == 'pop_2020_worldpop' | variable == 'motor_travel_time_weiss'  ) %>% 
  ggplot(aes(y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(case_when(variable == "pop_2020_worldpop" ~ "Human population",
                       variable == "motor_travel_time_weiss" ~ "Time to reach healthcare") ~ . , ncol=3)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4") ,
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude",
                  title = "Indcators of exposure and spread in humans") 

phum

# landscape change
want_landscape <- c('builtup', 'energy', 'trans', 'agriharv', 'pollution',
                    'forest_integrity_grantham',
                    'hewson_forest_transition_potential'                  )

pland <- sumelt %>% filter(variable %in% want_landscape  ) %>% 
  ggplot(aes(y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(case_when(variable == 'builtup' ~ "Built up area",
                       variable == 'energy' ~ "Energy-related structures",
                       variable == 'trans' ~ 'Transport-related structures',
                       variable == 'pollution' ~ 'Human intrusion and pollution',
                       variable == 'agriharv' ~ 'Agriculture and harvest land',
                       variable == 'forest_integrity_grantham' ~ 'Forest integrity',
                       variable == 'hewson_forest_transition_potential' ~'Deforestation potential') ~ . , ncol=3)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4") ,
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude",
            title = "Landscape change indicators") 
pland

#
library(grid)
setwd(here())
dir.create('results_1.645')
setwd('results_1.645')

ggsave(
  'hotspots_univariate_bio.png',
  plot = grid.arrange(pw, pdom, phum, nrow = 3),
  dpi = 400,
  width = 6.5,
  height = 13,
  limitsize = TRUE)

# Landscape change 
ggsave(
  'hotspots_univariate_pland.png',
  plot = pland,
  dpi = 400,
  width = 10,
  height = 10,
  limitsize = TRUE)

# --- All

ggplot(data = sumelt, aes( y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(~variable, ncol=3)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4") ,
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude") 

ggsave('hotspots_facets.png',
       plot = last_plot(),
       dpi = 400,
       width = 8,
       height = 14,
       limitsize = TRUE)

write.table(file='table_pct.txt', row.names = FALSE,
            round(table(sumelt$variable, sumelt$value)/ nrow(dfgplot)*100, digits=2) )

# Percentages per indicator
sumelt %>%
  mutate(unilabs = fct_recode(variable,
                              'Bat hosts' = 'hosts',
                              'All mammals'= 'mammals',
                              'Pig' = 'pig',
                              'Cattle' = 'cattle',
                              "Human population" = 'pop_2020_worldpop' ,
                              "Time to reach healthcare" = "motor_travel_time_weiss",
                              "Built up area"    =   "builtup",
                              "Energy-related structures" =  'energy',
                              'Transport-related structures' = 'trans', 
                              'Human intrusion and pollution' = 'pollution',
                              'Agriculture and harvest land' = 'agriharv',
                              'Forest integrity' = 'forest_integrity_grantham',
                              'Deforestation potential' ='hewson_forest_transition_potential' )) %>% 
  mutate(highlevel = fct_recode(variable,
                              'Wildlife - potential hosts' = 'hosts',
                              'Wildlife - potential hosts'= 'mammals',
                              'Domesticated - potential hosts' = 'pig',
                              'Domesticated - potential hosts' = 'cattle',
                              "Exposure and spread in humans" = 'pop_2020_worldpop' ,
                              "Exposure and spread in humans" = "motor_travel_time_weiss",
                              "Landscape change"    =   "builtup",
                              "Landscape change" =  'energy',
                              'Landscape change' = 'trans', 
                              'Landscape change' = 'pollution',
                              'Landscape change' = 'agriharv',
                              'Landscape change' = 'forest_integrity_grantham',
                              'Landscape change' ='hewson_forest_transition_potential' )) %>% 
  ggplot(aes(unilabs  )) + 
  facet_grid(~highlevel)+
  geom_bar(aes(fill= value), alpha=0.8,position="fill") +
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4" ) ,
                    labels = c("Neutral", "Coldspot", "Hotspot"))+
  theme_minimal(base_size = 15) + geom_rug() +
  theme( legend.title = element_blank(), legend.position = "bottom") + coord_flip() + 
  labs( title = "", x = "", y = "Area (%)", caption = "") 

ggsave(
  'hotspots_all_pct_geom_bar.png',
  plot = last_plot(),
  dpi = 400,
  width = 13,
  height = 6,
  limitsize = TRUE)

#------------------------------------------------------------------
# Individual values

sumelt$tile <- rep(1:nrow(su))


# Latitude variation 

png(filename= "stripes_hotspots_lat.png", res = 400, 
    width = 22, height = 16, unit="cm")
ggplot(sumelt,aes(x=variable,y=y,fill=value))+
  geom_tile(color=NA)+ labs(x="Component",y="Latitude") + theme_bw()+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4" ) ,
                    labels = c("Neutral", "Coldspot", "Hotspot"))+
  theme(strip.background = element_rect(fill="snow2"),legend.position = "top",
        text=element_text(size=16))+guides(x =  guide_axis(angle = 45))+
  labs(fill = "") 
dev.off()

# Longitude variation 
png(filename= "stripes_hotspots_long_flip.png", res = 400, 
    width = 26, height = 16, unit="cm")
ggplot(sumelt,aes(x=variable,y=x,fill=value))+
  geom_tile(color=NA)+ labs(x="Component",y="Longitude") + theme_bw()+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4" ) ,
                    labels = c("Neutral", "Coldspot", "Hotspot"))+
  theme(strip.background = element_rect(fill="snow2"),legend.position = "top",
        text=element_text(size=16))+guides(x =  guide_axis(angle = 45))+
  labs(fill = "") + coord_flip()
dev.off()

# Convergence of hotspots

# Hotspot convergence
# Few areas 

want_risk3

su$tile <- 1:nrow(su)
allhot <- dplyr::filter(su, 
                        grepl('violetred4', su$`hosts` ) & 
                        grepl('violetred4', su$`mammals` )  &
                        grepl('violetred4', su$`pig`) & 
                        grepl('violetred4', su$`cattle` ) &
                         # grepl('violetred4', su$`builtup` ) &
                          #grepl('violetred4', su$`energy` ) &
                          #grepl('violetred4', su$`trans` ) &
                          #grepl('violetred4', su$`agriharv` ) &
                          #grepl('violetred4', su$`pollution` ) &
                           grepl('violetred4', su$`forest_integrity_grantham` ) &
                           grepl('violetred4', su$`hewson_forest_transition_potential` ) 
                                         )

allhot_mammals <- dplyr::filter(su, 
                                grepl('violetred4', su$`hosts` ) & 
                                  grepl('violetred4', su$`mammals` )  &
                                  grepl('violetred4', su$`pig`) & 
                                  grepl('violetred4', su$`cattle` ) )

  
allhot_mammals
sumelt[sumelt$tile == allhot_mammals$tile, ]

#   grepl('violetred4', su$`pop_2020_worldpop` ) 
#             grepl('violetred4', su$`motor_travel_time_weiss` )
sumelt[sumelt$tile == allhot$tile, ]
su$`Human vulnerability` <- as.character(su$`Human vulnerability`)
su$`Bat hosts` <- as.character(su$`Bat hosts`)
su$`Secondary hosts` <- as.character(su$`Secondary hosts`)
su$`Habitat modification` <- as.character(su$`Habitat modification`)

# java convergence only happened with those averaged, but I don't think it is a good idea to 
# do it like that
#su %>% filter("Bat hosts" == "Hotspot" &
#                "Secondary hosts" == "Hotspot" &
#               "Habitat modification" == "Hotspot"  &
#                "Human vulnerability" == "Neutral" )

write.table(file = 'hotspot_convergence.txt', allhot, row.names = FALSE)
