# Correlation tests
# Figure 01
##### Stripes for characterizing hotspot gradients across space
# Correlation between covariates

require(stringr)
require(reshape2)
require(here)
require(gridExtra)
require(tidyverse)
library(grid)


setwd(here())
setwd('results')
dfgplot <- read.csv('gstar.csv' )

colnames(dfgplot)

# Correlation tests 
# Yes
cor.test(dfgplot$pollution, dfgplot$pop_2020_worldpop)
cor.test(dfgplot$builtup, dfgplot$trans)
cor.test(dfgplot$pop_2020_worldpop, dfgplot$trans)

# No
cor.test(dfgplot$agriharv, dfgplot$pop_2020_worldpop)
cor.test(dfgplot$pollution, dfgplot$lincomb_hosts)
cor.test(dfgplot$builtup, dfgplot$energy)
cor.test(dfgplot$builtup, dfgplot$pop_2020_worldpop)
cor.test(dfgplot$pop_2020_worldpop, dfgplot$energy)
cor.test(dfgplot$energy, dfgplot$trans)
cor.test(dfgplot$forest_integrity_grantham, dfgplot$hewson_forest_transition_potential)
cor.test(dfgplot$lincomb_hosts, dfgplot$mammals_iucn_mode)
cor.test(dfgplot$lincomb_hosts, dfgplot$pig_gilbert)
cor.test(dfgplot$lincomb_hosts, dfgplot$cattle_gilbert)
cor.test(dfgplot$motor_travel_time_weiss, dfgplot$pop_2020_worldpop)
cor.test(dfgplot$motor_travel_time_weiss, dfgplot$energy)
cor.test(dfgplot$motor_travel_time_weiss, dfgplot$builtup)


# similar layers presented in supplements
cor.test(dfgplot$bovliv, dfgplot$cattle_gilbert)
cor.test(dfgplot$mammals_iucn_mode, dfgplot$mmb)


# Plot melt panels

su <- dfgplot %>% dplyr::select(c('x','y', starts_with("col95")))

head(su)

colnames(su) <- sub('col95_', '', colnames(su))

tofocus <- colnames(su %>% dplyr::select(!c( 
                                               'trans',
                                               'pollution',
                                               'motor_travel_time_weiss'   ))    )


sumelt <- melt(su[,tofocus], id=c("x", "y"))

#-------------------------------------------------------------------

# Wildlife

pw <- sumelt %>% filter(variable == 'mmb' | variable == 'hosts'  ) %>% 
ggplot(aes( y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(case_when(variable == "hosts" ~ "Bat hosts",
                       variable == "mmb" ~ "Wild mammals") ~ . , ncol=3)+
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

head(sumelt)
unique(sumelt$variable)

pdomallbovidae <- sumelt %>% filter(variable == 'bovliv' | variable == 'cattle'  ) %>% 
  ggplot(aes(y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(case_when(variable == "cattle" ~ "Cattle",
                       variable == "bovliv" ~ "All domestic bovidae") ~ . , ncol=3)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4") ,
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude",
                                                     title = "Potential domestic secondary hosts") 

pdomallbovidae

# landscape change
#'trans', 'pollution'
want_landscape <- c('builtup', 'energy',  'agriharv', 
                    'forest_integrity_grantham',
                    'hewson_forest_transition_potential'                  )

# Factor reorder ---------------------------------------------
myorder <- c('hosts', 'mmb', 'mammals', want_landscape, 
             'pig',
             'cattle',
             "bovliv" ,
             'pop_2020_worldpop')

sumelt$variable <- factor(sumelt$variable, 
                          levels = myorder)


# --- All

allpp <- sumelt %>% 
  filter(variable %in% c('hosts', 'mmb', want_landscape, 'pig', 'cattle',
                         'pop_2020_worldpop')   )%>%  
  mutate(unilabs = fct_recode(variable,
                              'Bat hosts' = 'hosts',
                              'Wild mammals'= 'mmb',
                              'Pig' = 'pig',
                              'Cattle' = 'cattle',
                              "Human population" = 'pop_2020_worldpop' ,
                              "Built up area"    =   "builtup",
                              "Energy and mining" =  'energy',
                              'Agriculture and harvest' = 'agriharv',
                              'Forest integrity' = 'forest_integrity_grantham',
                              'Deforestation potential' ='hewson_forest_transition_potential' )) %>% 
  ggplot(aes( y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(~unilabs, nrow=2)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4") ,
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = 'none',
        strip.text = element_text(size = 14)) + 
  labs(x='Longitude', y="Latitude") 



allpp

setwd(here())
setwd('results')

#ggsave('Figure_01a.png',
  #     plot = last_plot(),
 #      dpi = 400,
  #     width = 14,
  #     height = 6,
  #     limitsize = TRUE)

write.table(file='table_pct.txt', row.names = FALSE,
            round(table(sumelt$variable, sumelt$value)/ nrow(dfgplot)*100, digits=2) )

# Percentages per indicator
unique(sumelt$variable)

plotpcts <- sumelt %>%  filter(variable %in% c('hosts', 'mmb',
                                               want_landscape,
                                               'pig', 'cattle',
                                               'pop_2020_worldpop')  ) %>% 
  mutate(unilabs = fct_recode(variable,
                              'Bat hosts' = 'hosts',
                              'Wild mammals'= 'mmb',
                              'Pig' = 'pig',
                              'Cattle' = 'cattle',
                              "Built up area"    =   "builtup",
                              "Energy and mining" =  'energy',
                              'Agriculture and harvest land' = 'agriharv',
                              'Forest integrity' = 'forest_integrity_grantham',
                              'Deforestation potential' ='hewson_forest_transition_potential',
                              "Human population" = 'pop_2020_worldpop' )) %>% 
  mutate(highlevel = fct_recode(variable,
                              'Wildlife - potential hosts' = 'hosts',
                              'Wildlife - potential hosts'= 'mmb',
                              'Domesticated - potential hosts' = 'pig',
                              'Domesticated - potential hosts' = 'cattle',
                              "Landscape change"    =   "builtup",
                              "Landscape change" =  'energy',
                              'Landscape change' = 'agriharv',
                              'Landscape change' = 'forest_integrity_grantham',
                              'Landscape change' ='hewson_forest_transition_potential',
                              "Exposure in humans" = 'pop_2020_worldpop')) %>% 
  ggplot(aes(unilabs  )) + 
  facet_grid(~highlevel) +
  geom_bar(aes(fill= value), alpha=0.8,position="fill") +
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4" ) ,
                    labels = c("Neutral", "Coldspot", "Hotspot"))+
  theme_minimal(base_size = 15) + geom_rug() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom") + coord_flip() + 
  labs( title = "", x = "", y = "Area (%)", caption = "") 

  
plotpcts

# Exporting 
ggsave('Figure_01.png',
  plot = grid.arrange(allpp, plotpcts, nrow = 2), #last_plot()
  dpi = 400,
  width = 12,
  height = 11,
  limitsize = TRUE)

#------------ Bovidae Livestock

allppb <- sumelt %>% 
  filter(variable %in% c('hosts', 'mmb', want_landscape, 'pig', 'bovliv',
                         'pop_2020_worldpop')   )%>%  
  mutate(unilabs = fct_recode(variable,
                              'Bat hosts' = 'hosts',
                              'Wild mammals'= 'mmb',
                              'Pig' = 'pig',
                              'Bovidae livestock' = 'bovliv',
                              "Human population" = 'pop_2020_worldpop' ,
                              "Built up area"    =   "builtup",
                              "Energy and mining" =  'energy',
                              'Agriculture and harvest' = 'agriharv',
                              'Forest integrity' = 'forest_integrity_grantham',
                              'Deforestation potential' ='hewson_forest_transition_potential' )) %>% 
  ggplot(aes( y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(~unilabs, nrow=2)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4") ,
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = 'none',
        strip.text = element_text(size = 14)) + 
  labs(x='Longitude', y="Latitude") 



allppb

setwd(here())
setwd('results')

write.table(file='table_pct_bovliv.txt', row.names = FALSE,
            round(table(sumelt$variable, sumelt$value)/ nrow(dfgplot)*100, digits=2) )

# Percentages per indicator
unique(sumelt$variable)

plotpctsb <- sumelt %>%  filter(variable %in% c('hosts', 'mmb',
                                               want_landscape,
                                               'pig', 'bovliv',
                                               'pop_2020_worldpop')  ) %>% 
  mutate(unilabs = fct_recode(variable,
                              'Bat hosts' = 'hosts',
                              'Wild mammals'= 'mmb',
                              'Pig' = 'pig',
                              'Bovidae livestock' = 'bovliv',
                              "Built up area"    =   "builtup",
                              "Energy and mining" =  'energy',
                              'Agriculture and harvest land' = 'agriharv',
                              'Forest integrity' = 'forest_integrity_grantham',
                              'Deforestation potential' ='hewson_forest_transition_potential',
                              "Human population" = 'pop_2020_worldpop' )) %>% 
  mutate(highlevel = fct_recode(variable,
                                'Wildlife - potential hosts' = 'hosts',
                                'Wildlife - potential hosts'= 'mmb',
                                'Domesticated - potential hosts' = 'pig',
                                'Domesticated - potential hosts' = 'cattle',
                                "Landscape change"    =   "builtup",
                                "Landscape change" =  'energy',
                                'Landscape change' = 'agriharv',
                                'Landscape change' = 'forest_integrity_grantham',
                                'Landscape change' ='hewson_forest_transition_potential',
                                "Exposure in humans" = 'pop_2020_worldpop')) %>% 
  ggplot(aes(unilabs  )) + 
  facet_grid(~highlevel) +
  geom_bar(aes(fill= value), alpha=0.8,position="fill") +
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4" ) ,
                    labels = c("Neutral", "Coldspot", "Hotspot"))+
  theme_minimal(base_size = 15) + geom_rug() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom") + coord_flip() + 
  labs( title = "", x = "", y = "Area (%)", caption = "") 


plotpcts

# Exporting 

ggsave('Figure_01_bovliv.png',
       plot = grid.arrange(allppb, plotpctsb, nrow = 2), #last_plot()
       dpi = 400,
       width = 12,
       height = 11,
       limitsize = TRUE)


#------------------------------------------------------------------
# Individual values - Supplements

sumelt$tile <- rep(1:nrow(su))

# Convergence of hotspots
# Hotspot convergence
# Few areas 

su$tile <- 1:nrow(su)

allhot <- dplyr::filter(su, 
                        grepl('violetred4', su$`hosts` ) & 
                        grepl('violetred4', su$`mmb` )  &
                        grepl('violetred4', su$`pig`) & 
                        grepl('violetred4', su$`cattle` ) &
                          grepl('violetred4', su$`builtup` ) &
                         grepl('violetred4', su$`energy` ) &
                         grepl('violetred4', su$`agriharv` ) &
                           grepl('violetred4', su$`forest_integrity_grantham` ) &
                           grepl('violetred4', su$`hewson_forest_transition_potential` ) 
                                         )
allhot


allhot_mammals <- dplyr::filter(su, 
                                grepl('violetred4', su$`hosts` ) & 
                                  grepl('violetred4', su$`mmb` )  &
                                  grepl('violetred4', su$`pig`) & 
                                  grepl('violetred4', su$`cattle` ) )


allhot_mammals


allhot_mammals_all <- dplyr::filter(su, 
                                grepl('violetred4', su$`hosts` ) & 
                                  grepl('violetred4', su$`mammals` )  &
                                  grepl('violetred4', su$`pig`) & 
                                  grepl('violetred4', su$`cattle` ) )


# -------------------
# all host layers
allhot_mmb <- dplyr::filter(su, 
                                 grepl('violetred4', su$`hosts` ) & 
                                   grepl('violetred4', su$`mmb` )  &
                                   grepl('violetred4', su$`pig`) & 
                                   grepl('violetred4', su$`cattle` ) )

# 

allhot_mammalsb <- dplyr::filter(su, 
                                grepl('violetred4', su$`hosts` ) & 
                                  grepl('violetred4', su$`mmb` )  &
                                  grepl('violetred4', su$`pig`) & 
                                  grepl('violetred4', su$`bovliv` ) )


nrow(distinct(allhot_mammalsb[, c('x','y')]))

nrow(distinct(allhot_mammals_all[, c('x','y')]))

nrow(distinct(allhot_mmb[, c('x','y')]))

write.csv(file = 'hotspot_convergence_mammals.csv', allhot_mmb, row.names = FALSE)

write.csv(file = 'hotspot_convergence_mammals_bovidae.csv', allhot_mammalsb, row.names = FALSE)


#--------------------------------------------------------