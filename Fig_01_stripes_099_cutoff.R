# Muylaert et al.
# Figure Supplements
# Stripes for characterizing hotspot gradients across space

rm(list = ls())

library(here)
setwd(here())

source('00_packages.R')

setwd('results')
dfgplot <- read.csv('gstar_099.csv' )

colnames(dfgplot)

# similar layers presented in supplements
cor.test(dfgplot$bovliv, dfgplot$cattle_gilbert)

# Plot melt panels

su <- dfgplot %>% dplyr::select(c('x','y', starts_with("col99")))

head(su)

colnames(su) <- sub('col99_', '', colnames(su))

tofocus <- colnames(su %>% dplyr::select(!c( 
  'trans',
  'pollution',
  'motor_travel_time_weiss'   ))    )


sumelt <- melt(su[,tofocus], id=c("x", "y"))

#-------------------------------------------------------------------

# Wildlife
sumelt$valuef <- factor(sumelt$value, levels =  c("royalblue3" ,"khaki"  ,    "violetred4"))
valuesfac <- c("royalblue3", "khaki", "violetred4")
labelsfac  <- c("Coldspot", "Intermediate", "Hotspot")

pw <- sumelt %>% filter(variable == 'mmb' | variable == 'hosts'  ) %>% 
  ggplot(aes( y=y, x=x, fill = valuef))+
  geom_tile()+
  facet_wrap(case_when(variable == "hosts" ~ "Bat hosts",
                       variable == "mmb" ~ "Wild mammals") ~ . , ncol=3)+
  scale_fill_manual(values =valuesfac, 
                    labels = labelsfac) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude",
                                                     
                                                     title = "Wildlife indicators") 
pw

# Potential domestic secondary hosts
unique(sumelt$value)

pdom <- sumelt %>% 
  filter(variable == 'pig' | variable == 'cattle'  ) %>% 
  ggplot(aes(y=y, x=x, fill = valuef))+
  geom_tile()+
  facet_wrap(case_when(variable == "pig" ~ "Pig",
                       variable == "cattle" ~ "Cattle") ~ . , ncol=3)+
  scale_fill_manual(values =c("royalblue3", "khaki","violetred4") ,
                    labels = c( "Coldspot","Intermediate", "Hotspot")) + theme_bw() +
  theme(legend.title=element_blank(), legend.position = 'bottom',
        strip.text = element_text(size = 14)) + labs(x='Longitude', y="Latitude",
                                                     title = "Potential domestic secondary hosts") 

pdom


pdomallbovidae <- sumelt %>% filter(variable == 'bovliv' | variable == 'cattle'  ) %>% 
  ggplot(aes(y=y, x=x, fill = valuef))+
  geom_tile()+
  facet_wrap(case_when(variable == "cattle" ~ "Cattle",
                       variable == "bovliv" ~ "All domestic bovidae") ~ . , ncol=3)+
  scale_fill_manual(values =c("royalblue3", "khaki","violetred4") ,
                    labels = c( "Coldspot","Intermediate", "Hotspot")) + theme_bw() +
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
                              'Pig' = 'pig',
                              'Cattle' = 'cattle',
                              'Wild mammals'= 'mmb',
                              "Human population" = 'pop_2020_worldpop' ,
                              "Built up area"    =   "builtup",
                              "Energy and mining" =  'energy',
                              'Agriculture and harvest' = 'agriharv',
                              'Forest integrity' = 'forest_integrity_grantham',
                              'Deforestation potential' ='hewson_forest_transition_potential' )) %>% 
  mutate(unilabs = fct_relevel(unilabs, 
                               'Bat hosts',
                               'Cattle' ,
                               'Pig',
                               'Wild mammals',
                               "Human population" ,
                               'Agriculture and harvest',
                               "Built up area",
                               "Energy and mining",
                               'Forest integrity',
                               'Deforestation potential'    )) %>%
  ggplot(aes( y=y, x=x, fill = value))+
  geom_tile()+
  facet_wrap(~unilabs, nrow=2)+
  scale_fill_manual(values =c("khaki","royalblue3", "violetred4") ,
                    labels = c("Neutral", "Coldspot", "Hotspot")) + theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = 'none', strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(size = 14)) + 
  labs(x='Longitude', y="Latitude", title="A")  

allpp

#----- 
setwd(here())
setwd('results')

write.table(file='table_pct_099_cutoff.txt', row.names = FALSE,
            round(table(sumelt$variable, sumelt$value)/ nrow(dfgplot)*100, digits=2) )

# Proportions per indicator
#--- R1 plot cattle-only

plotpcts <- sumelt %>%  filter(variable %in% c('hosts', 'mmb',
                                               want_landscape,
                                               'pig', 'cattle',
                                               'pop_2020_worldpop')  ) %>% 
  mutate(unilabsr1 = fct_recode(variable,
                                'Forest integrity' = 'forest_integrity_grantham',
                                'Deforestation potential' ='hewson_forest_transition_potential',
                                "Human population" = 'pop_2020_worldpop',
                                'Agriculture and harvest' = 'agriharv',
                                "Built up area"    =   "builtup",
                                "Energy and mining" =  'energy',
                                'Bat hosts' = 'hosts',
                                'Wild mammals'= 'mmb',
                                'Pig' = 'pig',
                                'Cattle' = 'cattle' )) %>% 
  mutate(unilabsr1 = fct_relevel(unilabsr1, 
                                 'Deforestation potential',
                                 'Forest integrity',
                                 "Energy and mining",
                                 "Built up area",
                                 'Agriculture and harvest',
                                 "Human population" , 
                                 'Wild mammals',
                                 'Pig',
                                 'Cattle' ,
                                 'Bat hosts')) %>%
  mutate(scenario = fct_recode(variable,
                               "All scenarios" = 'pop_2020_worldpop',
                               "All scenarios"  = "builtup",
                               "All scenarios" =  'energy',
                               'All scenarios' = 'agriharv',
                               'All scenarios' = 'forest_integrity_grantham',
                               'All scenarios' = 'hewson_forest_transition_potential',
                               'Scenario 1' = 'hosts',
                               'Scenario 2 & 4' = 'pig',
                               'Scenario 2 & 4' = 'cattle' ,
                               'Scenario 3 & 4' = 'mmb'    )) %>% 
  mutate(scenario = fct_relevel(scenario, 
                                'Scenario 1', 
                                'Scenario 2 & 4', 
                                'Scenario 3 & 4',  
                                "All scenarios" )) %>%
  ggplot(aes(unilabsr1  )) + 
  #facet_grid(~scenario) +
  geom_bar(aes(fill= valuef), alpha=0.8, position="fill") +
  scale_fill_manual(values =valuesfac,
                    labels = labelsfac)+
  theme_minimal(base_size = 15) + geom_rug() +
  theme( legend.title = element_blank(), 
         legend.position = "right", 
         axis.text.y=element_blank(),
         axis.text.x=element_text(size=15,angle=0, vjust=0.3),
         #axis.text.y=element_text(size=15)
  ) + coord_flip() + 
  labs(  x = "", y = "Area (%)",title="") 


plotpcts

#--------- Creating scenario dotmatrix


A <- matrix( rev(c(
  rep(1, 4),
  1, 0, 1, 0,
  1, 0, 1, 0,
  1, 1, 0, 0,
  rep(1, 4),
  rep(1, 4),
  rep(1, 4),
  rep(1, 4),
  rep(1, 4),
  rep(1, 4)           )), byrow=T, nrow=10, ncol=4)

colnames(A) <- c('1', '2', '3', '4')

rownames(A) <- rev(c( 
  'Bat hosts', 
  'Cattle' ,  
  'Pig',
  'Wild mammals',
  "Human population" ,
  'Agriculture and harvest',
  "Built up area",
  "Energy and mining",
  'Forest integrity',
  'Deforestation potential'))

print(A)

library(reshape2)

longData<-melt(A)

longData<-longData[longData$value!=0,]

# steelblue4
#stroke = 10 shape = 21, , stroke = 10
scenariodots <- ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_point(shape = 19, colour = "grey30", fill = "white", size=10) + 
  labs(x="Scenario", y="", title="B") +
  theme_minimal(base_size = 15) + geom_rug() + 
  theme(axis.text.x=element_text(size=15,angle=0, vjust=0.3),
        axis.text.y=element_text(size=15)                  )

scenariodots

#----------
# Exporting 
ggsave('Figure_01_099_cutoff.png',
       plot = grid.arrange(allpp, grid.arrange(scenariodots, plotpcts, ncol = 2), nrow = 2), #last_plot()
       dpi = 400,
       width = 14, #12
       height = 12, #11
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
  mutate(unilabs = fct_relevel(unilabs, 
                               'Bat hosts',
                               'Bovidae livestock' ,
                               'Pig',
                               'Wild mammals',
                               "Human population" ,
                               'Agriculture and harvest',
                               "Built up area",
                               "Energy and mining",
                               'Forest integrity',
                               'Deforestation potential'    )) %>%
  ggplot(aes( y=y, x=x, fill = valuef))+
  geom_tile()+
  facet_wrap(~unilabs, nrow=2)+
  scale_fill_manual(values =valuesfac ,
                    labels = labelsfac) + theme_bw() +
  theme(legend.title = element_blank(),  
        strip.background = element_rect(fill = "whitesmoke"),
        legend.position = 'none',
        strip.text = element_text(size = 14)) + 
  labs(x='Longitude', y="Latitude") 


allppb

#----
setwd(here())
setwd('results')

write.table(file='table_pct_bovliv_099_cutoff.txt', row.names = FALSE,
            round(table(sumelt$variable, sumelt$value)/ nrow(dfgplot)*100, digits=2) )

# Percentages per indicator  - bovidae livestock

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
                              'Agriculture and harvest' = 'agriharv',
                              'Forest integrity' = 'forest_integrity_grantham',
                              'Deforestation potential' ='hewson_forest_transition_potential',
                              "Human population" = 'pop_2020_worldpop' )) %>% 
  mutate(unilabs = fct_relevel(unilabs, 
                               'Deforestation potential',
                               'Forest integrity',
                               "Energy and mining",
                               "Built up area",
                               'Agriculture and harvest',
                               "Human population" , 
                               'Wild mammals',
                               'Pig',
                               'Bovidae livestock' ,
                               'Bat hosts')) %>%
  mutate(scenario = fct_recode(variable,
                               "All scenarios" = 'pop_2020_worldpop',
                               "All scenarios"  = "builtup",
                               "All scenarios" =  'energy',
                               'All scenarios' = 'agriharv',
                               'All scenarios' = 'forest_integrity_grantham',
                               'All scenarios' = 'hewson_forest_transition_potential',
                               'Scenario 1' = 'hosts',
                               'Scenario 2 & 4' = 'pig',
                               'Scenario 2 & 4' = 'bovliv' ,
                               'Scenario 3 & 4' = 'mmb'    )) %>% 
  mutate(scenario = fct_relevel(scenario, 
                                'Scenario 1', 
                                'Scenario 2 & 4', 
                                'Scenario 3 & 4',  
                                "All scenarios" )) %>%
  ggplot(aes(unilabs )) + 
  #facet_grid(~scenario) +
  geom_bar(aes(fill= valuef), alpha=0.8, position="fill") +
  scale_fill_manual(values =valuesfac,
                    labels = labelsfac)+
  theme_minimal(base_size = 15) + geom_rug() +
  theme( legend.title = element_blank(), 
         legend.position = "right", 
         axis.text.y=element_blank(),
         axis.text.x=element_text(size=15,angle=0, vjust=0.3),
         #axis.text.y=element_text(size=15)
  ) + coord_flip() + 
  labs(  x = "", y = "Area (%)",title="") 


plotpctsb

# Exporting bovliv -------------------------

ggsave('Figure_01_bovliv_099_cutoff.png',
       plot = grid.arrange(allppb, grid.arrange(scenariodots, plotpctsb, ncol = 2), nrow = 2), #last_plot()
       dpi = 400,
       width = 14, #12
       height = 12, #11
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


# ------------------------------------
# Overlapp - all potential host layers

allhot_mmb <- dplyr::filter(su, 
                            grepl('violetred4', su$`hosts` ) & 
                              grepl('violetred4', su$`mmb` )  &
                              grepl('violetred4', su$`pig`) & 
                              grepl('violetred4', su$`cattle` ) )


allhot_mammalsb <- dplyr::filter(su, 
                                 grepl('violetred4', su$`hosts` ) & 
                                   grepl('violetred4', su$`mmb` )  &
                                   grepl('violetred4', su$`pig`) & 
                                   grepl('violetred4', su$`bovliv` ) )


nrow(distinct(allhot_mammalsb[, c('x','y')]))

nrow(distinct(allhot_mammals_all[, c('x','y')]))

nrow(distinct(allhot_mmb[, c('x','y')]))

write.csv(file = 'hotspot_overlap_mammals_099_cutoff.csv', allhot_mmb, row.names = FALSE)

write.csv(file = 'hotspot_overlap_mammals_bovidae_099_cutoff.csv', allhot_mammalsb, row.names = FALSE)

#--------------------------------------------------------