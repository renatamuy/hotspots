# sensitivity to changing the critical value for defining hotspots

rm(list = ls())

library(here)
setwd(here())

source('00_packages.R')

setwd('results')

d99 <- read.csv('gstar_099.csv' )

d <- read.csv('gstar.csv' )

# % percentage in grids that were hotspots at 95% and % of pixels that were hotspots at 99%

d99col <- d99 %>% dplyr::select(c('x','y', starts_with("col99")))

dcol <- d %>% dplyr::select(c('x','y', starts_with("col95")))

head(d99col)

head(dcol)

tofocus99 <- colnames(d99col %>% dplyr::select(!c( 'x', 'y',
  'col99_bovliv',
  'col99_pollution',
  'col99_trans', 'col99_mammals', 'col99_motor_travel_time_weiss'   ))    )

tofocus <- colnames(dcol %>% dplyr::select(!c( 'x', 'y',
                                                   'col95_bovliv',
                                                   'col95_pollution',
                                                   'col95_trans', 'col95_mammals', 'col95_motor_travel_time_weiss'   ))    )




#99
d99s <- d99col %>% dplyr::select(all_of(tofocus99))

colnames(d99s) <- sub('col99_', '', colnames(d99s))

d99s %>% purrr::map(., ~janitor::tabyl(.))

d99map <- melt(d99s %>% purrr::map(., ~janitor::tabyl(.))) %>% filter(!variable == 'n')

#95 

ds <- dcol %>% dplyr::select(all_of(tofocus)) 

colnames(ds) <- sub('col95_', '', colnames(ds))

ds %>% purrr::map(., ~janitor::tabyl(.)) 

dmap <- melt(ds %>% purrr::map(., ~janitor::tabyl(.))) %>% filter(!variable == 'n')
 
head(d99map)
head(dmap)

#
d99map$critical <- '99%'
dmap$critical <- '95%'

all <- rbind( d99map, dmap)

colnames(all) <- c('status', 'variable', 'value', 'driver', 'critical')



#

head(all)
all$valuef <- factor(all$status, levels =  c("royalblue3" ,"khaki"  ,    "violetred4"))
valuesfac <- c("royalblue3", "khaki", "violetred4")
labelsfac  <- c("Coldspot", "Intermediate", "Hotspot")
head(all)


all_labs <- all %>%    mutate(hotspot = fct_relevel(status ,
                                'Hotspot' = 'violetred4',
                                'Intermediate' = 'khaki',
                                'Coldspot' = 'royalblue3') )%>% 
  mutate(hotspot = fct_recode(status ,
                                'Hotspot' = 'violetred4',
                                'Intermediate' = 'khaki',
                                'Coldspot' = 'royalblue3' ))%>%  mutate(unilabsr1 = fct_recode(driver,
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
  dplyr::select(unilabsr1, hotspot, critical, value ) %>% 
arrange(unilabsr1)
  
  


head(all_labs)
# Adding built up area hotspots equal zero
builtrows <- data.frame(unilabsr1 = c('Built up area','Built up area','Pig', 'Human population'),
                            hotspot =  rep('Coldspot',4),
                          critical = c('95%','99%','99%','99%'),
                        value = c(0,0,0,0 ))
    
                    

head(all_labs)
all_labs_all <- rbind(all_labs, builtrows)

library(lemon)

all_labs_all %>% #filter(hotspot == 'Hotspot') %>% 
  mutate(hotspot_lab = fct_relevel(hotspot, 'Coldspot' ='Coldspot',
                             'Intermediate' = 'Intermediate',
                             'Hotspot' = 'Hotspot') ) %>%
  mutate(labss = fct_recode(unilabsr1,
                            'Forest \n integrity'    = 'Forest integrity' ,
                                'Deforestation \n potential' = 'Deforestation potential',
                                "Human \n population" = "Human population",
                                'Agriculture \n and harvest' = 'Agriculture and harvest',
                                "Built \n up area"    =   "Built up area",
                                "Energy \n and mining" =  "Energy and mining",
                                'Bat hosts' = 'Bat hosts',
                                'Wild \n mammals'=   'Wild mammals',
                                'Pig' = 'Pig',
                                'Cattle' =  'Cattle' )) %>% 
  mutate(labss = fct_relevel(labss, 
                             'Bat hosts',
                             'Cattle' ,
                             'Pig',
                             'Wild \n mammals',
                             "Human \n population" , 
                             'Agriculture \n and harvest',
                             "Built \n up area",
                             "Energy \n and mining",
                             'Forest \n integrity',
                                 'Deforestation \n potential'  )) %>% 
ggplot(aes(x = critical, y = value, group = labss)) + 
  geom_line() + 
  geom_point(size = 3, aes(color = critical)) + 
  lemon::facet_rep_grid( hotspot_lab ~ labss  ) +
  #facet_grid(~ hotspot * unilabsr1) + #, ncol=10
  ## one call to labs reduces the code to relabel the axis
  labs(x = NULL, y = "% Area") + 
  theme_minimal(base_size = 15) +
  theme( legend.position = "bottom", axis.text.x=element_blank()) 

setwd(here())
setwd('results')

ggsave('Fig_sensitivity.png',
       plot = last_plot(),
       dpi = 400,
       width = 16, 
       height = 5,
       limitsize = TRUE)

# Difference
dmap

d99map

# Export table


xlsx::write.xlsx2(all_labs_all, file= 'sensitivity_table.xlsx', sheetName = 'Table')

