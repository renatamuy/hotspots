# Transmission risk scenarios and access to health care  ---------------------------------

require(tidyverse)
require(ggplot2)
library(here())
require(raster)

#remotes::install_github("AllanCameron/geomtextpath")

setwd(here())

setwd('results/')

dfg <- read.csv("gstar.csv")

d <- read.csv('prepdf.csv')

colnames(dfg)
colnames(d)

# Minutes to hours


length(d$motor_travel_time_weiss/60)
tt <- d$motor_travel_time_weiss/60

plot(dfg$lincomb_hosts , tt  , col = dfg$col95_hosts, 
     ylab='Access to healthcare (h)',
     xlab= 'SARS-like bat hosts', pch=21)


dfg$tt <- tt

summary(d$hewson_forest_transition_potential)


plot(dfg$cattle_gilbert, dfg$tt   , col = dfg$col95_cattle, 
     ylab='Access to healthcare (h)',
     xlab= 'Cattle', pch=21)

plot(dfg$pig_gilbert, dfg$tt   , col = dfg$col95_pig, 
     ylab='Access to healthcare (h)',
     xlab= 'Pig', pch=21)

plot(dfg$mammals_iucn_mode, dfg$tt   , col = dfg$col95_mammals, 
     ylab='Access to healthcare (h)',
     xlab= 'Wild mammals', pch=21)

# Summaries
dfg %>% group_by(col95_hosts) %>% 
  summarise( mean_hours = mean(tt), sd=sd(tt) )

dfg %>% group_by(col95_mammals) %>% 
  summarise( mean_hours = mean(tt), sd=sd(tt) )

dfg %>% group_by(col95_cattle) %>% 
  summarise( mean_hours = mean(tt), sd=sd(tt) )

dfg %>% group_by(col95_pig) %>% 
  summarise( mean_hours = mean(tt), sd=sd(tt) )



table(dfg$col95_mammals) / nrow(dfg)

boxplot(dfg$lincomb_hosts ~ dfg$col95_hosts )

boxplot(tt ~ dfg$col95_hosts, col = c('khaki', 'royalblue3', 'violetred4'))

boxplot(d$forest_integrity_grantham ~ dfg$col95_hosts)


#
check <- dfg %>% group_by(col95_hosts,col95_mammals, col95_pig,col95_cattle ) %>% 
  summarise( mean_hours = mean(tt), sd=sd(tt) )


# Forest loss risk
dfg$forest_loss_risk <- d$hewson_forest_transition_potential

dfg %>% group_by(col95_hosts ) %>% 
  summarise( mean_risk = mean(forest_loss_risk), sd=sd(forest_loss_risk) )

dfg %>% group_by(col95_mammals) %>% 
  summarise( mean_risk = mean(forest_loss_risk), sd=sd(forest_loss_risk) )

dfg %>% group_by(col95_pig) %>% 
  summarise( mean_risk = mean(forest_loss_risk), sd=sd(forest_loss_risk) )

dfg %>% group_by(col95_cattle) %>% 
  summarise( mean_risk = mean(forest_loss_risk), sd=sd(forest_loss_risk) )


check2 <- dfg %>% group_by(col95_hosts,col95_mammals, col95_pig, col95_cattle ) %>% 
  summarise( mean_risk = mean(forest_loss_risk), sd=sd(forest_loss_risk) )



cor(dfg$motor_travel_time_weiss, dfg$pop_2020_worldpop, method = 'spearman')

cor(dfg$motor_travel_time_weiss, dfg$pop_2020_worldpop, method = 'pearson')

cor(d$motor_travel_time_weiss, d$pop_2020_worldpop, method = 'spearman')

cor(d$motor_travel_time_weiss, d$pop_2020_worldpop, method = 'pearson')

plot(d$motor_travel_time_weiss, d$pop_2020_worldpop)

