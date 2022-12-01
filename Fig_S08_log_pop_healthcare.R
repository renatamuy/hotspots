# Muylaert et al. 
# Supplemental plot log pop log healthcare
# Population plots (continuation of code 02)

setwd(here())
setwd('results/')

dfg <- read.csv("gstar.csv")
d <- read.csv('prepdf.csv')

colnames(dfg)
colnames(d)

# Minutes to hours
length(d$motor_travel_time_weiss/60)
tt <- d$motor_travel_time_weiss/60
dfg$tt <- tt

tcont <- (d$motor_travel_time_weiss-min(d$motor_travel_time_weiss))/
  (max(d$motor_travel_time_weiss)-min(d$motor_travel_time_weiss))
popcont <- (d$pop_2020_worldpop-min(d$pop_2020_worldpop))/
  (max(d$pop_2020_worldpop)-min(d$pop_2020_worldpop))

plot(bivmap1, col = as.vector(col.matrix))

unique(values(bivmap1))

as.vector(col.matrix)

bivmap1m <- na.omit(values(bivmap1) )

# khaki is #F0E68C

col.matrix[4] # 3, tricky

col.matrix[8] # 3, tricky

col.matrix[16] # red ->  12 #FF0000 

col.matrix[14]  # black -> 10 #141414 

listacor <- c()
for ( i in unique(bivmap1m) ) {
  print(i+4)
  coratual <- as.vector(col.matrix)[i+4]
  listacor <- c(listacor, coratual)
}
unique(bivmap1m)

col.matrix
listacor

# Colors assigned from bottom to up (row), left to right (col)
# Last color in legend is red =  "#FF0000"

cores1 <- ifelse(bivmap1m == 11, "#890A0A",
                 ifelse(bivmap1m == 8, "#F77346",
                        ifelse(bivmap1m == 7, "#B87963" ,
                               ifelse(bivmap1m == 10,'blue', #"#141414",
                                      ifelse(bivmap1m == 1, "#E0EEEE",
                                             ifelse(bivmap1m == 9, "#798181",
                                                    ifelse(bivmap1m == 2, "#E8EABD",
                                                           ifelse(bivmap1m == 3,"#F0E68C",
                                                                  ifelse(bivmap1m == 12,"#FF0000", NA)))))))))




table(cores1)

d$cores1 <- cores1
#5344  #E8EABD
#d1 <- d %>%  filter( cores1 != "#141414") 

plot(log10(d$motor_travel_time_weiss), 
     log10(d$pop_2020_worldpop), 
     col = alpha(d$cores1, 0.4), 
     pch=16, 
     cex=0.6,
     ylab='Human population (log)',
     xlab='Time to reach healthcare (log)')

# Ggplot giving weird points on plot 

d$travel_log <- log10(d$motor_travel_time_weiss)
d$poplog <-log10(d$pop_2020_worldpop)

plot_quantiles_scenario1 <- ggplot(d, 
                                   aes(x = travel_log,
                                       y = poplog) )+ 
  geom_point(color=d$cores1, alpha=0.2)+
  facet_wrap(~d$cores1 )+
  labs(x='Time to reach healthcare (log10)',
       y='Human population (log10)')+
  theme_minimal()  +
  theme(strip.text.x = element_blank())
#ylim(-4, 3.5) # weird points only in ggplot

plot_quantiles_scenario1

# Cores scenario 2 ----

bivmap2m <- na.omit(values(bivmap2) )
unique(bivmap2m)

cores2 <- ifelse(bivmap2m == 11, "#890A0A",
                 ifelse(bivmap2m == 8, "#F77346",
                        ifelse(bivmap2m == 7, "#B87963" ,
                               ifelse(bivmap2m == 10, 'blue', #"#141414",
                                      ifelse(bivmap2m == 1, "#E0EEEE",
                                             ifelse(bivmap2m == 9, "#798181",
                                                    ifelse(bivmap2m == 2, "#E8EABD",
                                                           ifelse(bivmap2m == 3,"#F0E68C",
                                                                  ifelse(bivmap2m == 12,"#FF0000", NA)))))))))




d$cores2 <- cores2

plot(log10(d$motor_travel_time_weiss), 
     log10(d$pop_2020_worldpop), 
     col = alpha(d$cores2, 0.4), 
     pch=16, 
     cex=0.6,
     ylab='Human population (log)',
     xlab='Time to reach healthcare (log)')

d$travel_log <- log10(d$motor_travel_time_weiss)
d$poplog <-log10(d$pop_2020_worldpop)

plot_quantiles_scenario2 <- ggplot(d, 
                                   aes(x = travel_log,
                                       y = poplog) )+ 
  geom_point(color=d$cores2, alpha=0.2)+
  facet_wrap(~d$cores2)+
  labs(x='Time to reach healthcare (log10)',
       y='Human population (log10)')+
  theme_minimal()  +
  theme(strip.text.x = element_blank())

plot_quantiles_scenario2

#----------------------------------------------------------------------