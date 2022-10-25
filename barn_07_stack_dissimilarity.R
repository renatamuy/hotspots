# Pretend each raster is a species

tofocus <- colnames(   dfgplot %>% dplyr::select(c(contains("hot95")))    )
tofocus

mycom <- dfgplot[tofocus]

head(mycom)

mycombin <- data.frame(ifelse(mycom == 'Hotspot', "1",
       ifelse(mycom != 'Hotspot', "0", NA) ) )

str(mycombin)
head(mycombin)

as.numeric('1')

mycombin_num <- data.frame(apply(mycombin, FUN=as.numeric, MARGIN=2)) # column dimension
str(mycombin_num)

head(mycombin_num)

colnames(mycombin_num) <- sub('hot95_', '',colnames(mycombin_num) )
colnames(mycombin_num) 
want_landscape <- c('builtup', 'energy',  'agriharv', 
                    'forest_integrity_grantham',
                    'hewson_forest_transition_potential'                  )

want_bat <- c('hosts')

want_expo_spread <- c('pop_2020_worldpop')

# Sarbecovirus-oriented scenario
want_risk1 <- c(want_landscape,want_bat,want_expo_spread)

# More general - viruses
want_risk2 <- c(want_risk1, 'pig', 'cattle','mammals' )

want_risk3 <- c(want_landscape, want_expo_spread, 'pig', 'cattle','mammals' )

want_risk4 <- c(want_landscape, want_expo_spread, 'mammals' )

com1 <- mycombin_num

# Setting to zero in scenario 1 the things that are only true in scenario 2

com1[com1$'pig' ==1, 'pig'] <- 0

com1[com1$'pig' ==1, 'cattle'] <- 0

com1[com1$'pig' ==1, 'mammals'] <- 0

com2 <- mycombin_num

table(com1$hosts)
table(com1$pig)
table(com2$pig)

which(com2$pig == 1)

myjacs <- c()

for(s in 1:nrow(com1))
{
  
  print(s)

temp <- rbind(com1[s,], com2[s,])

# 0 1 

 jac <- vegdist(temp, "jaccard")

 myjacs <- c(myjacs, jac[1]) 

 }
 
hist(myjacs)

length(myjacs)

getwd()

save(myjacs, file = 'jaccard.RData')

#

# Jaccard difference map -------------------------
load('jaccard.RData')

myjacs[is.nan(myjacs)] <- 0

go2$jac <- myjacs

p2jac <- ggplot()+
  geom_tile(data = go2, aes( y=y, x=x, fill = jac))+ theme_bw() +
  geom_sf(data=sf::st_as_sf(target), fill= 'transparent',col="black", size=0.50)+
  scale_fill_gradientn(colours = pal2) + #, breaks = b
  #scale_fill_gradientn(colours = c("royalblue3", "khaki", "violetred4"), breaks=b, labels=format(b)) +
  theme(legend.title=element_blank(), legend.position = 'bottom',  strip.text = element_text(size = 14)) +
  labs(x='Longitude', y="Latitude",
       title = "Jaccard dissimilarity between scenarios", 
       subtitle = ''  ) 

p2jac

ggsave(
  'hotspots_jaccard.png',
  plot = last_plot(),
  dpi = 400,
  width = 5,
  height = 6,
  limitsize = TRUE)

