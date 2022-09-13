# Checking data covariation

library(here)
require(gghighlight)
require(ggplot2)

setwd(here())

setwd('results')
df <- read.csv('prepdf.csv')

head(df)

s <- ggplot(df, aes(forest_integrity_grantham, pop_2020_worldpop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(title = "") +
  #ylab("") +
  #xlab("") +
  theme(legend.position="none",plot.title=element_text(hjust=0.5))

# Plot with highlight
s + gghighlight(forest_integrity_grantham > 3 & pop_2020_worldpop > 2000)

# bats and people

sb <- ggplot(df, aes(hosts_muylaert, pop_2020_worldpop)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(title = "") +
  #ylab("") +
  #xlab("") +
  theme(legend.position="none",plot.title=element_text(hjust=0.5))+
   gghighlight(hosts_muylaert > 5 & pop_2020_worldpop > 5000)

sb


