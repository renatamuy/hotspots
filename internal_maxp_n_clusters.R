
library(here)

setwd(here())

setwd('results/maxp_bovliv')

load("maxp_o5pct.RData")

table(maxp_o5pct$Clusters)

load("maxp_o10pct.RData")

table(maxp_o10pct$Clusters)

