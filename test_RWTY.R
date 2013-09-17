setwd("~/Dropbox/R Projects/RWTY")
source("RWTY.R")

test1 <- read.nexus(file="PCFW.nex.run1.t")
test2 <- read.nexus(file="PCFW.nex.run2.t")
test3 <- read.nexus(file="PCFW.nex.run1.t")
test4 <- read.nexus(file="PCFW.nex.run2.t")
cumtest <- cumulative.freq(test1, burnin=100, window=100, gens.per.tree=1000)
slidetest <- slide.freq(test1, burnin=100, window=100, gens.per.tree=1000)
comp2test <- compare.two(test1, test2, burnin=100)
compntest <- compare.n(x=list(test1, test2, test3, test4), setnames = c("test1", "test2", "test3", "test4"), burnin=100)
# Change in SD along chain for cumulative?
# Same for n chains?
# For compare.n, use melt and plotmatrix
# comment