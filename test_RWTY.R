setwd("~/Dropbox/R Projects/RWTY")
#setwd("~/Documents/Projects_current/RWTY")

source("RWTY.R")

test1 <- read.nexus(file="./testdata/fairywrens/PCFW.nex.run1.t")
test2 <- read.nexus(file="./testdata/fairywrens/PCFW.nex.run2.t")
test3 <- read.nexus(file="./testdata/fairywrens/PCFW.nex.run1.t")
test4 <- read.nexus(file="./testdata/fairywrens/PCFW.nex.run2.t")
cumtest <- cumulative.freq(test1, burnin=100, window=100, gens.per.tree=1000)
slidetest <- slide.freq(test1, burnin=100, window=100, gens.per.tree=1000)
comp2test <- compare.two(test1, test2, burnin=100)
compntest <- compare.n(x=list(test1, test2, test3, test4), setnames = c("test1", "test2", "test3", "test4"), burnin=100)
# Change in SD along chain for cumulative?
# Same for n chains?


test.hox1 <- read.nexus(file="./testdata/hoxgenes/Dataset2_con.nex.run1.t")
test.hox2 <- read.nexus(file="./testdata/hoxgenes/Dataset2_con.nex.run2.t")

slidetest <- slide.freq(test1, burnin=100, window=100, gens.per.tree=1000)

cumtest <- cumulative.freq(test1, burnin=100, window=100, gens.per.tree=1000, slide.freq.table=slidetest)
plot.cladeprobs(cumtest, 20)
plot.cladeprobs(slidetest, 20)
plot.cladevar(cumtest, 20)
plot.cladevar(slidetest, 20)