setwd("~/Documents/GitHub/RWTY")
#setwd("~/Documents/Projects_current/RWTY")

source("RWTY.R")

test1 <- read.nexus(file="./testdata/fairywrens/PCFW.nex.run1.t")
test2 <- read.nexus(file="./testdata/fairywrens/PCFW.nex.run2.t")
test3 <- read.nexus(file="./testdata/fairywrens/PCFW.nex.run1.t")
test4 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run2.t")
test5 <- load.trees("./testdata/t_and_p/AllDataProt.nex.run1.t")
cumtest <- cumulative.freq(test1, burnin=100, window=100, gens.per.tree=1000)
slidetest <- slide.freq(test1, burnin=100, window=100, gens.per.tree=1000)
#comp2test <- compare.two(test1, test2, burnin=100)
compntest <- compare.n(x=list(test1, test2, test3, test4), setnames = c("test1", "test2", "test3", "test4"), burnin=100)
# Change in SD along chain for cumulative?
# Same for n chains?


test.hox1 <- read.nexus(file="./testdata/hoxgenes/Dataset2_con.nex.run1.t")
test.hox2 <- read.nexus(file="./testdata/hoxgenes/Dataset2_con.nex.run2.t")
slidetest <- slide.freq(test.hox1, burnin=100, window=100, gens.per.tree=1000)
cumtest <- cumulative.freq(test.hox1, burnin=100, window=100, gens.per.tree=1000, slide.freq.table=slidetest)
esstest <- tree.ess(test.hox1, burnin=1000)


plot.cladeprobs(cumtest, 20)
plot.cladeprobs(slidetest, 20)
plot.cladevar(cumtest, 20)
plot.cladevar(slidetest, 20)



test.distmatrix1 <- tree.dist.matrix(c(test1[900:1000], test2[900:1000]), 
    treenames=c(paste("Run.1", seq(1:101), sep="."), paste("Run.2", seq(1:101), sep=".")))

mds.treespace <- cmdscale(test.distmatrix1 ,eig=TRUE, k=2)


# plot treespace
test9.t <- read.nexus("./testdata/t_and_p/AllDataProt.nex.run1.t")
d <- treespace.single(test9.t[1:100])
d

# test ESS metric with continuous data
library(coda)
test9.p <- read.table("./testdata/t_and_p/AllDataProt.nex.run1.p", header=TRUE, skip=1)

est1 <- continuous.ess(test9.p$TL, burnin=1000, N=500)
est2 <- effectiveSize(test9.p$TL[1000:length(test9.p$TL)])
est1$approx.ESS
est2

est1 <- continuous.ess(test9.p$TL, burnin=5000, N=500)
est2 <- effectiveSize(test9.p$TL[5000:length(test9.p$TL)])
est1$approx.ESS
est2

est1 <- continuous.ess(test9.p$TL, burnin=8000, N=500)
est2 <- effectiveSize(test9.p$TL[8000:length(test9.p$TL)])
est1$approx.ESS
est2

est1 <- continuous.ess(test9.p$TL[3000:7000], burnin=0, N=500)
est2 <- effectiveSize(test9.p$TL[3000:7000])
est1$approx.ESS
est2

# and with simulated data
ar.sim <- arima.sim(model=list(ar=c(.001)),n=10000)
est1 <- continuous.ess(ar.sim, burnin=0, N=500)
est2 <- effectiveSize(ar.sim)
est1$approx.ESS
est2



