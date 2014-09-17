#setwd("~/Dropbox/R Projects/RWTY")
#setwd("~/Documents/Projects_current/RWTY")

# comment

source("RWTY.R")

test1 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run1.t")
test2 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run2.t")
test3 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run1.t")
test4 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run2.t")
anole1 <- load.trees(file="~/Desktop/Missing_Data_Project/96_taxon_dataset/96_Bayes_Max_gens_t0.02/anolis_0.5complete_rep1.nex.run1.t")
multitest <- analyze.rwty(chains=list(test1, test2, test3, test4), burnin=100, 
                          window.size=20, labels=c("Run1", "Run2", "Run3", "Run4"), filename="multitest.pdf")
# removed step=10
### Ah crap, we've got it auto-detecting gens per tree but are still passing that as an argumenty

singletest <- analyze.rwty(test2, burnin=100, window.size=100, filename="test.pdf", labels = "Singletest") #removed command , step=10
singletest.anole <- analyze.rwty(anole1, burnin=100, window.size=100, filename="anoletest.pdf", labels = "Singletest") 

test5 <- load.trees("./testdata/t_and_p/AllDataProt.nex.run1.t")
test6 <- load.trees("./testdata/t_and_p/AllDataProt.nex.run1.t")

test7 <- load.trees(file="./testdata/renee/StarBEASTLog.amr_alignment3edited.trees")

cumtest <- cumulative.freq(test1, burnin=100, window=100, gens.per.tree=1000)
slidetest <- slide.freq(test1, burnin=100, window=100, gens.per.tree=1000)
#comp2test <- compare.two(test1, test2, burnin=100)
compntest <- compare.n(x=list(test1, test2, test3, test4), setnames = c("test1", "test2", "test3", "test4"), burnin=100)
# Change in SD along chain for cumulative?
# Same for n chains?

mytrees <- load.trees("./testdata/t_and_p/AllDataProt.nex.run1.t")
rwtytest <- analyze.rwty(mytrees, burnin=100, window.size=50, gens.per.tree=1000, step=100)


test.hox1 <- read.nexus(file="./testdata/hoxgenes/Dataset2_con.nex.run1.t")
test.hox2 <- read.nexus(file="./testdata/hoxgenes/Dataset2_con.nex.run2.t")
slidetest <- slide.freq(test.hox1, burnin=1000, window=1000, gens.per.tree=1000)
cumtest <- cumulative.freq(test.hox1, burnin=1000, window=1000, gens.per.tree=1000, slide.freq.table=slidetest)
esstest <- tree.ess(test.hox1, burnin=1000)


plot.cladeprobs(cumtest$cumulative.table, 20)
plot.cladeprobs(slidetest$slide.table, 20)
plot.cladevar(cumtest$cumulative.table, 20)
plot.cladevar(slidetest$slide.table, 20)



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
ts.plot(ar.sim)
est1 <- continuous.ess(ar.sim, burnin=0, N=500)
est2 <- effectiveSize(ar.sim)
est1$approx.ESS
est2

ar.sim <- arima.sim(model=list(ar=c(.25)),n=10000)
est1 <- continuous.ess(ar.sim, burnin=0, N=500)
est2 <- effectiveSize(ar.sim)
est1$approx.ESS
est2

ar.sim <- arima.sim(model=list(ar=c(.5)),n=10000)
est1 <- continuous.ess(ar.sim, burnin=0, N=500)
est2 <- effectiveSize(ar.sim)
est1$approx.ESS
est2

