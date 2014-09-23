#setwd("~/Dropbox/R Projects/RWTY")
#setwd("~/Documents/Projects_current/RWTY")


# comment

setwd("~/Github/RWTY")
source("RWTY.R")

setwd("~/Github/RWTY/testdata/Hibbett/")
treefiles <- list.files(pattern="\\.t")
run1 <- load.trees(treefiles[1], trim=20, skiplines.p=0)
run2 <- load.trees(treefiles[2], trim=20, skiplines.p=0)
run3 <- load.trees(treefiles[3], trim=20, skiplines.p=0)
run4 <- load.trees(treefiles[4], trim=20, skiplines.p=0)
run5 <- load.trees(treefiles[5], trim=20, skiplines.p=0)
run6 <- load.trees(treefiles[6], trim=20, skiplines.p=0)
run7 <- load.trees(treefiles[7], trim=20, skiplines.p=0)
run8 <- load.trees(treefiles[8], trim=20, skiplines.p=0)
run9 <- load.trees(treefiles[9], trim=20, skiplines.p=0)
run10 <- load.trees(treefiles[10], trim=20, skiplines.p=0)


with.p.single.test <- analyze.rwty(run1, burnin=10, window.size=5, treespace.points = 30, filename="with p single.pdf")
no.treespace.single.test <- analyze.rwty(run1, burnin=10, window.size=5, treespace.points=30, filename="no treespace single.pdf", treespace=FALSE)
no.p.single.test <- analyze.rwty(run2, burnin=10, window.size=5, treespace.points=30, filename="without p single.pdf")
multitest <- analyze.rwty(chains=list(run1, run2, run3, run4, run5, run6, run7, run8, run9, run10), burnin=10, 
                          window.size=5, treespace.points=20, 
                          labels=c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6", "Run7", "Run8", "Run9", "Run10"),
                          filename="Hibbett RWTY.pdf")
smaller.multi <- analyze.rwty(chains=list(run2, run3, run4, run5, run6, run10), burnin=10, 
                              window.size=5, treespace.points=20, 
                              labels=c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6"),
                              filename="Hibbett RWTY.pdf")


test1 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run1.t")
test2 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run2.t")
test3 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run1.t")
test4 <- load.trees(file="./testdata/fairywrens/PCFW.nex.run2.t")
anole1 <- load.trees(file="~/Desktop/Missing_Data_Project/96_taxon_dataset/96_Bayes_Max_gens_t0.02/anolis_0.5complete_rep1.nex.run1.t")
multitest <- analyze.rwty(chains=list(test1, test2, test3, test4), burnin=100, # removed step=10
                          window.size=20, labels=c("Run1", "Run2", "Run3", "Run4"), filename="multitest.pdf")

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

