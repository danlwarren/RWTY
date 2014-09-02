
setwd("~/Github/RWTY")
source("RWTY.R")
setwd("~/Github/RWTY/testdata/Hibbett")

run1 <- load.trees("run1.t", skiplines.p=0)
run2 <- load.trees("run2.t", skiplines.p=0)
run3 <- load.trees("run3.t", skiplines.p=0)
run4 <- load.trees("run4.t", skiplines.p=0)
run5 <- load.trees("run5.t", skiplines.p=0)
run6 <- load.trees("run6.t", skiplines.p=0)
run7 <- load.trees("run7.t", skiplines.p=0)
run8 <- load.trees("run8.t", skiplines.p=0)
run9 <- load.trees("run9.t", skiplines.p=0)
run10 <- load.trees("run10.t", skiplines.p=0)


with.p.single.test <- analyze.rwty(run1, burnin=10, window.size=20, treespace.points = 100, filename="with p single.pdf")
no.treespace.single.test <- analyze.rwty(run1, burnin=10, window.size=20, treespace.points=100, filename="no treespace single.pdf", treespace=FALSE)
multitest <- analyze.rwty(chains=list(run1, run2, run3, run4, run5, run6, run7, run8, run9, run10), burnin=10, 
                          window.size=20, treespace.points=50, 
                          labels=c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6", "Run7", "Run8", "Run9", "Run10"),
                          filename="Hibbett RWTY.pdf")

