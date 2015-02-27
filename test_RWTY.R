#setwd("~/Dropbox/R Projects/RWTY")
#setwd("~/Documents/Projects_current/RWTY")


# comment

setwd("~/Desktop/RWTY")
source("RWTY.R")

setwd("testdata/Hibbett/")
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
no.p.single.test <- analyze.rwty(run2, burnin=10, window.size=5, treespace.points=30, filename="without p single.pdf", min.freq=90)
multitest <- analyze.rwty(chains=list(run1, run2, run3, run4, run5, run6, run7, run8, run9, run10), burnin=10, 
                          window.size=5, treespace.points=20, 
                          labels=c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6", "Run7", "Run8", "Run9", "Run10"),
                          filename="Hibbett RWTY.pdf")
smaller.multi <- analyze.rwty(chains=list(run2, run3, run4, run5, run6, run10), burnin=10, 
                              window.size=5, treespace.points=20, 
                              labels=c("Run1", "Run2", "Run3", "Run4", "Run5", "Run6"),
                              filename="Hibbett RWTY.pdf", min.freq=0.1)
