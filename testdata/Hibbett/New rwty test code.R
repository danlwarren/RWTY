setwd("~/GitHub/RWTY/testdata/Hibbett")

install.packages("~/GitHub/RWTY/rwty",repos=NULL, type="source")
library(rwty)
data(fungus)
burnin <- 10
test.chains <- list(run1, run2)
test.chains <- check.chains(test.chains)
test.ptable <- merge.ptables(test.chains, burnin = burnin)
p <- analyze.simple(chains = list(run1, run2), burnin = 100,treespace.points = 50)

run3 <- run2
run3$trees <- run3$trees[1:501]
run3$ptable <- run3$ptable[1:501,]
test.chains <- list(run1, run3)
test.chains <- check.chains(test.chains)
test.ptable <- merge.ptables(test.chains, burnin = burnin)
p <- analyze.simple(chains = list(run1, run3), burnin = 100,treespace.points = 50)

# HUGE analysis
alltrees <- load.multi(path="/Users/danwarren/Downloads/AP_DRYAD/MrBayes nexus Files", trim=20, skip=1)
t <- names(alltrees)
t <- lapply(t, FUN=function(x) gsub("AP_", "", x))
t <- lapply(t, FUN=function(x) gsub("\\.t", "", x))
t <- lapply(t, FUN=function(x) gsub("\\.nex", "", x))
names(alltrees) <- t
p <- analyze.simple(alltrees, burnin=20, treespace.points=20, file="alltrees.pdf", overwrite=TRUE, facet=FALSE)

# Smaller multilocus
data(demo)
p <- analyze.simple(demo, burnin=20, treespace.points=50, file="demo.pdf", overwrite=TRUE, facet=TRUE)


#Let's set off some errors

# Should say all trees must have same tips
data(fungus)
run1$trees <- lapply(run1$trees,drop.tip,tip="Typhula_phacorhiza")
test <- check.chains(list(run1, run2))

# Various burnin and window.num errors
data(fungus)
analyze.simple(list(run1, run2), burnin= "x")
analyze.simple(list(run1, run2), burnin= -1)
analyze.simple(list(run1, run2), burnin= 2000)
analyze.simple(list(run1, run2), burnin=100, window.num = "x")
analyze.simple(list(run1, run2), burnin=100, window.num = -1)
analyze.simple(list(run1, run2), burnin=100, window.num =  500)
analyze.simple(list(run1, run2), burnin=100, treespace.points = "x")
analyze.simple(list(run1, run2), burnin=100, treespace.points = -1)
analyze.simple(list(run1, run2), burnin=100, treespace.points = 1000)
analyze.simple(list(run1, run2), burnin=100, min.freq="x")
analyze.simple(list(run1, run2), burnin=100, min.freq=-1)
analyze.simple(list(run1, run2), burnin=100, min.freq=5)
analyze.simple(list(run1, run2), burnin=100, filename="test.pdf", overwrite=TRUE) # This should run
analyze.simple(list(run1, run2), burnin=100, filename="test.pdf", overwrite=FALSE) # This should fail
