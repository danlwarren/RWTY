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
