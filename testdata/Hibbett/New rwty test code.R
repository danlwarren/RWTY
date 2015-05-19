setwd("~/GitHub/RWTY/testdata/Hibbett")

rfiles <- list.files(path = "~/GitHub/RWTY/rwty/R/", pattern="*.R", full.names=TRUE)
sapply(rfiles, source)
#install.packages("~/GitHub/RWTY/rwty",repos=NULL, type="source")
#library(rwty)
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

