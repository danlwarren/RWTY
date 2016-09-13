rm(list=ls(all = TRUE))

library(testthat)
library(rwty)

#############################################################
#
#     Testing behavior on short, randomly generated chain
#
#############################################################

test.chain <- rep(list(compute.brlen(rtree(10)), compute.brlen(rtree(10))), 10)
class(test.chain) <- "multiPhylo"
test.table <- data.frame(Param.1 = c(rep(1, 10), rep(0, 10)),
                           Param.2 = c(rep(2, 10), rep(0, 10)))
test.rwty.chain <- list(trees = test.chain,
                        ptable = test.table,
                        gens.per.tree = 100)
class(test.rwty.chain) <- "rwty.chain"

expect_error(analyze.rwty(test.rwty.chain), 
             'window.size cannot be more than half the number of post-burnin trees')

expect_error(analyze.rwty(test.rwty.chain, window.size = 4), 
             'treespace.points cannot be more than the number of post-burnin trees')

