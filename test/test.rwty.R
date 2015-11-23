rm(list=ls())

library(testthat)
library(rwty)


# Load in test trees
testtrees <- load.trees("~/GitHub/RWTY/test/testchain.t", skip=0)

# Evaluate the trees object
expect_equal(names(testtrees), c("trees", "ptable", "gens.per.tree"))
expect_equal(length(names(testtrees$trees)), 16)
expect_equal(names(testtrees$trees), c("tree_100", "tree_200", "tree_300", "tree_400",
                                       "tree_500", "tree_600", "tree_700", "tree_800",
                                       "tree_900", "tree_1000", "tree_1100", "tree_1200",
                                       "tree_1300", "tree_1400", "tree_1500", "tree_1600"))
expect_equal(testtrees$gens.per.tree, 100)

# Get output from RWTY for test trees object
test.output <- analyze.rwty(testtrees, window.num=4, treespace.points=4, autocorr.intervals = 4)

# Show plots
test.output

# Test attributes of RWTY object
expect_equal(names(test.output), c("LnL", "Param1", "Param2", "heatmap", "points.plot", 
                                        "Chain.1 Sliding Window Posterior Probability",
                                        "Chain.1 Cumulative Posterior Probability",     
                                        "Chain.1 Sliding Window Variance", "Chain.1 Cumulative Variance",
                                        "ess.plot",  "autocorr.plot"))

# Test attributes of LnL object
LnL <- test.output$LnL
expect_equal(class(LnL), c("gg", "ggplot"))
expect_equal(names(LnL), c("data", "layers", "scales", "mapping", "theme", 
                           "coordinates", "facet","plot_env", "labels" ))
expect_equal(LnL$data$Gen, seq(100, 1600, by=100))
