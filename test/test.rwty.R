rm(list=ls())

library(testthat)
library(rwty)


#############################################################
#
#     Single chain analysis
#
#############################################################

# Load in test trees
test.trees <- load.trees("~/GitHub/RWTY/test/testchain.t", skip=0)

# Evaluate the trees object
expect_equal(names(test.trees), c("trees", "ptable", "gens.per.tree"))
expect_equal(length(names(test.trees$trees)), 16)
expect_equal(names(test.trees$trees), c("tree_0", "tree_100", "tree_200", "tree_300", 
                                        "tree_400", "tree_500", "tree_600", "tree_700", 
                                        "tree_800", "tree_900", "tree_1000", "tree_1100", 
                                        "tree_1200", "tree_1300", "tree_1400", "tree_1500"))
expect_equal(test.trees$gens.per.tree, 100)

# Get output from RWTY for test trees object
test.output <- analyze.rwty(test.trees, window.num=4, treespace.points=4, autocorr.intervals = 4, burnin=0)

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
expect_equal(LnL$data$Gen, seq(0, 1500, by=100))
expect_equal(LnL$data$LnL, c(60, 50, 50, 50, 50, 50, 60, 50, 60, 50, 50, 50, 50, 50, 60, 50))


# Test attributes of Param1
Param1 <- test.output$Param1
expect_equal(class(Param1), c("gg", "ggplot"))
expect_equal(names(Param1), c("data", "layers", "scales", "mapping", "theme", 
                           "coordinates", "facet","plot_env", "labels" ))
expect_equal(Param1$data$Gen, seq(0, 1500, by=100))
expect_equal(Param1$data$Param1, c(8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7))


# Test attributes of Param2
Param2 <- test.output$Param2
expect_equal(class(Param2), c("gg", "ggplot"))
expect_equal(names(Param2), c("data", "layers", "scales", "mapping", "theme", 
                              "coordinates", "facet","plot_env", "labels" ))
expect_equal(Param2$data$Gen, seq(0, 1500, by=100))
expect_equal(Param2$data$Param2, rep(5, 16))


# Test attributes of heatmap
heatmap <- test.output$heatmap
expect_equal(names(heatmap), c("data", "layers", "scales", "mapping", "theme", 
                              "coordinates", "facet","plot_env", "labels" ))
expect_equal(heatmap$data$sample, c(1,5,9,13))


# Test attributes of points.plot
points.plot <- test.output$points.plot
expect_equal(names(points.plot), c("data", "layers", "scales", "mapping", "theme", 
                               "coordinates", "facet","plot_env", "labels" ))
expect_equal(heatmap$data$sample, c(1,5,9,13))


# Test attributes of slide posterior plot
slide.posterior.plot <- test.output[["Chain.1 Sliding Window Posterior Probability"]]
expect_equal(names(slide.posterior.plot), c("data", "layers", "scales", "mapping", "theme", 
                                   "coordinates", "facet","plot_env", "labels" ))

# Test attributes of cumulative posterior plot
cumulative.posterior.plot <- test.output[["Chain.1 Cumulative Posterior Probability"]]
expect_equal(names(cumulative.posterior.plot), c("data", "layers", "scales", "mapping", "theme", 
                                            "coordinates", "facet","plot_env", "labels" ))


# Test attributes of slide variance plot
slide.variance.plot <- test.output[["Chain.1 Sliding Window Variance"]]
expect_equal(names(slide.variance.plot), c("data", "layers", "scales", "mapping", "theme", 
                                            "coordinates", "facet","plot_env", "labels" ))

# Test attributes of cumulative variance plot
cumulative.variance.plot <- test.output[["Chain.1 Cumulative Variance"]]
expect_equal(names(cumulative.variance.plot), c("data", "layers", "scales", "mapping", "theme", 
                                                 "coordinates", "facet","plot_env", "labels" ))

# Test attributes of ess plot
ess.plot <- test.output$ess.plot
expect_equal(names(ess.plot), c("data", "layers", "scales", "mapping", "theme", 
                                "coordinates", "facet","plot_env", "labels" ))


# Test attributes of autocorr plot
autocorr.plot <- test.output$autocorr.plot
expect_equal(names(autocorr.plot), c("data", "layers", "scales", "mapping", "theme", 
                                      "coordinates", "facet","plot_env", "labels", "autocorr.k" ))




#############################################################
#
#     Multi-chain analysis
#
#############################################################


# Re-run test.trees with multiple tree files from load.multi
test.trees <- load.multi("~/GitHub/RWTY/test/", ext.tree = "t", skip=0)

# Evaluate the trees object
expect_equal(names(test.trees), c("testchain.t", "testchain2.t"))


# Evaluate contents of first chain
expect_equal(names(test.trees$testchain.t), c("trees", "ptable", "gens.per.tree"))
expect_equal(length(names(test.trees$testchain.t$trees)), 16)
expect_equal(names(test.trees$testchain.t$trees), c("tree_0", "tree_100", "tree_200", "tree_300", 
                                        "tree_400", "tree_500", "tree_600", "tree_700", 
                                        "tree_800", "tree_900", "tree_1000", "tree_1100", 
                                        "tree_1200", "tree_1300", "tree_1400", "tree_1500"))
expect_equal(test.trees$testchain.t$gens.per.tree, 100)

# Evaluate contents of second chain
expect_equal(names(test.trees$testchain2.t), c("trees", "ptable", "gens.per.tree"))
expect_equal(length(names(test.trees$testchain2.t$trees)), 16)
expect_equal(names(test.trees$testchain2.t$trees), c("tree_0", "tree_100", "tree_200", "tree_300", 
                                                    "tree_400", "tree_500", "tree_600", "tree_700", 
                                                    "tree_800", "tree_900", "tree_1000", "tree_1100", 
                                                    "tree_1200", "tree_1300", "tree_1400", "tree_1500"))
expect_equal(test.trees$testchain2.t$gens.per.tree, 100)


# Get output from RWTY for test trees object
test.output <- analyze.rwty(test.trees, window.num=4, treespace.points=4, autocorr.intervals = 4, burnin=0)

# Show plots
test.output

# Test attributes of RWTY object
expect_equal(names(test.output), c("LnL", "Param1", "Param2", "heatmap", "points.plot", 
                                   "testchain.t Sliding Window Posterior Probability",
                                   "testchain.t Cumulative Posterior Probability",     
                                   "testchain.t Sliding Window Variance", "testchain.t Cumulative Variance",
                                   "testchain2.t Sliding Window Posterior Probability",
                                   "testchain2.t Cumulative Posterior Probability",     
                                   "testchain2.t Sliding Window Variance", "testchain2.t Cumulative Variance",
                                   "ess.plot",  "autocorr.plot", "cumulative.asdsf", "compare.plot", "asdsf.tree"))

