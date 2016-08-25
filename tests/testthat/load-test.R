rm(list=ls(all = TRUE))

library(testthat)
library(rwty)

#############################################################
#
#     Testing file loading for single chain
#
#############################################################

# Load in test trees
test.trees <- load.trees("testchain.t", skip=0)

clade.freq(test.trees, 1, 16)

# 1  1 2 3 4 5 6 7 8 9 10       1.00
# 2              1 8 9 10       1.00
# 3        1 5 6 7 8 9 10       1.00
# 4      1 2 5 6 7 8 9 10       0.50
# 5        1 2 3 4 8 9 10       1.00
# 6      1 2 3 4 7 8 9 10       1.00
# 7         1 2 3 4 5 6 7       1.00
# 8       1 2 3 4 5 6 7 8       0.1875
# 9      1 3 5 6 7 8 9 10       0.50
# 10      1 2 3 4 5 6 7 9       0.8125

# Evaluate the trees object
expect_equal(names(test.trees), c("trees", "ptable", "gens.per.tree"))
expect_equal(length(names(test.trees$trees)), 181)

treenames <- paste0("tree_", seq(0, 18000, by = 100))

expect_equal(names(test.trees$trees), treenames)
expect_equal(test.trees$gens.per.tree, 100)

# Get output from RWTY for test trees object
test.output <- analyze.rwty(test.trees, window.num=4, treespace.points=4, autocorr.intervals = 4, max.sampling.interval = 4, burnin=0)

# Show plots
test.output

# Test attributes of RWTY object
expect_equal(names(test.output), c("LnL.trace", "Param1.trace", "Param2.trace", "topology.trace.plot",       
                                   "Chain.1.correlations", "splitfreqs.sliding.plot", "acsf.sliding.plot",
                                   "splitfreqs.cumulative.plot", "acsf.cumulative.plot", "treespace.heatmap",         
                                   "treespace.points.plot"))

# Test attributes of LnL object
LnL <- test.output$LnL.trace$trace
expect_equal(class(LnL), c("gg", "ggplot"))
expect_equal(names(LnL), c("data", "layers", "scales", "mapping", "theme", 
                           "coordinates", "facet","plot_env", "labels" ))
expect_equal(LnL$data$Gen, seq(0, 18000, by=100))


# Test attributes of Param1
Param1 <- test.output$Param1.trace$trace
expect_equal(class(Param1), c("gg", "ggplot"))
expect_equal(names(Param1), c("data", "layers", "scales", "mapping", "theme", 
                           "coordinates", "facet","plot_env", "labels" ))
expect_equal(Param1$data$Gen, seq(0, 18000, by=100))


# Test attributes of Param2
Param2 <- test.output$Param2.trace$trace
expect_equal(class(Param2), c("gg", "ggplot"))
expect_equal(names(Param2), c("data", "layers", "scales", "mapping", "theme", 
                              "coordinates", "facet","plot_env", "labels" ))
expect_equal(Param2$data$Gen, seq(0, 18000, by=100))



# Test attributes of heatmap
heatmap <- test.output$treespace.heatmap
expect_equal(class(heatmap), c("gg", "ggplot"))
expect_equal(names(heatmap), c("data", "layers", "scales", "mapping", "theme", 
                              "coordinates", "facet","plot_env", "labels" ))
expect_equal(heatmap$data$sample, c(1, 46, 91, 136, 181))


# Test attributes of points.plot
points.plot <- test.output$treespace.points.plot
expect_equal(class(points.plot), c("gg", "ggplot"))
expect_equal(names(points.plot), c("data", "layers", "scales", "mapping", "theme", 
                               "coordinates", "facet","plot_env", "labels" ))
expect_equal(heatmap$data$sample, c(1, 46, 91, 136, 181))


# Test attributes of slide posterior plot
slide.posterior.plot <- test.output$acsf.sliding.plot
expect_equal(class(slide.posterior.plot), c("gg", "ggplot"))
expect_equal(names(slide.posterior.plot), c("data", "layers", "scales", "mapping", "theme", 
                                   "coordinates", "facet","plot_env", "labels" ))

# Test attributes of cumulative posterior plot
cumulative.posterior.plot <- test.output$acsf.cumulative.plot
expect_equal(class(cumulative.posterior.plot), c("gg", "ggplot"))
expect_equal(names(cumulative.posterior.plot), c("data", "layers", "scales", "mapping", "theme", 
                                            "coordinates", "facet","plot_env", "labels" ))

# Test attributes of ess plot
ess.plot <- test.output$ess.plot
expect_equal(class(ess.plot), c("gg", "ggplot"))
expect_equal(names(ess.plot), c("data", "layers", "scales", "mapping", "theme", 
                                "coordinates", "facet","plot_env", "labels" ))

