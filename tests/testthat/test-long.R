rm(list=ls(all = TRUE))

library(testthat)
library(rwty)

#############################################################
#
#     Testing behavior on short, randomly generated chain
#
#############################################################

test.chain <- rep(list(compute.brlen(rtree(10)), compute.brlen(rtree(10)),
                       compute.brlen(rtree(10)),compute.brlen(rtree(10))), 100)
class(test.chain) <- "multiPhylo"
test.table <- data.frame(Gen = seq(100, 40000, by = 100),
                         LnL = rnorm(400),
                         Param1 = c(rep(1, 200), rep(0, 200)),
                           Param2 = c(rep(2, 200), rep(0, 200)))
test.rwty.chain <- list(trees = test.chain,
                        ptable = test.table,
                        gens.per.tree = 100)
class(test.rwty.chain) <- "rwty.chain"

test.output <- analyze.rwty(test.rwty.chain, window.size = 10)


# Show plots
test.output

# Test attributes of RWTY object
expect_equal(names(test.output), c("LnL.trace", "Param1.trace", "Param2.trace", "topology.trace.plot",       
                                   "Chain.1.correlations", "autocorr.plot", "splitfreqs.sliding.plot", 
                                   "acsf.sliding.plot", "splitfreqs.cumulative.plot", "acsf.cumulative.plot", 
                                   "treespace.heatmap", "treespace.points.plot"))

# Test attributes of LnL object
LnL <- test.output$LnL.trace$trace
expect_equal(class(LnL), c("gg", "ggplot"))
expect_equal(names(LnL), c("data", "layers", "scales", "mapping", "theme", 
                           "coordinates", "facet","plot_env", "labels" ))
expect_equal(LnL$data$Gen, seq(100, 40000, by=100))


# Test attributes of Param1
Param1 <- test.output$Param1.trace$trace
expect_equal(class(Param1), c("gg", "ggplot"))
expect_equal(names(Param1), c("data", "layers", "scales", "mapping", "theme", 
                              "coordinates", "facet","plot_env", "labels" ))
expect_equal(Param1$data$Gen, seq(100, 40000, by=100))


# Test attributes of Param2
Param2 <- test.output$Param2.trace$trace
expect_equal(class(Param2), c("gg", "ggplot"))
expect_equal(names(Param2), c("data", "layers", "scales", "mapping", "theme", 
                              "coordinates", "facet","plot_env", "labels" ))
expect_equal(Param2$data$Gen, seq(100, 40000, by=100))


# Test attributes of points.plot
points.plot <- test.output$treespace.points.plot
expect_equal(class(points.plot), c("gg", "ggplot"))
expect_equal(names(points.plot), c("data", "layers", "scales", "mapping", "theme", 
                                   "coordinates", "facet","plot_env", "labels" ))
expect_equal(points.plot$data$sample, seq(1, 397, by = 4))
expect_equal(points.plot$data$generation, seq(0, 39600, by = 400))

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

