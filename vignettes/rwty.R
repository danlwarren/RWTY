## ----setup1, eval=FALSE--------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("danlwarren/RWTY")
#  library(rwty)

## ----setup2, eval=FALSE--------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_local("PATH")
#  library(rwty)

## ----setproc, eval=FALSE-------------------------------------------------
#  rwty.processors <<- 8

## ----loadtrees1, eval=FALSE----------------------------------------------
#  my.trees <- load.trees("PATH")
#  
#  my.beast.trees <- load.trees("PATH", format = "beast", trim = 5)
#  
#  my.starbeast.trees <- load.trees("PATH", format = "*beast")

## ----loadtrees2, eval=FALSE----------------------------------------------
#  my.trees <- load.trees("path_to_tree_file", type = "nexus", logfile = "path_to_log_file", skip = 1, gens.per.tree = 1)

## ----loadtrees3, eval=FALSE----------------------------------------------
#  my.trees <- load.trees("~/Desktop/trees.phy", type = "newick", gens.per.tree = 1)

## ----loadmulti, eval=FALSE-----------------------------------------------
#  my.trees <- load.multi("PATH", format = "mb")

## ----analyze.salamanders, results='hide', fig.keep='none', eval=FALSE, message=FALSE, warning=FALSE----
#  data(salamanders)
#  salamanders.rwty <- analyze.rwty(salamanders, burnin=50, fill.color = 'LnL')
#  
#  # to see which plots you have
#  names(salamanders.rwty)

## ----makeplot.params, results='hide', fig.keep='none', eval=FALSE, message=FALSE, warning=FALSE----
#  makeplot.all.params(salamanders, burnin=0) # the LnL trace suggests burnin should be >0
#  
#  makeplot.all.params(salamanders, burnin=50) # this looks OK

## ----topo.ess, eval=FALSE------------------------------------------------
#  approx.ess <- topological.approx.ess(salamanders, burnin = 50)

## ----param.trace, eval=FALSE, fig.width=8, fig.height=6------------------
#  salamanders.rwty$pi.C

## ----param.cor, eval=FALSE, fig.width=8, fig.height=6--------------------
#  salamanders.rwty$AMOTL2.run1.correlations

## ----param.cor2, eval=FALSE, fig.width=8, fig.height=6-------------------
#  data(fungus)
#  plotparams <- c("LnL", "pi.A.", "pi.T.")
#  makeplot.pairs(fungus$Fungus.Run1, burnin = 20, params = plotparams)

## ----topo.trace, eval=FALSE, fig.width=8, fig.height=6-------------------
#  salamanders.rwty$topology.trace

## ----autocorr.plot, eval=FALSE, fig.width=8, fig.height=6----------------
#  salamanders.rwty$autocorr.plot

## ----treespace.1, eval=FALSE, fig.width=8, fig.height=6------------------
#  salamanders.rwty$treespace.heatmap
#  salamanders.rwty$treespace.points.plot

## ----treespace.2, eval=FALSE, fig.width=8, fig.height=6------------------
#  salamanders.treespace = makeplot.treespace(salamanders[1:2], burnin = 50, n.points = 200, fill.color = "LnL")
#  
#  salamanders.treespace$treespace.heatmap
#  salamanders.treespace$treespace.points.plot

## ----splitfreq.slide, eval=FALSE, fig.width=8, fig.height=6--------------
#  salamanders.rwty$splitfreqs.sliding.plot
#  salamanders.rwty$splitfreqs.cumulative.plot

## ----splitfreq.cumulative, eval=FALSE, fig.width=8, fig.height=6---------
#  makeplot.splitfreqs.cumulative(salamanders[[1]])
#  makeplot.splitfreqs.sliding(salamanders[[1]])

## ----acsf.slide, eval=FALSE, fig.width=8, fig.height=6-------------------
#  salamanders.rwty$acsf.sliding.plot
#  salamanders.rwty$acsf.cumulative.plot

## ----asdsf, eval=FALSE, fig.width=8, fig.height=6------------------------
#  makeplot.asdsf(salamanders[c(1,2)])

## ----splitfreq.matrix, eval=FALSE, fig.width=8, fig.height=6-------------
#  salamanders.rwty$splitfreq.matrix

## ----asdsf.tree, eval=FALSE, fig.width=8, fig.height=6-------------------
#  salamanders.rwty$asdsf.tree

