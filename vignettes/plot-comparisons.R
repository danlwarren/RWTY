## ---- message=FALSE------------------------------------------------------
library(rwty)
data(salamanders)
data(fungus)
rwty.processors <- 1

## ----eval = FALSE, message=FALSE-----------------------------------------
#  salamanders.rwty <- analyze.rwty(salamanders, burnin = 0)
#  fungus.rwty <- analyze.rwty(fungus, burnin = 0)

## ----make-ptables-1, fig.width=8, fig.height=6, message=FALSE------------
colnames(fungus$Fungus.Run1$ptable)
makeplot.param(fungus, burnin = 0, "LnL")

## ----make-ptables-2, fig.width=8, fig.height=6, message=FALSE------------
makeplot.param(fungus, burnin = 50, "LnL")

## ----make-ptables-3, fig.width=8, fig.height=6, message=FALSE------------
makeplot.param(salamanders, burnin = 50, "LnL")

## ----make-ptables-4, fig.width=8, fig.height=6, message=FALSE------------
salamanders.amotl <- list(salamanders[[1]], salamanders[[2]])
makeplot.param(salamanders.amotl, burnin = 50, "LnL")

## ----make-ptables-5, fig.width=8, fig.height=6, message=FALSE------------
makeplot.param(fungus, burnin = 50, "pi.A.")

## ----topo-plots-1, fig.width=8, fig.height=6, message=FALSE--------------
makeplot.topology(salamanders.amotl, burnin = 50)

## ----topo-plots-2, fig.width=8, fig.height=6, message=FALSE--------------
makeplot.topology(fungus, burnin = 50)

## ----splitfreq-1, fig.width=8, fig.height=6, message=FALSE---------------
makeplot.splitfreqs.cumulative(fungus, burnin = 50)

## ----splitfreq-2, fig.width=8, fig.height=10, message=FALSE--------------
makeplot.splitfreqs.cumulative(salamanders, burnin = 50)

## ----splitfreq-3, fig.width=8, fig.height=10, message=FALSE--------------
makeplot.splitfreqs.sliding(salamanders, burnin = 50)

## ----splitfreq-4, fig.width=8, fig.height=6, message=FALSE---------------
makeplot.splitfreqs.sliding(fungus, burnin = 50)

## ----pairs-1, fig.width=8, fig.height=6, message=FALSE-------------------
makeplot.pairs(salamanders[[1]], burnin = 50, params = c("LnL", "pi.A.", "pi.C."))

## ----pairs-2, fig.width=8, fig.height=6, message=FALSE-------------------
makeplot.pairs(fungus[[1]], burnin = 50, params = c("LnL", "pi.A.", "pi.C."))

## ----acsf-1, fig.width=8, fig.height=6, message=FALSE--------------------
makeplot.acsf.cumulative(salamanders, burnin = 50)

## ----acsf-2, fig.width=8, fig.height=6, message=FALSE--------------------
makeplot.acsf.cumulative(fungus, burnin = 50)

## ----acsf-3, fig.width=8, fig.height=6, message=FALSE--------------------
makeplot.acsf.sliding(salamanders, burnin = 50)

## ----acsf-4, fig.width=8, fig.height=6, message=FALSE--------------------
makeplot.acsf.sliding(fungus, burnin = 50)

## ----treespace-1, fig.width=8, fig.height=6, message=FALSE---------------
makeplot.treespace(salamanders, burnin =50, fill.color = "LnL")

## ----treespace-2, fig.width=8, fig.height=6, message=FALSE---------------
makeplot.treespace(salamanders.amotl, burnin =50, fill.color = "LnL")

## ----treespace-3, fig.width=8, fig.height=6, message=FALSE---------------
makeplot.treespace(fungus, burnin =50, fill.color = "LnL")

## ----treespace-4, fig.width=8, fig.height=6, message=FALSE---------------
my.treespace <- makeplot.treespace(fungus, burnin =50, fill.color = "LnL")
qplot(x, y, data = my.treespace$treespace.points.plot$data, color = chain) +theme_bw()

## ----autocorr-1, fig.width = 8, fig.height = 6, message = FALSE----------
makeplot.autocorr(salamanders, burnin = 0)

## ----autocorr-2, fig.width = 8, fig.height = 6, message = FALSE----------
makeplot.autocorr(fungus, burnin = 0)

## ----splitfreq-matrix-1, fig.width = 8, fig.height = 6, message = FALSE----
makeplot.splitfreq.matrix(salamanders, burnin = 50)

## ----splitfreq-matrix-2, fig.width = 8, fig.height = 6, message = FALSE----
makeplot.splitfreq.matrix(fungus, burnin = 50)

## ----asdsf-1, fig.width = 8, fig.height = 6, message = FALSE-------------
makeplot.asdsf(salamanders.amotl, burnin = 50)

## ----asdsf-2, fig.width = 8, fig.height = 6, message = FALSE-------------
makeplot.asdsf(salamanders, burnin = 50)

## ----asdsf-3, fig.width = 8, fig.height = 6, message = FALSE-------------
makeplot.asdsf(fungus, burnin = 50)

