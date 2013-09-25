library(ape) #note that this needs to be >=3.0-10
library(phytools) # note that the right version for this is not yet on cran, but here: http://www.phytools.org/nonstatic/phytools_0.3-53.tar.gz
library(ggplot2)
library(reshape2)
library(phangorn)
library(MASS)
source("clade.freq.R")
source("cumulative.freq.R")
source("slide.freq.R")
source("plawty.R")
source("compare.two.R")
source("compare.n.R")
source("tree.ess.R")
source("tree.dist.matrix.R")
source("mds.treespace.R")

# test