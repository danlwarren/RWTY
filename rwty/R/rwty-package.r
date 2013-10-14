#' R We There Yet?  An R package for visualizing convergence in MCMC phylogenetics.
#' 
#' This package implements various tests, visualizations, and metrics
#' for diagnosing convergence of MCMC chains in phylogenetics.  It implements
#' and automates many of the functions of the AWTY package in the R environment.
#'
#' @keywords Phylogenetics, mcmc, convergence, awty
#'
#' @export
#' 
#' @examples
#' See documentation for analyze.rwty
#'
#' @name rwty
#' @docType package

library(ape)
library(ggplot2)
library(reshape2)
library(phangorn)
library(MASS)
library(GGally)

source("analyze.multi.R")
source("analyze.rwty.R")
source("analyze.single.R")
source("clade.freq.R")
source("compare.n.R")
source("continuous.distance.R")
source("continuous.ess.R")
source("cummean.R")
source("cumulative.freq.R")
source("get.sequential.distance.R")
source("get.sequential.distances.c.R")
source("load.trees.R")
source("mds.treespace.R")
source("p.from.ranked.list.R")
source("pairwise.RF.R")
source("parse.clades.R")
source("plawty.R")
source("plot.cladeprobs.R")
source("plot.cladevar.R")
source("abs.diffs.R")
source("plot.tree.ess.R")
source("plot.treespace.R")
source("slide.freq.R")
source("tree.distance.R")
source("tree.dist.matrix.R")
source("tree.ess.R")
source("treespace.single.R")

