#' R We There Yet?  An R package for visualizing convergence in MCMC phylogenetics.
#' 
#' This package implements various tests, visualizations, and metrics
#' for diagnosing convergence of MCMC chains in phylogenetics.  It implements
#' and automates many of the functions of the AWTY package in the R environment.
#' It also adds a whole bunch of new functionality.
#'
#' @keywords Phylogenetics, mcmc, convergence, awty
#'
#' @export rwty
#' 
#' @examples
#' \dontrun{
#' data(fungus)
#' analyze.rwty(fungus, burnin=50)
#' }
#'
#' @name rwty
#' @docType package