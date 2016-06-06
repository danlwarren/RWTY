#' Plotting all parameters
#' 
#' Plots all parameter values, including tree topologies (see makeplot.topology) over the length of the MCMC chain
#'
#' @param chains A set of rwty.trees objects 
#' @param burnin The number of trees to omit as burnin. 
#' @param facet Boolean denoting whether to make a facet plot.
#' @param free_y TRUE/FALSE to turn free y scales on the facetted plots on or off (default FALSE). Only works if facet = TRUE.
#' @param strip Number indicating which column to strip off (i.e., strip=1 removes first column, which is necessary for most MCMC outputs in which the first column is just the generation).
#' You can skip multiple columns by passing a vector of columns to skip, e.g., strip=c(1,4,6). 
#'
#' @return param.plot Returns a list of ggplot objects.
#'
#' @keywords parameter, plot, convergence, mcmc, phylogenetics
#'
#' @export makeplot.all.params
#' @examples
#' data(fungus)
#' makeplot.all.params(fungus, burnin=20)

makeplot.all.params <- function(chains, burnin = 0, facet=TRUE, free_y=FALSE, strip = 1){

    chains = check.chains(chains)
    chain = chains[[1]]
    if(is.null(chain$ptable)) stop("No parameters associated with your chains")

    params <- names(chain$ptable)[-strip]

    param.plots <- lapply(params, FUN = function(x) makeplot.param(param = x, burnin = burnin, chains = chains, facet = facet))

    t.plot = makeplot.topology(chains, burnin = burnin, facet = facet)

    param.plots[[length(param.plots)+1]] <- t.plot

    plot.names = c(paste(params, ".trace", sep=""), "topology.trace.plot")

    names(param.plots) <- plot.names

    return(param.plots)
}