#' Analyze.rwty, the main interface for rwty analyses and plots.
#' 
#' This is the main user interface to rwty.  It allows users to chuck in arguments for
#' chains, burnin, window size, gens per tree, and "step", and returns an object that
#' contains sliding window and cumulative posterior probability plots, treespace plots,
#' and multi-chain diagnostic plots when multiple chains are provided.
#'
#' @param chains A list of rwty.trees objects. \code{chains}
#' @param burnin The number of trees to eliminate as burnin.  Default value is zero. \code{burnin}
#' @param window.size The length of window (in trees) for the sliding window plot.  If no value is provided, RWTY selects a number so that 20 windows are analyzed over the chain. \code{window.size}
#' @param gens.per.tree The number of generations per tree in the .t file.  If no value is provided, RWTY will attempt to determine the number of generations from the tree names.  \code{gens.per.tree}
#' @param treespace.points The number of trees to plot in the treespace plot. Default is 100 \code{treespace.points}
#' @param min.freq The minimum frequency for a node to be used for calculating discordance. Default is zero.  \code{min.freq}
#'
#' @return output A list of outputs from the analyze.single runs on each chain, as well as a compare.n run for all chains.  Eventually we will add more multi-chain analyses.
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' data(fungus)

analyze.simple <- function(chains, burnin=0, window.num=50, treespace.points = 100, min.freq = 0, labels=NA, likelihood.param = NA, ...){
    
    chains = check.chains(chains, labels)

    N = length(chains[[1]]$trees)

    # Now merge the ptables into one large data frame, keeping only what we want 
    ptable = merge.ptables(chains)

    # plot parameters for all chains
    parameter.plots = plot.all.params(chains, burnin = burnin, facet=TRUE, strip = 1)

    # plot treespace for all chains
    treespace.plots = plot.treespace(chains, n.points = treespace.points, burnin = burnin, likelihood = likelihood.param)


    plots = c(parameter.plots, treespace.plots)

    return(plots)

}