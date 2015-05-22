#' analyze.rwty, the main interface for rwty analyses and plots.
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
#' p <- analyze.simple(list(run1, run2), burnin = 50, window.num = 50)
#' p

analyze.simple <- function(chains, burnin=0, window.num=50, treespace.points = 100, min.freq = 0, labels=NA, likelihood.param = NA, filename = NA, ...){
    
    chains <- check.chains(chains, labels)

    N <- length(chains[[1]]$trees)

    # Now merge the ptables into one large data frame, keeping only what we want 
    ptable <- merge.ptables(chains, burnin = burnin)

    # plot parameters for all chains
    parameter.plots <- makeplot.all.params(chains, burnin = burnin, facet=TRUE, strip = 1)

    # plot treespace for all chains
    treespace.plots <- makeplot.treespace(chains, n.points = treespace.points, burnin = burnin, likelihood = likelihood.param)
    
    # plot posterior probabilities for all chains
    posterior.plots <- makeplot.posteriors(chains, burnin=burnin, window.num = window.num)
    
    # plot multichain plots when appropriate, populate plots list
    if(length(chains) > 1){
      multichain.plots <- makeplot.multichain(chains, burnin, min.freq, ...)
      plots <- c(parameter.plots, treespace.plots, posterior.plots, multichain.plots)
    }
    else{
      plots <- c(parameter.plots, treespace.plots, posterior.plots)
    }
    
    # Print all to pdf if filename provided
    if(!is.na(filename)){
      pdf(file=filename)
      print(plots)
      dev.off()
    }

    return(plots)

}