#' analyze.rwty, the main interface for rwty analyses and plots.
#' 
#' This is the main user interface to rwty.  It allows users to conduct simple
#' visualizations of MCMC chain performance with  very few arguments.
#' 

#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin.  Default value is zero.
#' @param window.size The length of window (in trees) for the sliding window plot.  If no value is provided, RWTY selects a number so that 20 windows are analyzed over the chain. 
#' @param gens.per.tree The number of generations per tree in the .t file.  If no value is provided, RWTY will attempt to determine the number of generations from the tree names.  
#' @param treespace.points The number of trees to plot in the treespace plot. Default is 100 
#' @param min.freq The minimum frequency for a node to be used for calculating discordance. Default is zero.  
#' @param filename Name of an output file (e.g., "output.pdf").  If none is supplied, rwty will not print outputs to file.
#' @param overwrite Boolean variable saying whether output file should be overwritten, if it exists.
#'
#' @return output The output is a list containing the following plots:
#' 
#' Plots of likelihood and model parameters as a function of chain length (when p table is available).
#' 
#' Point and heatmap depictions of chains in treespace (points.plot and heatmap).
#' 
#' Sliding window posterior probability, depicting posterior probabilities of the most variable clades
#' over a series of non-overlapping windows along the MCMC chain.
#' 
#' Cumulative posterior probability, depicting the posterior probabilities of the most variable clades
#' as a function of chain length.
#' 
#' Sliding window variance, depicting the distribution of variances of clade posterior probability estimates
#' over a series of non-overlapping windows along the MCMC chain.
#' 
#' Cumulative variance, depicting the distribution of variances in posterior probability estimates as a function of 
#' chain length.
#' 
#' Compare plot (when multiple rwty.trees objects are passed): an xy plot (or several) of the correlation between posterior
#' estimates from multiple chains.
#' 
#' Discordance tree (when multiple rwty.trees objects are passed): a tree that depicts the similarity in posterior support 
#' values obtained by multiple chains.  Chains that produce similar posterior estimates will appear grouped, while those that 
#' support very different topologies (or posterior support values) will be more distant from each other.  
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' p <- analyze.simple(list(run1, run2), burnin = 50, window.num = 50)
#' p

analyze.simple <- function(chains, burnin=0, window.num=50, treespace.points = 100, min.freq = 0, labels=NA, likelihood.param = NA, filename = NA, overwrite=FALSE, ...){
    
    chains <- check.chains(chains, labels)
    
    N <- length(chains[[1]]$trees)
    
    rwty.params.check(chains, N, burnin, window.num, treespace.points, min.freq, filename, overwrite)

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

rwty.params.check <- function(chains, N, burnin, window.num, treespace.points, min.freq, filename, overwrite){
  # Checks for reasonable burnin
  if(!is.numeric(burnin)){
    stop("burnin must be numeric")
  }
  if(burnin < 0){
    stop("burnin must be zero or greater")
  }
  if(burnin > N){
    stop("burnin must be smaller than the number of trees in the chain")
  }
  
  # Checks for reasonable window.num
  if(!is.numeric(window.num)){
    stop("window.num must be numeric")
  }
  if(window.num < 2){
    stop("window.num must be 2 or greater")
  }
  if((N - burnin)/window.num < 2){
    stop("window.num cannot be more than half the number of post-burnin trees")
  }
  
  # Checks for reasonable treespace.points
  if(!is.numeric(treespace.points)){
    stop("treespace.points must be numeric")
  }
  if(treespace.points < 2){
    stop("treespace.points must be 2 or greater")
  }
  if((N - burnin) < treespace.points){
    stop("treespace.points cannot be more than the number of post-burnin trees")
  }
  
  # Checks for reasonable min.freq
  if(!is.numeric(min.freq)){
    stop("min.freq must be numeric")
  }
  if(min.freq < 0 || min.freq > 1){
    stop("min.freq must be between 0 and 1")
  }
  
  # Checks for output file
  if(!is.na(filename)){
    if(file.exists(filename) && overwrite==FALSE){
      stop("Output file exists and overwrite is set to FALSE")
    }
  } 
}