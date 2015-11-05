#' analyze.rwty, the main interface for rwty analyses and plots.
#' 
#' This is the main user interface to rwty.  It allows users to conduct simple
#' visualizations of MCMC chain performance with  very few arguments.
#' 

#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin.  Default value is zero.
#' @param window.num The number of windows for the posterior plots.
#' @param labels A vector of names to apply to a list of chains.
#' @param likelihood.param The name of the likelihood column in the log file.
#' @param facet A Boolean expression indicating whether multiple chains should be plotted as facet plots.
#' @param ess.reps The number of replicate trees to use as the focal tree for estimating confidence in topological ESS estimates.
#' @param autocorr.intervals The number of intervals to use for autocorrelation plots.
#' @param treespace.points The number of trees to plot in the treespace plot. Default is 100 
#' @param min.freq The minimum frequency for a node to be used for calculating ASDSF. Default is 0.1  
#' @param filename Name of an output file (e.g., "output.pdf").  If none is supplied, rwty will not print outputs to file.
#' @param overwrite Boolean variable saying whether output file should be overwritten, if it exists.
#' @param ... Extra arguments to be passed to plotting and analysis functions.
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
#' @export analyze.rwty
#' @examples
#' data(fungus)
#' p <- analyze.rwty(fungus, burnin = 50, window.num = 50)
#' p

analyze.rwty <- function(chains, burnin=0, window.num=50, treespace.points = 100, 
                           min.freq = 0.1, labels=NA, likelihood.param = NA, filename = NA, 
                           overwrite=FALSE, facet=TRUE, ess.reps=50, autocorr.intervals=100, ...){
    
    chains <- check.chains(chains, labels)
    
    N <- length(chains[[1]]$trees)
    
    rwty.params.check(chains, N, burnin, window.num, treespace.points, min.freq, filename, overwrite)
    
    # check to see if ptables exist, make related plots
    if(all(unlist(lapply(chains, function(x) length(x$ptable[,1])))) > 0){
      # Now merge the ptables into one large data frame, keeping only what we want 
      ptable <- combine.ptables(chains, burnin = burnin)
  
      # plot parameters for all chains
      parameter.plots <- makeplot.all.params(chains, burnin = burnin, facet=facet, strip = 1)
    }
    else{
      parameter.plots <- NULL
    }

    # plot treespace for all chains
    treespace.plots <- makeplot.treespace(chains, n.points = treespace.points, burnin = burnin, likelihood = likelihood.param)
    
    # plot posterior probabilities for all chains
    posterior.plots <- makeplot.posteriors(chains, burnin=burnin, window.num = window.num)
    
    # plot ESS and confidence intervals
    ess.plots <- makeplot.ess(chains, burnin, ess.reps)
    
    # plot autocorrelation
    autocorr.plots <- makeplot.autocorr(chains, burnin, autocorr.intervals, facet)
    
    # plot multichain plots when appropriate, populate plots list
    if(length(chains) > 1){
      multichain.plots <- makeplot.multichain(chains, burnin, window.num, min.freq, ...)
      plots <- c(parameter.plots, treespace.plots, posterior.plots, ess.plots, autocorr.plots, multichain.plots)
    }
    else{
      plots <- c(parameter.plots, treespace.plots, posterior.plots, ess.plots, autocorr.plots)
    }
    
    # Print all to pdf if filename provided
    if(!is.na(filename)){
      pdf(file=filename, ...)
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