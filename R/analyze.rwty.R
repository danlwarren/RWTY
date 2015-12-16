#' analyze.rwty, the main interface for rwty analyses and plots.
#' 
#' This is the main user interface to rwty.  It allows users to conduct simple
#' visualizations of MCMC chain performance with  very few arguments.
#' 

#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin.  Default value is zero.
#' @param window.size The number of trees to include in each windows of sliding window plots
#' @param treespace.points The number of trees to plot in the treespace plot. Default is 100 
#' @param n.clades The number of clades to include in plots of split frequencies over the course of the MCMC
#' @param min.freq The minimum frequency for a node to be used for calculating ASDSF. Default is 0.1  
#' @param labels A vector of names to apply to a list of chains. If nothing is specified, the chain names are used.
#' @param fill.color The name of a column in your log file that you would like to use as the fill colour of points in the treespace plots
#' @param filename Name of an output file (e.g., "output.pdf").  If none is supplied, rwty will not save outputs to file.
#' @param overwrite Boolean variable saying whether output file should be overwritten, if it exists.
#' @param facet A Boolean expression indicating whether multiple chains should be plotted as facet plots (default TRUE).
#' @param autocorr.intervals The maximum number of intervals to use for autocorrelation plots.
#' @param n The number of replicate analyses to do when calculating the pseudo ESS.
#' @param ... Extra arguments to be passed to plotting and analysis functions.
#'
#' @return output The output is a list containing the following plots:
#' 
#' Plots of likelihood, model parameters, and tree topologies as a function of chain length (the first two only when a p table is available).
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

analyze.rwty <- function(chains, burnin=0, window.size=20, treespace.points = 100, n.clades = 20,
                           min.freq = 0.0, labels=NA, fill.color = NA, filename = NA, 
                           overwrite=FALSE, facet=TRUE, autocorr.intervals=100, ess.reps = 20, ...){
    
    chains <- check.chains(chains, labels)
    
    N <- length(chains[[1]]$trees)
    
    rwty.params.check(chains, N, burnin, window.size, treespace.points, min.freq, filename, overwrite)
    
    # check to see if ptables exist, make related plots
    if(all(unlist(lapply(chains, function(x) length(x$ptable[,1])))) > 0){ 
      # plot parameters for all chains
      parameter.plots <- makeplot.all.params(chains, burnin = burnin, facet=facet, strip = 1)
    }
    else{
      parameter.plots <- makeplot.topology.trace(chains, burnin = burnin, facet = facet)
    }

    # plot autocorrelation
    autocorr.plot <- makeplot.autocorr(chains, burnin = burnin, autocorr.intervals = autocorr.intervals, facet = facet)

    # plot sliding window sf plots
    cladeprob.sliding <- makeplot.cladeprobs.sliding(chains, burnin=burnin, n.clades = n.clades, window.size = window.size, facet = facet)
    acsf.sliding <- makeplot.acsf.sliding(chains, burnin=burnin, window.size = window.size, facet = facet)

    # plot cumulative sf plots
    cladeprob.cumulative <- makeplot.cladeprobs.cumulative(chains, burnin=burnin, n.clades = n.clades, window.size = window.size, facet = facet)
    acsf.cumulative <- makeplot.acsf.cumulative(chains, burnin=burnin, window.size = window.size, facet = facet)

    # plot treespace for all chains
    treespace.plots <- makeplot.treespace(chains, n.points = treespace.points, burnin = burnin, fill.color = fill.color)


    plots <- c(parameter.plots,
                autocorr.plot,
                cladeprob.sliding,
                acsf.sliding,
                cladeprob.cumulative,
                acsf.cumulative,
                treespace.plots)
    
            
    # plot multichain plots when appropriate
    if(length(chains) > 1){
      
      asdsf.plot <- makeplot.asdsf(chains, burnin = burnin, window.size = window.size, min.freq = min.freq)

      splitfreq.matrix.plots <- makeplot.splitfreq.matrix(chains, burnin = burnin)

      plots <- c(plots, asdsf.plot, splitfreq.matrix.plots)
    }
    
    # Print all to pdf if filename provided
    if(!is.na(filename)){
      print(sprintf("Saving plots to file: %s", filename))
      pdf(file=filename, ...)
      print(plots)
      dev.off()
    }

    return(plots)

}

rwty.params.check <- function(chains, N, burnin, window.size, treespace.points, min.freq, filename, overwrite){
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
  if(!is.numeric(window.size)){
    stop("window.num must be numeric")
  }
  if(window.size < 1){
    stop("window.size must be 1 or greater")
  }
  if(as.integer((N - burnin) / window.size < 2)) {
    stop("window.size cannot be more than half the number of post-burnin trees")
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
      stop("You specified an output filename, but an output file already exists and overwrite is set to FALSE. Please check and try again.")
    }
  }
}