globalVariables(c("lower.95", "upper.95", "lower.75", "upper.75", "Generation", "ACSF",
                  "Chain", "Clade", "title", "Generations", "par", "cor", "text", "sd",
                  "generation", "chain", "x", "y", "strwidth", "mtext", "Split.Frequency",
                  "hclust", "..density..", "recordPlot", "rgb", "panel.smooth", "ci.lower",
                  "ci.upper", "median", "quantile", "median.ess", "ASDSF" ,"Axis" ,"CSF" ,"Gen" ,
                  "as.dist" ,"box" ,"cmdscale" ,"dev.flush" ,"dev.hold" ,"dev.off" ,"ess" ,
                  "optim" ,"pdf" ,"plot" ,"points" ,"read.table" ,"reorder" ,"sampling.interval",
                  "split.frequency" ,"tail" ,"topo.distance" ,"topological.distance", 
                  "dev.control"))

#' analyze.rwty, the main interface for rwty analyses and plots.
#' 
#' This is the main user interface to rwty.  It allows users to conduct simple
#' visualizations of MCMC chain performance with  very few arguments.
#' 
#' @import ape
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom phangorn RF.dist
#' @importFrom coda effectiveSize mcmc
#' @importFrom viridis scale_color_viridis scale_fill_viridis viridis plasma
#' @importFrom grid unit
#' @importFrom plyr ddply summarize . 
#' @importFrom ggdendro ggdendrogram
#' @importFrom GGally ggpairs
#' @importFrom parallel mclapply detectCores
#' @importFrom utils citation
#' @importFrom usedist dist_get
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin.  Default value is zero.
#' @param window.size The number of trees to include in each windows of sliding window plots
#' @param treespace.points The number of trees to plot in the treespace plot. Default is 100 
#' @param n.clades The number of clades to include in plots of split frequencies over the course of the MCMC
#' @param min.freq The minimum frequency for a node to be used for calculating ASDSF. Default is 0.1  
#' @param fill.color The name of a column in your log file that you would like to use as the fill colour of points in the treespace plots
#' @param filename Name of an output file (e.g., "output.pdf").  If none is supplied, rwty will not save outputs to file.
#' @param overwrite Boolean variable saying whether output file should be overwritten, if it exists.
#' @param facet A Boolean expression indicating whether multiple chains should be plotted as facet plots (default TRUE).
#' @param free_y TRUE/FALSE to turn free y scales on the facetted plots on or off (default FALSE). Only works if facet = TRUE.
#' @param autocorr.intervals The maximum number of intervals to use for autocorrelation plots.
#' @param ess.reps The number of replicate analyses to do when calculating the pseudo ESS.
#' @param treedist the type of tree distance metric to use, can be 'PD' for path distance or 'RF' for Robinson Foulds distance.
#' @param params A vector of parameters to use when making the parameter correlation plots.  Defaults to the first two columns in the log table.
#' @param max.sampling.interval The maximum sampling interval to use for generating autocorrelation plots
#' @param ... Extra arguments to be passed to plotting and analysis functions.
#'
#' @return output The output is a list containing the following plots:
#' 
#' Plots of likelihood, model parameters, and tree topologies as a function of chain length (the first two only when output from MCMC parameters has been loaded along with the tree topologies).
#' 
#' Plot of autocorrelation of tree topolgies at different sampling intervals along a chain 
#' 
#' Plot of split frequencies calculated in sliding windows for the most variable clades
#' 
#' Plot of change in split frequencies between sliding windows for all clades
#' 
#' Plot of cumulative split frequencies as the MCMC progresses
#' 
#' Plot of change in cumulative split frequencies as the MCMC progresses
#' 
#' Heatmap and point depictions of chains in treespace.
#' 
#' Plot of the Average Standard Deviation of Split Frequencies (ASDSF) between chains as the MCMC progresses
#'
#' Plot of pairwise correlations between split frequencies among chains  
#'
#' Plot of chains clustered by their pairwise ASDSF values 
#'
#' @keywords plots, rwty, MCMC, topology, ESS
#'
#' @export analyze.rwty
#' @examples
#' \dontrun{
#' data(fungus)
#' p <- analyze.rwty(fungus, burnin = 50, window.num = 50)
#' p
#' }



analyze.rwty <- function(chains, burnin=0, window.size=20, treespace.points = 100, n.clades = 20,
                         min.freq = 0.0, fill.color = NA, filename = NA, 
                         overwrite=FALSE, facet=TRUE, free_y=FALSE, autocorr.intervals=100, ess.reps = 20,
                         treedist = 'PD', params = NA, max.sampling.interval = NA, ...){
  
  chains <- check.chains(chains)
  
  N <- length(chains[[1]]$trees)
  
  rwty.params.check(chains, N, burnin, window.size, treespace.points, min.freq, filename, overwrite)
  
  # check to see if ptables exist, make related plots
  if(all(unlist(lapply(chains, function(x) length(x$ptable[,1])))) > 0){ 
    # plot parameters for all chains
    parameter.plots <- makeplot.all.params(chains, burnin = burnin, facet=facet, strip = 1)
    parameter.correlations <- makeplot.pairs(chains, burnin = burnin, params = params, treedist = treedist, strip = 1)
    names(parameter.correlations) <- paste0(names(parameter.correlations), ".correlations")
  }
  else{
    parameter.plots <- makeplot.topology(chains, burnin = burnin, facet = facet)
    parameter.correlations <- NA
  }
  
  # plot autocorrelation
  if(N < 200){
    autocorr.plot <- NULL
  } else {
    autocorr.plot <- makeplot.autocorr(chains, burnin = burnin, autocorr.intervals = autocorr.intervals, facet = facet, max.sampling.interval = max.sampling.interval) 
  }
  
  # plot sliding window sf plots
  splitfreq.sliding <- makeplot.splitfreqs.sliding(chains, burnin=burnin, n.clades = n.clades, window.size = window.size, facet = facet)
  acsf.sliding <- makeplot.acsf.sliding(chains, burnin=burnin, window.size = window.size, facet = facet)
  
  # plot cumulative sf plots
  splitfreq.cumulative <- makeplot.splitfreqs.cumulative(chains, burnin=burnin, n.clades = n.clades, window.size = window.size, facet = facet)
  acsf.cumulative <- makeplot.acsf.cumulative(chains, burnin=burnin, window.size = window.size, facet = facet)
  
  # plot treespace for all chains
  treespace.plots <- makeplot.treespace(chains, n.points = treespace.points, burnin = burnin, fill.color = fill.color)
  
  # Add citations for all packages
  citations <- list(
    citation('rwty'),
    citation('ape'),
    citation('phangorn'),
    citation('ggplot2'),
    citation('coda'),
    citation('viridis'),
    citation('ggdendro'),
    citation('GGally'),
    citation('plyr'),
    citation('reshape2'),
    citation('stats')
  )
  
  
  plots <- c(parameter.plots,
             parameter.correlations,
             autocorr.plot,
             splitfreq.sliding,
             acsf.sliding,
             splitfreq.cumulative,
             acsf.cumulative,
             treespace.plots)
  
  
  # plot multichain plots when appropriate
  if(length(chains) > 1){
    
    asdsf.plot <- makeplot.asdsf(chains, burnin = burnin, window.size = window.size, min.freq = min.freq)
    
    splitfreq.matrix.plots <- makeplot.splitfreq.matrix(chains, burnin = burnin)
    
    plots <- c(plots, asdsf.plot, splitfreq.matrix.plots)
  }
  
  plots[["citations"]] <- citations
  
  # Print all to pdf if filename provided
  if(!is.na(filename)){
    print(sprintf("Saving plots to file: %s", filename))
    pdf(file=filename, width = 10, height = 7, ...)
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



get.processors <- function(processors){

  if(Sys.info()["sysname"] == 'Windows'){
    # mclapply is not supported on windows
    # so we give a single processor,
    # in which case mclapply calls fall back
    # on lapply
    return(1)
  }

  # check for global user-defined variable
  if(exists('rwty.processors')){
    # should be an integer
    if(!is.numeric(rwty.processors)){
      stop("the global rwty.processors variable must be an integer")
    }
    if(rwty.processors%%1==0){
      available_processors = detectCores(all.tests = FALSE, logical = FALSE)
      if(rwty.processors > available_processors){
        rwty.processors = available_processors - 1
      }
      if(rwty.processors < 1){
        rwty.processors = 1
      }
      return(rwty.processors)
    }else{
      stop("the global rwty.processors variable must be an integer")
    }
  }

  
  if(is.null(processors)){ 
    available_processors = detectCores(all.tests = FALSE, logical = FALSE)
    processors = max(c(1, c(available_processors - 1)))
  }
  
  return(processors)
  
}
