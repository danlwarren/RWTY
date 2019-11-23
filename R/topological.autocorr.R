#' Calculate data for autocorrelation plots of tree topologies from MCMC analyses
#' 
#' This function takes a list of rwty.chain objects, and calculates the
#' mean phylogenetic distance at a series of roughly even sampling intervals.
#' In really well behaved MCMC analyses, the mean distance will stay constant
#' as the sampling interval increases. If there is autocorrelation, it will 
#' increase as the sampling interval increases, and is expected to level
#' off when the autocorrelation decreases to zero. The function calculates
#' path distances, though other distances could also be employed.
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param autocorr.intervals The number of sampling intervals to use. These will be spaced evenly between 1 and the max.sampling.interval 
#' @param max.sampling.interval The largest sampling interval for which you want to calculate the mean distance between pairs of trees (default is 10 percent of the length of the list of trees).
#'
#' @return A data frame with one row per sampling interval, per chain. 
#' The first column is the sampling interval. The second column is the mean 
#' path distance between pairs of trees from that sampling interval. The third
#' column is the chain ID.
#'
#' @keywords autocorrelation, path distance
#'
#' @export topological.autocorr
#' @examples
#' \dontrun{
#' data(fungus)
#' topological.autocorr(fungus, burnin = 20)
#' }


topological.autocorr <- function(chains, burnin = 0, max.sampling.interval = NA, autocorr.intervals = 100){
  
  chains = check.chains(chains)
  
  chain = chains[[1]]
  
  N = length(chains[[1]]$trees)
  
  if(is.na(max.sampling.interval)){
    max.sampling.interval = max(floor((N - burnin) * 0.1), 20)
  }
  
  indices = seq(from = burnin + 1, to = N, by = 1)   
  
  dist.matrices = lapply(chains, function(x) x[['tree.dist.matrix']])
  
  raw.autocorr = lapply(dist.matrices, tree.autocorr, max.sampling.interval, autocorr.intervals)
  
  final.autocorr = do.call("rbind", raw.autocorr)
  
  final.autocorr$chain = unlist(lapply(names(chains), rep, nrow(raw.autocorr[[1]])))
  
  rownames(final.autocorr) = NULL
  
  return(final.autocorr)
  
  
}


tree.autocorr <- function(dist.mat, max.sampling.interval = NA, autocorr.intervals = 100){
  
  if(!is.numeric(autocorr.intervals)) stop("autocorr.intervals must be a positive integer")
  if(autocorr.intervals<1 | autocorr.intervals%%1!=0) stop("autocorr.intervals must be a positive integer")
  
  # this ensures that we can tell you if your ESS is < some threshold
  # the max(,2) bit is a fallback for extremely short tree lists
  max.thinning <- max.sampling.interval
  
  n.samples = sqrt(2*length(dist.mat)+0.25)+0.5
  
  if(max.thinning > (n.samples - 100)) {
    max.thinning = n.samples - 100
  }
  
  if(max.thinning < 20){
    stop(paste("Too few trees to calculate autocorrelation. Try including more trees in the sample, or changing the max.sampling.interval. Current value is", 
               max.sampling.interval, ", minimum value is 20."))
  }
  
  # we analyze up to autocorr.intervals thinnings spread evenly, less if there are non-unique numbers
  thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=autocorr.intervals)))
  
  r <- lapply(as.list(thinnings), get.sequential.distances, dist.mat) 
  r <- data.frame(matrix(unlist(r), ncol=2, byrow=T))
  names(r) = c("topo.distance", "sampling.interval")
  
  return(r)
}



get.sequential.distances <- function(thinning, dist.mat){
  
  # dist.mat is an object of class 'dist'
  
  n.samples = sqrt(2*length(dist.mat)+0.25)+0.5
  
  starts = 1:(n.samples - thinning)
  ends = starts + thinning
  keep = c(rbind(starts, ends))
  
  pairs = split(keep, ceiling(seq_along(keep)/2))
  
  # calculate the mean distance from all pairs of trees at this thinning
  # as a simple list of distances
  distances = dist_get(dist.mat, starts, ends)
  
  result <- data.frame('distance' = mean(distances))
  result$sampling.interval <- thinning
  return(result)
}


