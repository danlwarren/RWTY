#' Calculate data for autocorrelation plots of tree topologies from MCMC analyses
#' 
#' This function takes a list of rwty.chain objects, and calculates the
#' mean phylogenetic distance between pairs of trees at a range of sampling intervals.
#' In really well behaved MCMC analyses, the mean distance will stay constant
#' as the sampling interval increases. If there is autocorrelation, it will 
#' increase as the sampling interval increases, and is expected to level
#' off when the autocorrelation decreases to zero. 
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
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


topological.autocorr <- function(chains, burnin = NA){
  
  chains = check.chains(chains)
  
  chain = chains[[1]]
  
  if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
  
  N = length(chains[[1]]$trees)
  
  # choose a max sampling interval such that we have minimum 100 trees to get the mean topo distance from in each bin
  max.sampling.interval = N - burnin - 100
  
  # now make the max sampling interval such that we only do enough intervals to detect a minimum ESS of 5
  if(max.sampling.interval>(N-burnin)/5){
    max.sampling.interval = as.integer((N-burnin)/5)
  }
  
  if(max.sampling.interval < 1){
    stop(sprintf("Only %d trees remain after removing burnin, which is not enough to calculate any topological autocorrelation"))
  }
  
  dist.matrices = lapply(chains, function(x) x[['tree.dist.matrix']])
  
  raw.autocorr = lapply(dist.matrices, tree.autocorr, max.sampling.interval, burnin)
  
  final.autocorr = do.call("rbind", raw.autocorr)
  
  final.autocorr$chain = unlist(lapply(names(chains), rep, nrow(raw.autocorr[[1]])))
  
  rownames(final.autocorr) = NULL
  
  return(final.autocorr)
  
}


tree.autocorr <- function(dist.mat, max.sampling.interval, burnin){
  
  if(!is.numeric(max.sampling.interval)) stop("autocorr.intervals must be a positive integer")
  if(max.sampling.interval<1 | max.sampling.interval%%1!=0) stop("autocorr.intervals must be a positive integer")
  
  # we analyze up to autocorr.intervals thinnings spread evenly, less if there are non-unique numbers
  thinnings <- unique(as.integer(seq(from = 1, to = max.sampling.interval, length.out=max.sampling.interval)))
  
  r <- lapply(as.list(thinnings), get.sequential.distances, dist.mat, burnin) 
  r <- data.frame(matrix(unlist(r), ncol=2, byrow=T))
  names(r) = c("topo.distance", "sampling.interval")
  
  return(r)
}



get.sequential.distances <- function(thinning, dist.mat, burnin){
  
  # dist.mat is an object of class 'dist'
  
  n.samples = sqrt(2*length(dist.mat)+0.25)+0.5
  
  starts = (burnin+1):(n.samples - thinning)
  ends = starts + thinning

  # calculate the mean distance from all pairs of trees at this thinning
  # as a simple list of distances
  distances = dist_get(dist.mat, starts, ends)
  
  result <- data.frame('distance' = mean(distances))
  result$sampling.interval <- thinning
  return(result)
}


