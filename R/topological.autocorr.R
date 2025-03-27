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
#' @param squared TRUE/FALSE use squared tree distances (necessary to calculate approximate ESS)
#' @param treedist the type of tree distance metric to use, can be 'PD' for path distance or 'RF' for Robinson Foulds distance
#' @param use.all.samples (TRUE/FALSE). Whether to calculate autocorrelation from all possible pairs of trees in your chain. The default is FALSE, in which case 500 samples are taken at each sampling interval. This is sufficient to get reasonably accurate estimates of the approximate ESS. Setting this to TRUE will give you slightly more accurate ESS estimates, at the cost of potentially much longer execution times.
#'
#' @return A data frame with one row per sampling interval, per chain. 
#' The first column is the sampling interval. The second column is the mean 
#' path distance between pairs of trees from that sampling interval. The third
#' column is the chain ID.
#'
#' @keywords autocorrelation distance
#'
#' @export topological.autocorr
#' @examples
#' \dontrun{
#' data(fungus)
#' topological.autocorr(fungus, burnin = 20)
#' }


topological.autocorr <- function(chains, burnin = 0, max.sampling.interval = NA, autocorr.intervals = 100, squared = FALSE, treedist = 'PD', use.all.samples = FALSE){
  
  chains = check.chains(chains)
  
  chain = chains[[1]]
  
  N = length(chains[[1]]$trees)
  
  if(is.na(max.sampling.interval)){
    max.sampling.interval = max(floor((N - burnin) * 0.1), 20)
  }
  
  indices = seq(from = burnin + 1, to = N, by = 1)   
  
  trees = lapply(chains, function(x) x[['trees']][indices])
  
  raw.autocorr = lapply(trees, tree.autocorr, max.sampling.interval, autocorr.intervals, squared, treedist, use.all.samples)
  
  final.autocorr = do.call("rbind", raw.autocorr)
  
  final.autocorr$chain = unlist(lapply(names(chains), rep, nrow(raw.autocorr[[1]])))
  
  rownames(final.autocorr) = NULL
  
  return(final.autocorr)
  
  
}


tree.autocorr <- function(tree.list, max.sampling.interval = NA, autocorr.intervals = 100, squared = FALSE, treedist = 'PD', use.all.samples = FALSE){
  
  if(!is.numeric(autocorr.intervals)) stop("autocorr.intervals must be a positive integer")
  if(autocorr.intervals<1 | autocorr.intervals%%1!=0) stop("autocorr.intervals must be a positive integer")
  
  # this ensures that we can tell you if your ESS is < some threshold
  # the max(,2) bit is a fallback for extremely short tree lists
  max.thinning <- max.sampling.interval
  
  if(max.thinning > (length(tree.list) - 100)) {
    max.thinning = length(tree.list) - 100
  }
  
  if(max.thinning < 20){
    stop(paste("Too few trees to calculate autocorrelation. Try including more trees in the sample, or changing the max.sampling.interval. Current value is", 
               max.sampling.interval, ", minimum value is 20."))
  }
  
  # we analyze up to autocorr.intervals thinnings spread evenly, less if there are non-unique numbers
  thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=autocorr.intervals)))
  
  
  r <- lapply(as.list(thinnings), get.sequential.distances, tree.list, squared = squared, treedist = treedist, use.all.samples = use.all.samples) 
  r <- data.frame(matrix(unlist(r), ncol=2, byrow=T))
  names(r) = c("topo.distance", "sampling.interval")
  
  return(r)
}

path.distance <- function(tree1, tree2){
  
  pair = c(1,2)
  trees = list(tree1, tree2)
  return(path.dist(pair, list(tree1, tree2)))
  
}


rf.distance <- function(tree1, tree2){
  pair = c(1,2)
  trees = list(tree1, tree2)
  
  return(rf.dist(pair, list(tree1, tree2)))
  
}

rf.dist.squared <- function(pair, trees){
  
  rf = rf.dist(pair, trees)
  
  return(rf*rf)
  
}


rf.dist <- function(pair, trees){
  
  tree1 = trees[[pair[1]]]
  tree2 = trees[[pair[2]]]
  rf = RF.dist(tree1, tree2)
  return(rf)
  
}

path.dist.squared <- function (pair, trees, check.labels = FALSE){
  
  pd = path.dist(pair, trees, check.labels)
  
  return(pd*pd)
  
}


path.dist <- function (pair, trees, check.labels = FALSE) 
{
  
  # a trimmed down version of the phangorn tree.dist function
  tree1 = trees[[pair[1]]]
  tree2 = trees[[pair[2]]]
  
  
  tree1 = reorder(tree1, "postorder")
  tree2 = reorder(tree2, "postorder")
  
  path.difference = NULL
  
  tree1$edge.length = rep(1, nrow(tree1$edge))
  tree2$edge.length = rep(1, nrow(tree2$edge))
  
  # the commented code uses phangorn, which is faster
  # but the coph function is not exported, so we use
  # ape instead.
  #dt1 = phangorn:::coph(tree1)
  #dt2 = phangorn:::coph(tree2)
  
  dt1 = cophenetic.phylo(tree1)
  dt2 = cophenetic.phylo(tree2)
  dt1[upper.tri(dt1)] = 0
  dt2[upper.tri(dt2)] = 0
  
  path.difference = sqrt(sum((dt1 - dt2)^2))
  
  return(path.difference)
}


get.sequential.distances <- function(thinning, tree.list, N=500, squared = FALSE, treedist = 'PD', use.all.samples = FALSE){
  
  
  processors = get.processors(NULL)
  
  starts = 1:(length(tree.list) - thinning)
  ends = starts + thinning
  keep = c(rbind(starts, ends))
  
  pairs = split(keep, ceiling(seq_along(keep)/2))
  
  
  if(use.all.samples == FALSE){
    if(length(pairs)>N){
      # only subsample if we have enough...
      pairs = sample(pairs, N)
    }
  }
  
  if(treedist == 'PD'){
    if(squared == TRUE){
      distances <- mclapply(pairs, path.dist.squared, trees = tree.list, mc.cores = processors)        
    }else{
      distances <- mclapply(pairs, path.dist, trees = tree.list, mc.cores = processors )
    }
  }else if(treedist == 'RF'){
    if(squared == TRUE){
      distances <- mclapply(pairs, rf.dist.squared, trees = tree.list, mc.cores = processors)        
    }else{
      distances <- mclapply(pairs, rf.dist, trees = tree.list, mc.cores = processors)
    }
  }else{
    stop("Unknown option for treedist. Valid options are 'PD' (for path distance) or 'RF' (for Robinson Foulds distance). Please try again")
  }
  
  distances <- as.numeric(unlist(distances))
  distances <- as.data.frame(distances)
  result <- apply(distances, 2, mean)
  result <- data.frame('distance' = t(t(result)))
  result$sampling.interval <- thinning
  return(result)
}


