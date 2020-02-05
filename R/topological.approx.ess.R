#' Calculate the approximate Effective Sample Size (ESS) of tree topologies
#'
#' This function takes a list of rwty.chain objects, and calculates the
#' approximate ESS of the trees from each chain, after removing burnin.
#' The calculation uses the autocorrelation among squared topological distances between
#' trees to calculate an approximate ESS of tree topologies for each chain.
#' NB: if all post-burnin trees are identical, the function will return an ESS value that equals the number of post burnin samples.
#'
#' @param chains A list of rwty.chain objects.
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
#'
#' @return A data frame with one row per chain, and columns describing the
#' approximate ESS and the name of the chain.
#'
#' @keywords treespace, tree distance, path distance
#'
#' @export topological.approx.ess
#' @examples
#' \dontrun{
#' data(fungus)
#' topological.approx.ess(fungus, burnin = 20)
#' }



topological.approx.ess <- function(chains, burnin = NA){

    chains = check.chains(chains)

    # set burnin to the maximum from across all chains
    if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
    
    N = length(chains[[1]]$trees)
    
    # choose a max sampling interval such that we have minimum 100 trees to get the mean topo distance from in each bin
    max.sampling.interval = N - burnin - 100

    if(max.sampling.interval < 10){
      warning(sprintf("There are only %d trees left after removing burnin, which is not enough to use the method of Lanfear et al 2016 to calculate the topological ESS. Instead, RWTY simply estimates that the maximum possible ESS of trees in your chains is %d", N-burnin, N-burnin))
      result = data.frame("operator" = "<", "approx.ess" = N-burnin, "chain" = names(chains), "distance.metric" = chains[[1]]$tree.dist.metric)
      return(result)
    }
    
    # now make the max sampling interval such that we only do enough intervals to detect a minimum ESS of 5
    if(max.sampling.interval>(N-burnin)/5 & (N-burnin)/5 > 20){
      max.sampling.interval = as.integer((N-burnin)/5)
    }
    
    # if we get to here, we have at least 10 intervals from which to calculate the approx.ess, so we can go ahead
    print(sprintf("Calculating approximate ESS with sampling intervals from 1 to %d", max.sampling.interval))

    autocorr.df = topological.autocorr.squared(chains, burnin, max.sampling.interval)

    autocorr.m = estimate.autocorr.m(autocorr.df)

    approx.ess.df = approx.ess.multi(autocorr.df, autocorr.m, (N-burnin))

    approx.ess.df$distance.metric = chains[[1]]$tree.dist.metric
    
    return(approx.ess.df)

}

  

topological.autocorr.squared <- function(chains, burnin, max.sampling.interval){
  
  #identical to the topological.autocorr function but tree distances are squared first, see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5010905/
  
  chain = chains[[1]]
  
  N = length(chains[[1]]$trees)

  # here we square all the distance matrices to use in the ESS calculation
  dist.matrices = lapply(chains, function(x) x[['tree.dist.matrix']]^2)
  
  raw.autocorr = lapply(dist.matrices, tree.autocorr, max.sampling.interval, burnin)
  
  final.autocorr = do.call("rbind", raw.autocorr)
  
  final.autocorr$chain = unlist(lapply(names(chains), rep, nrow(raw.autocorr[[1]])))
  
  rownames(final.autocorr) = NULL
  
  return(final.autocorr)
  
}


approx.ess.multi <- function(autocorr.df, autocorr.m, N){
    # estimate the approximate ESS from squared topological distances,
    # using a single chain

    r = length(unique(autocorr.df$chain))

    approx.ess.df = data.frame(operator = rep(NA, r), approx.ess = rep(NA, r), chain = unique(autocorr.df$chain))

    # Loop over chains, calculate approx ess
    for(i in 1:nrow(approx.ess.df)){

        thischain = approx.ess.df$chain[i]
        thism = autocorr.m$autocorr.time[autocorr.m$chain == thischain]
        thisdata = autocorr.df[autocorr.df$chain == thischain,]

        ess.info = approx.ess.single(thisdata, thism, N)

        ess = ess.info$ess
        operator = ess.info$operator

        approx.ess.df$approx.ess[approx.ess.df$chain == thischain] = ess
        approx.ess.df$operator[approx.ess.df$chain == thischain] = operator

  }

  return(approx.ess.df)

}


approx.ess.single <- function(df, autocorr.time, N){
    # many thanks to Xia Hua and David Bryant for help with this

    # if the autocorrelation time is larger than the maximum sample
    # our it is recorded as -1
  
    if(autocorr.time < 0){
        m = nrow(df) + 1
    }else{
        m = autocorr.time
    }

    D = max(df$topo.distance)
    
    if(D==0){
      ESS = N
      operator = "="
      return(list("ess" = ESS, "operator" = operator))
    }
    
    S = 0

    if(m>1){
        for(k in 1:(m - 1)){
            f = df$topo.distance[k]
            S = S + ((N - k) * f)
        }
    }

    S = S + (N - m + 1) * (N - m) * D / 2
    S = S / 2 / N^2
    ESS = 1 / (1 - 4 * S / D)

    # sometimes we can only give an upper bound
    if(autocorr.time<0){
        operator = "<"
    }else{
        operator = "="
    }

    return(list("ess" = ESS, "operator" = operator))
}
