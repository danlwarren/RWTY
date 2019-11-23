#' Calculate the approximate Effective Sample Size (ESS) of tree topologies
#'
#' This function takes a list of rwty.chain objects, and calculates the
#' pseudo ESS of the trees from each chain, after removing burnin.
#' The calculation uses the autocorrelation among squared topological distances between
#' trees to calculate an approximate ESS of tree topologies for each chain.
#' NB this function requires the calculation of many many
#' tree distances, so can take some time.
#'
#' @param chains A list of rwty.chain objects.
#' @param burnin The number of trees to eliminate as burnin
#' @param max.sampling.interval The largest sampling interval you want to use to calculate the ESS. Every sampling interval up to and including this number will be sampled. Higher is better, but also slower. In general, setting this number to 100 (the default) should be fine for most cases. However, if you get an upper bound on the ESS estimate (i.e. ESS<x) rather than a point estimate (i.e. ESS = x) then that indicates a higher max.sampling.interval would be better, because the algorithm could not find the asymptote on the autocorrelation plot with the current max.sampling.interval.
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



topological.approx.ess <- function(chains, burnin = 0, max.sampling.interval = 100){

    chains = check.chains(chains)

    if(inherits(chains, "list")){
      N = length(chains[[1]]$trees)
    } else {
      N = length(chains$trees)
    }

    if(N-burnin < max.sampling.interval){
        warning("Not enough trees to use your chosen max.sampling.interval")
        warning("Setting it to 90% of the length of your post-burnin chain instead")
        max.sampling.interval = floor((N - burnin) * 0.9)
    }


    # set them equal, so we get every interval
    autocorr.intervals = max.sampling.interval

    print(sprintf("Calculating approximate ESS with sampling intervals from 1 to %d", max.sampling.interval))

    autocorr.df = topological.autocorr.squared(chains, burnin, max.sampling.interval, autocorr.intervals)

    autocorr.m = estimate.autocorr.m(autocorr.df)

    approx.ess.df = approx.ess.multi(autocorr.df, autocorr.m, (N-burnin))

    approx.ess.df$distance.metric = chains[[1]]$tree.dist.metric
    
    return(approx.ess.df)

}


topological.autocorr.squared <- function(chains, burnin = 0, max.sampling.interval = NA, autocorr.intervals = 100){
  
  #identical to the topological.autocorr function but tree distances are squared first, see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5010905/
  
  chain = chains[[1]]
  
  N = length(chains[[1]]$trees)
  
  if(is.na(max.sampling.interval)){
    max.sampling.interval = max(floor((N - burnin) * 0.1), 20)
  }
  
  indices = seq(from = burnin + 1, to = N, by = 1)   
  
  # here we square all the distance matrices to use in the ESS calculation
  dist.matrices = lapply(chains, function(x) x[['tree.dist.matrix']]^2)
  
  raw.autocorr = lapply(dist.matrices, tree.autocorr, max.sampling.interval, autocorr.intervals)
  
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
