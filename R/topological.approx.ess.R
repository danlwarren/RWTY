#' Calculate the approximate Effective Sample Size (ESS) of tree topologies
#' 
#' This function takes a list of rwty.trees objects, and calculates the
#' pseudo ESS of the trees from each chain, after removing burnin. 
#' The calculation uses the autocorrelation among squared topological distances between
#' trees to calculate an approximate ESS of tree topologies for each chain.
#' NB this function requires the calculation of many many
#' tree distances, so can take some time.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param max.sampling.interval The largest sampling interval you want to use to calculate the ESS. Every sampling interval up to and including this number will be sampled. Higher is better, but also slower. In general, setting this number to 100 (the default) should be fine for most cases. However, if you get an upper bound on the ESS estimate (i.e. ESS<x) rather than a point estimate (i.e. ESS = x) then that indicates a higher max.sampling.interval would be better, because the algorithm could not find the asymptote on the autocorrelation plot with the current max.sampling.interval.
#' @param treedist the type of tree distance metric to use, can be 'PD' for path distance or 'RF' for Robinson Foulds distance
#' @param use.all.samples (TRUE/FALSE). Whether to calculate autocorrelation from all possible pairs of trees in your chain. The default is FALSE, in which case 500 samples are taken at each sampling interval. Setting this to TRUE will give you slightly more accurate ESS estimates, at the cost of potentially much longer execution times.
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



topological.approx.ess <- function(chains, burnin = 0, max.sampling.interval = 100, treedist = 'PD', use.all.samples = FALSE){

    chains = check.chains(chains)

    N = length(chains[[1]]$trees)

    if(N-burnin < max.sampling.interval){
        warning("Not enough trees to use your chosen max.sampling.interval")
        warning("Setting it to 90% of the length of your post-burnin chain instead")
        max.sampling.interval = floor((N - burnin) * 0.9)
    }


    # set them equal, so we get every interval.
    autocorr.intervals = max.sampling.interval

    print(sprintf("Calculating approximate ESS with sampling intervals from 1 to %d", max.sampling.interval))

    autocorr.df = topological.autocorr(chains, burnin, max.sampling.interval, autocorr.intervals, squared = TRUE, treedist = treedist, use.all.samples = use.all.samples)

    autocorr.m = estimate.autocorr.m(autocorr.df)

    approx.ess.df = approx.ess.multi(autocorr.df, autocorr.m, (N-burnin))

    return(approx.ess.df)

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
