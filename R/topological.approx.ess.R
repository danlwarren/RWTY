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
#'
#' @return A data frame with one row per chain, and columns describing the
#' approximate ESS and the name of the chain. 
#'
#' @keywords treespace, tree distance, path distance
#'
#' @export topological.approx.ess
#' @examples
#' data(fungus)
#' topological.approx.ess(fungus, burnin = 20)



topological.approx.ess <- function(chains, burnin = 0, autocorr.intervals = NA){

    chains = check.chains(chains)

    # to do this we need every possible interval
    # this interval means the minimum number of samples is 20
    N = length(chains[[1]]$trees)

    if(is.na(autocorr.intervals)){
      autocorr.intervals = as.integer(N/21)
    } 

    autocorr.df = topological.autocorr(chains, burnin, autocorr.intervals, squared = TRUE)

    autocorr.m = estimate.autocorr.m(autocorr.df)

    approx.ess.df = approx.ess.multi(autocorr.df, autocorr.m, N)

    return(approx.ess.df)

}


approx.ess.multi <- function(autocorr.df, autocorr.m, N){
    # estimate the approximate ESS from squared topological distances,
    # using a single chain


    approx.ess.df = data.frame(approx.ess = rep(NA, length(unique(autocorr.df$chain))), chain = unique(autocorr.df$chain))
  
    # Loop over chains, calculate approx ess
    for(i in 1:nrow(approx.ess.df)){
    
        thischain = approx.ess.df$chain[i]
        thism = autocorr.m$autocorr.time[autocorr.m$chain == thischain]
        thisdata = autocorr.df[autocorr.df$chain == thischain,]
        
        
        ess = approx.ess.single(thisdata, thism, N)

        approx.ess.df$approx.ess[approx.ess.df$chain == thischain] = ess
    
  }

  return(approx.ess.df)

}


approx.ess.single <- function(df, autocorr.time, N){
    # many thanks to Xia Hua and David Bryant for help with this

    # if the autocorrelation time is larger than the maximum sample
    # our it is recorded as -1
    if(autocorr.time < 0){
        m = nrow(df)
    }else{
        m = autocorr.time
    }

    D = mean(df$topo.distance[df$sampling.interval>=m])
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
        ESS = paste("<", M)
    }

    return(ESS)
}
