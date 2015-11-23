#' Calculate sampling interval based on exponential semivariogram model.
#' 
#' This function uses an exponential semivariogram model to estimate the asymptotic topological distance, and
#' uses that to estimate the sampling interval at which topological distances have reached some fixed
#' proportion of that value (default 0.95).  It expects as input a data table output by rwty's topological.autocorr function
#' 
#'
#' @param dat A data frame output from topological.autocorr.
#' @param ac.cutoff The proportion of the asymptotic topological distance to use as a cutoff for determining sampling interval.
#'
#' @return A data frame consisting of the k value matching the ac.cutoff proportion of the asymptotic 
#' topological distance for each chain.  This sampling interval estimates the interval at which topological distances
#' are no longer autocorrelated.
#' 
#' @keywords autocorrelation, path distance
#'
#' @export estimate.autocorr.m
#' @examples
#' data(fungus)
#' # To get a good estimate we need all sampling intervals
#' max.intervals = as.integer(length(fungus[[1]]$trees)/21)
#' sampling.table <- topological.autocorr(fungus, burnin = 20, max.intervals = max.intervals)
#' estimate.autocorr.m(sampling.table, ac.cutoff = 0.9)


estimate.autocorr.m <- function(dat, ac.cutoff = 0.95){
  
  # Build an empty data frame
  autocorr.m <- data.frame(autocorr.time = rep(NA, length(unique(dat$chain))), row.names = unique(dat$chain))
  
  # Loop over chains, calculate k
  for(i in 1:nrow(autocorr.m)){
    
    # Break out a single-chain copy of the data table for model fitting
    thischain <- rownames(autocorr.m)[i]
    thisdata <- dat[dat$chain == thischain,]
    
    # Fit an exponential variogram
    this.ac.result <- optim(par = c(1, 1), 
                            function(data, par){sum((par[1] * (1 - exp(-(data$sampling.interval/par[2]))) - data$topo.distance)^2)}, 
                            data = thisdata)
    
    # If the cutoff is exceeded within the set of sampling intervals, return the interval
    # Else return "> Max"
    if(any(thisdata$topo.distance/this.ac.result$par[1] > ac.cutoff)){
      autocorr.m[i,1] <- thisdata$sampling.interval[min(which(thisdata$topo.distance/this.ac.result$par[1] > ac.cutoff))]
    } else {
      autocorr.m[i,1] <- "> Max"
    }
  }

  autocorr.m$chains = rownames(autocorr.m)
  rownames(autocorr.m) = NULL

  return(autocorr.m)
}