#' Calculate sampling interval based on exponential semivariogram model.
#' 
#' This function uses an exponential semivariogram model to estimate the asymptotic path distance, and
#' uses that to estimate the sampling interval at which path distances have reached some fixed
#' proportion of that value (default 0.95).  It expects as input a data table output by rwty's
#' 
#'
#' @param dat A data frame output from topological.autocorr.
#' @param ac.cutoff The proportion of the asymptotic path distance to use as a cutoff for determining sampling interval.
#'
#' @return A data frame consisting of the k value matching the ac.cutoff proportion of the asymptotic 
#' path distance for each chain.  This sampling interval estimates the interval at which path distances
#' are no longer autocorrelated.
#' 
#' @keywords autocorrelation, path distance
#'
#' @export autocorr.k
#' @examples
#' data(fungus)
#' sampling.table <- topological.autocorr(fungus, burnin = 20, max.intervals = 40)
#' estimate.autocorr.k(sampling.table, ac.cutoff = 0.9)


estimate.autocorr.k <- function(dat, ac.cutoff = 0.95){
  
  # Build an empty data frame
  autocorr.k <- data.frame(autocorr.time = rep(NA, length(unique(dat$chain))), row.names = unique(dat$chain))
  for(i in 1:nrow(autocorr.k)){
    thischain <- rownames(autocorr.k)[i]
    thisdata <- dat[dat$chain == thischain,]
    this.ac.result <- optim(par = c(1, 1), 
                            function(data, par){sum((par[1] * (1 - exp(-(data$sampling.interval/par[2]))) - data$Path.distance)^2)}, 
                            data = thisdata)
    if(any(thisdata$Path.distance/this.ac.result$par[1] > ac.cutoff)){
      autocorr.k[i,1] <- thisdata$sampling.interval[min(which(thisdata$Path.distance/this.ac.result$par[1] > ac.cutoff))]
    } else {
      autocorr.k[i,1] <- "> Max"
    }
  }
  return(autocorr.k)
}