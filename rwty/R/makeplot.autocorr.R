#' Make autocorrelation plots of tree topologies from MCMC analyses
#' 
#' This function takes a list of rwty.trees objects, and makes an
#' autocorrelation plot for each chain. Each plot shows the median phylogenetic
#' distance at a series of sampling intervals. In really well behaved MCMC 
#' analyses, the median distance will stay constant as the sampling interval 
#' increases. If there is autocorrelation, the median distance will 
#' increase as the sampling interval increases, and is expected to level
#' off when the autocorrelation decreases to zero. The function calculates
#' path distances, though other distances could also be employed.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin.
#' @param max.intervals The maximum number of sampling intervals to use.
#' @param ac.cutoff The proportion of the estimated asymptotic path distance to use as a cutoff for estimating minimum sampling interval.  For instance, if ac.cutoff = 0.9, the sampling interval returned will be the minimum sampling interval necessary to achieve a path distance of 0.9 times the estimated asymptotic value.
#' @param facet TRUE/FALSE to turn facetting of the plot on or off (default)
#'
#' @return A ggplot2 plot object, with one line (facetting off) or facet
#' (facetting on) per rwty.trees object, and a data frame containing estimated minimum sampling intervals for each chain.
#'
#' @keywords autocorrelation, path distance
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' makeplot.autocorr(fungus, burnin = 100)

makeplot.autocorr <- function(chains, burnin = 0, max.intervals = 100, ac.cutoff = 0.95, facet = FALSE){

    chains = check.chains(chains)

    dat <- topological.autocorr(chains, burnin, max.intervals)

    autocorr.plot = ggplot(data=dat, aes(x=sampling.interval, y=Path.distance)) + 
            geom_line(alpha=0.2, aes(colour = chain)) + geom_point(size = 2, aes(colour = chain)) + 
            xlab("sampling interval") + ylab("median path distance") + 
            theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5))

    if(facet) autocorr.plot = autocorr.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")
    
    #' This bit uses an exponential semivariogram model to estimate the asymptotic path distance, and
    #' uses that to estimate the sampling interval at which path distances have reached some fixed
    #' proportion of that value (default 0.95).
    autocorr.interval <- data.frame(autocorr.time = rep(NA, length(unique(dat$chain))), row.names = unique(dat$chain))
    for(i in 1:nrow(autocorr.interval)){
      thischain <- rownames(autocorr.interval)[i]
      thisdata <- dat[dat$chain == thischain,]
      this.ac.result <- optim(par = c(1, 1), 
                              function(data, par){sum((par[1] * (1 - exp(-(data$sampling.interval/par[2]))) - data$Path.distance)^2)}, 
                              data = thisdata)
      if(any(thisdata$Path.distance/this.ac.result$par[1] > ac.cutoff)){
        autocorr.interval[i,1] <- min(which(thisdata$Path.distance/this.ac.result$par[1] > ac.cutoff))
      } else {
        autocorr.interval[i,1] <- "> Max"
      }
    }

    autocorr.plot <- list(autocorr.plot = autocorr.plot, autocorr.interval = autocorr.interval)
    
    return(autocorr.plot)
    
}
