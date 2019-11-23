#' Make autocorrelation plots of tree topologies from MCMC analyses
#' 
#' This function takes a list of rwty.chain objects, and makes an
#' autocorrelation plot for each chain. Each plot shows the mean phylogenetic
#' distance at a series of sampling intervals. In really well behaved MCMC 
#' analyses, the mean distance will stay constant as the sampling interval 
#' increases. If there is autocorrelation, the mean distance will 
#' increase as the sampling interval increases, and is expected to level
#' off when the autocorrelation decreases to zero. The function calculates
#' path distances, though other distances could also be employed.
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin.
#' @param autocorr.intervals The number of sampling intervals to use. These will be spaced evenly between 1 and the max.sampling.interval 
#' @param max.sampling.interval The largest sampling interval for which you want to calculate the mean distance between pairs of trees (default is the larger of 10 percent of the length of the chain, or the sampling interval that gives at least 100 paired samples).
#' @param facet TRUE/FALSE to turn facetting of the plot on or off (default FALSE)
#' @param free_y TRUE/FALSE to turn free y scales on the facetted plots on or off (default FALSE). Only works if facet = TRUE.
#'
#' @return A ggplot2 plot object, with one line (facetting off) or facet
#' (facetting on) per rwty.chain object.
#'
#' @keywords autocorrelation, path distance
#'
#' @export makeplot.autocorr
#' @examples
#' \dontrun{
#' data(fungus)
#' makeplot.autocorr(fungus, burnin = 20)
#' }

makeplot.autocorr <- function(chains, burnin = 0, max.sampling.interval = NA, autocorr.intervals = 40, facet = FALSE, free_y = FALSE){

    print(sprintf("Creating topological autocorrelation plot"))

    chains = check.chains(chains)

    chain = chains[[1]]

    N = length(chain$trees)

    if(is.na(max.sampling.interval)){
        max.sampling.interval = max(c(floor((N - burnin) * 0.1), N - burnin - 100))
    }

    dat <- topological.autocorr(chains, burnin, max.sampling.interval, autocorr.intervals)

    ylabel = sprintf("Mean %s distance between pairs of trees", chain$tree.dist.metric)
    
    autocorr.plot = ggplot(data=dat, aes(x=sampling.interval, y=topo.distance)) + 
            geom_line(aes(colour = chain)) + geom_point(aes(colour = chain)) + 
            scale_color_viridis(discrete = TRUE, end = 0.85, option = "C") + 
            xlab("Sampling Interval between Trees") + 
            ylab(ylabel) + 
            ggtitle("Topological autocorrelation plot")

    if(facet){ 
        if(free_y){

            autocorr.plot = autocorr.plot + facet_wrap(~chain, ncol=1, scales = "free_y") + theme(legend.position="none")

        }else{

            autocorr.plot = autocorr.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")

        }
    }
    
    autocorr.plot <- list(autocorr.plot = autocorr.plot)
    
    return(autocorr.plot)
    
}
