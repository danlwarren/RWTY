#' Make autocorrelation plots of tree topologies from MCMC analyses
#' 
#' This function takes a list of rwty.trees objects, and makes an
#' autocorrelation plot for each chain. Each plot shows the mean phylogenetic
#' distance at a series of sampling intervals. In really well behaved MCMC 
#' analyses, the mean distance will stay constant as the sampling interval 
#' increases. If there is autocorrelation, the mean distance will 
#' increase as the sampling interval increases, and is expected to level
#' off when the autocorrelation decreases to zero. The function calculates
#' path distances, though other distances could also be employed.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin.
#' @param autocorr.intervals The number of sampling intervals to use. These will be spaced evenly between 1 and the max.sampling.interval 
#' @param max.sampling.interval The largest sampling interval for which you want to calculate the mean distance between pairs of trees (default is 10 percent of the length of the chain).
#' @param squared TRUE/FALSE use squared tree distances (necessary to calculate approximate ESS; default FALSE)
#' @param facet TRUE/FALSE to turn facetting of the plot on or off (default FALSE)
#' @param free_y TRUE/FALSE to turn free y scales on the facetted plots on or off (default FALSE). Only works if facet = TRUE.
#' @param treedist the type of tree distance metric to use, can be 'PD' for path distance or 'RF' for Robinson Foulds distance
#' @param use.all.samples (TRUE/FALSE). Whether to calculate autocorrelation from all possible pairs of trees in your chain. The default is FALSE, in which case 250 samples are taken at each sampling interval. This is sufficient to get reasonably accurate estimates of the approximate ESS. Setting this to TRUE will give you slightly more accurate ESS estimates, at the cost of potentially much longer execution times.
#'
#' @return A ggplot2 plot object, with one line (facetting off) or facet
#' (facetting on) per rwty.trees object.
#'
#' @keywords autocorrelation, path distance
#'
#' @export makeplot.autocorr
#' @examples
#' data(fungus)
#' makeplot.autocorr(fungus, burnin = 20)

makeplot.autocorr <- function(chains, burnin = 0, max.sampling.interval = NA, autocorr.intervals = 40, squared = FALSE, facet = FALSE, free_y = FALSE, treedist = 'PD'){

    print(sprintf("Creating topological autocorrelation plot"))

    chains = check.chains(chains)

    chain = chains[[1]]

    N = length(chain$trees)

    if(is.na(max.sampling.interval)){
        max.sampling.interval = floor((N - burnin) - (0.9 * (N - burnin)))
    }

    dat <- topological.autocorr(chains, burnin, max.sampling.interval, autocorr.intervals, squared = squared, treedist = treedist)

    if(treedist=='RF'){
        td.name = "Robinson Foulds"
    }else if(treedist=="PD"){
        td.name = "Path Difference"
    }else{
        stop("Unknown option for treedist. Valid options are 'PD' (for path distance) or 'RF' (for Robinson Foulds distance). Please try again")
    }

    if(squared == TRUE){
        y.label = sprintf("Mean Squared %s between Pairs of Trees", td.name)
    }else{
        y.label = sprintf("Mean %s between Pairs of Trees", td.name)
    }

    autocorr.plot = ggplot(data=dat, aes(x=sampling.interval, y=topo.distance)) + 
            geom_line(aes(colour = chain)) + geom_point(aes(colour = chain)) + 
            xlab("Sampling Interval between Trees") + ylab(y.label) + 
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
