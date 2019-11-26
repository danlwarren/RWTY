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
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
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

makeplot.autocorr <- function(chains, burnin = NA, facet = FALSE, free_y = FALSE){

    print(sprintf("Creating topological autocorrelation plot"))

    chains = check.chains(chains)

    chain = chains[[1]]

    if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
    
    N = length(chain$trees)
    
    dat <- topological.autocorr(chains, burnin)

    ylabel = sprintf("Mean %s distance between pairs of trees", chain$tree.dist.metric)
    
    autocorr.plot = ggplot(data=dat, aes_string(x="sampling.interval", y="topo.distance")) + 
            geom_line(aes_string(colour = "chain")) + geom_point(aes_string(colour = "chain")) + 
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
