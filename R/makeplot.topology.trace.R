#' Plotting parameters
#' 
#' Plots tree topologies over the length of the MCMC chain. The plot shows the path distance
#' of each tree in each chain from the first tree of the first chain (after discarding burnin).
#' If required, the behaviour can be changed to plot the path distance of each tree from the first
#' tree in each chain, using the independent.chains option.
#'
#' @param chains A set of rwty.trees objects.
#' @param burnin The number of trees to omit as burnin. 
#' @param facet TRUE/FALSE denoting whether to make a facet plot (default TRUE)
#' @param free_y TRUE/FALSE to turn free y scales on the facetted plots on or off (default FALSE). Only works if facet = TRUE.
#' @param independent.chains TRUE/FALSE if FALSE (the default) then the plots show the distance of each tree from the first tree of the first chain. If TRUE, the plots show the distance of each tree from the first tree of the chain in which that tree appears. The TRUE option should only be used in the case that different chains represent analyses of different genes or datasets.
#'
#' @return topology.plot Returns a ggplot object.
#'
#' @keywords parameter, plot, convergence, mcmc, phylogenetics
#'
#' @export makeplot.topology.trace
#' @examples
#' data(fungus)
#' makeplot.topology.trace(fungus, burnin=20, parameter="pi.A.")

makeplot.topology.trace <- function(chains, burnin = 0, facet=TRUE, free_y = FALSE, independent.chains = FALSE){ 

    chains = check.chains(chains)


    # get ESS values
    ess = topological.approx.ess(chains, burnin)
    operator = ess$operator
    ess = list(ess$approx.ess, stringsAsFactors=FALSE)[[1]]
    ess = round(ess, digits = 0)
    labels = paste(names(chains), " (Approximate ESS ", operator, " ", ess, ")", sep="")
    names(chains) = labels

    if(independent.chains == TRUE){
        distances = tree.distances.from.first(chains, burnin)
    }else{
        focal.tree = chains[[1]]$trees[burnin+1]
        distances = tree.distances.from.first(chains, burnin, focal.tree = focal.tree)        
    }
    topology.plot =  ggplot(data = distances, aes(x=generation, y=topological.distance)) + 
                    geom_line(aes(colour = chain)) + 
                    ggtitle("Tree topology") +
                    theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5))

    if(facet) topology.plot = topology.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")



    if(facet){ 
        if(free_y){

            topology.plot = topology.plot + facet_wrap(~chain, ncol=1, scales = "free_y") + theme(legend.position="none")

        }else{

            topology.plot = topology.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")

        }
    }



    return(topology.plot)


}