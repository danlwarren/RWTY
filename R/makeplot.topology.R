#' Plotting parameters
#' 
#' Plots tree topologies over the length of the MCMC chain. The plot shows the path distance
#' of each tree in the chain from the first tree (after discarding burnin) 
#'
#' @param chains A set of rwty.trees objects.
#' @param burnin The number of trees to omit as burnin. 
#' @param facet Boolean denoting whether to make a facet plot.
#' @param free_y TRUE/FALSE to turn free y scales on the facetted plots on or off (default TRUE). Only works if facet = TRUE.
#'
#' @return topology.plot Returns a ggplot object.
#'
#' @keywords parameter, plot, convergence, mcmc, phylogenetics
#'
#' @export makeplot.topology.trace
#' @examples
#' data(fungus)
#' makeplot.topology.trace(fungus, burnin=20, parameter="pi.A.")

makeplot.topology.trace <- function(chains, burnin = 0, facet=TRUE, free_y = TRUE){ 

    chains = check.chains(chains)


    # get ESS values
    ess = topological.approx.ess(chains, burnin)
    operator = ess$operator
    ess = list(ess$approx.ess, stringsAsFactors=FALSE)[[1]]
    ess = round(ess, digits = 0)
    labels = paste(names(chains), " (Approximate ESS ", operator, " ", ess, ")", sep="")
    names(chains) = labels
    distances = tree.distances.from.first(chains, burnin)

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