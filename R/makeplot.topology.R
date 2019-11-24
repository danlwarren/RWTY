#' Plotting parameters
#' 
#' Plots a trace of topological distances of trees over the length of the MCMC chain. The plot shows the topological distance
#' of each post-burnin tree in each chain from the maximum clade credibility (MCC) tree for that chain. These plots can be 
#' interpreted very similarly to parameter trace plots. The key difference is that the plot is of differences, not of absolute values.
#' When the chain samples the MCC tree, the distance will be zero. So, one should expect that if the burnin is too short, you will
#' have a plot that starts at high values (i.e. the chain is sampling trees far from the MCC tree) and gradually approaches low values
#' (i.e. the chain ends up sampling trees fairly close to the MCC tree).
#'
#' @param chains A set of rwty.chain objects.
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
#' @param facet TRUE/FALSE denoting whether to make a facet plot (default TRUE)
#' @param free_y TRUE/FALSE to turn free y scales on the facetted plots on or off (default TRUE). Only works if facet = TRUE.
#'
#' @return topology.trace.plot Returns a ggplot object.
#'
#' @keywords parameter, plot, convergence, mcmc, phylogenetics
#'
#' @export makeplot.topology
#' @examples
#' \dontrun{
#' data(fungus)
#' makeplot.topology(fungus, burnin=20)
#' }

makeplot.topology <- function(chains, burnin = NA, facet=TRUE, free_y = TRUE){ 

    print(sprintf("Creating trace for tree topologies"))

    chains = check.chains(chains)
    
    # set burnin to the maximum from across all chains
    if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
    
    # get ESS values
    ess = topological.approx.ess(chains, burnin)
    operator = ess$operator
    ess = list(ess$approx.ess, stringsAsFactors=FALSE)[[1]]
    ess = round(ess, digits = 0)
    labels = paste(names(chains), " (Approximate ESS ", operator, " ", ess, ")", sep="")
    names(chains) = labels

    distances = tree.distances.from.mcc(chains, burnin)

    # some flag for a corner case where we need to manually set axis labels
    if(all(distances$topological.distance == 0)) { axis.flag = 1 }else{ axis.flag = 0 }
    
    # Calculate CIs either by chain or overall
    in.ci <- function(x){
      as.numeric(x > quantile(x, c(0.025)) &  x < quantile(x, c(0.975)))
    }
    
    if(facet){
      fill <- unlist(lapply(unique(distances$chain), function(x) in.ci(distances[distances$chain == x,"topological.distance"])))
    } else {
      fill <- in.ci(distances[,"topological.distance"])
    }
    fill[which(fill == 0)] = 'red'  
    fill[which(fill == 1)] = 'blue'

    axis_label = sprintf("%s distance of tree from first post-burnin tree", chains[[1]]$tree.dist.metric)
    
    trace.plot =  ggplot(data = distances, aes(x=generation, y=topological.distance)) + 
                        geom_line(aes(colour = chain)) + 
                        ggtitle("Tree topology trace") +
                        xlab("Generation") +
                        ylab(axis_label) +
                        scale_color_viridis(discrete = TRUE, begin = 0.2, end = .8, option = "C") +
                        scale_fill_viridis(discrete = TRUE, begin = 0.2, end = .8, option = "C") +
                        theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5))

    if(axis.flag == 0){
        density.plot =  ggplot(data = distances, aes(x=topological.distance)) + 
                        geom_histogram(aes(fill = fill)) + 
                        ggtitle("Tree topology density plot") +
                        xlab(axis_label) +
                        scale_fill_manual(values =plasma(2, end = 0.65), guide = FALSE) +
                        theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5))
    }else{
        density.plot =  ggplot(data = distances, aes(x=topological.distance)) + 
                        geom_histogram(aes(fill = fill)) + 
                        ggtitle("Tree topology density plot") +
                        xlab(axis_label) +
                        scale_fill_manual(values =plasma(2, end = 0.65), guide = FALSE) +
                        theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5)) + 
                        xlim(c(-0.1, 1))
        
    }
    if(facet){ 
        if(free_y){
            trace.plot = trace.plot + facet_wrap(~chain, ncol=1, scales = "free_y") + theme(legend.position="none")
            density.plot = density.plot + facet_wrap(~chain, ncol=1, scales = "free_y") + theme(legend.position="none")
        }else{
            trace.plot = trace.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")
            density.plot = density.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")
        }
    }

    return(list(trace.plot = trace.plot, density.plot = density.plot))

}


tree.distances.from.mcc <- function(chains, burnin){
  # return tree distances from the mcc tree of each chain

  chains <- check.chains(chains)
  N = length(chains[[1]]$trees)
  dist.vectors <- lapply(chains, function(x) x[['ptable']]$topo.dist.mcc[burnin:N])
  
  names <- lapply(names(chains), rep, length(dist.vectors[[1]]))
  
  gens <- rep((burnin:N)*chains[[1]]$gens.per.tree, length(chains))
  
  dist.df <- data.frame(topological.distance = unlist(dist.vectors), chain = unlist(names), generation = gens)
  
  return(dist.df)
  
}
