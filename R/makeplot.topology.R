#' Plotting parameters
#' 
#' Plots a trace of topological distances of trees over the length of the MCMC chain. The plot shows the topological distance
#' of each post-burnin tree (except the first) in each chain from the first post-burnin tree of that chain. These plots can be 
#' interpreted very similarly to parameter trace plots. The key difference is that the plot is of differences, not of absolute values.
#'
#' @param chains A set of rwty.chain objects.
#' @param burnin The number of trees to omit as burnin. 
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

makeplot.topology <- function(chains, burnin = 0, facet=TRUE, free_y = TRUE){ 

    print(sprintf("Creating trace for tree topologies"))

    chains = check.chains(chains)

    if(facet == FALSE && independent.chains == TRUE){
        stop("Cannot produce this plot. The independent.chains argument means that each topology trace is calculated from a different focal tree, so plotting them all on the same axes would be misleading")
    }

    # get ESS values
    ess = topological.approx.ess(chains, burnin)
    operator = ess$operator
    ess = list(ess$approx.ess, stringsAsFactors=FALSE)[[1]]
    ess = round(ess, digits = 0)
    labels = paste(names(chains), " (Approximate ESS ", operator, " ", ess, ")", sep="")
    names(chains) = labels

    distances = tree.distances.from.first(chains, burnin)

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

    density.plot =  ggplot(data = distances, aes(x=topological.distance)) + 
                        geom_histogram(aes(fill = fill)) + 
                        ggtitle("Tree topology trace") +
                        xlab(axis_label) +
                        scale_fill_manual(values =plasma(2, end = 0.65), guide = FALSE) +
                        theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5))

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


tree.distances.from.first <- function(chains, burnin = 0){
  # return tree distances from the first tree of each chain

  chains <- check.chains(chains)
  
  # the focal tree is the first tree after burnin
  focal.tree.index = burnin+1
  other.tree.indices = (focal.tree.index+1):length(chains[[1]]$trees)
  focal.tree.indices = rep(focal.tree.index, length(other.tree.indices))

  dist.matrices <- lapply(chains, function(x) x[['tree.dist.matrix']])
  
  processors = get.processors(NULL)
  
  distances = mclapply(dist.matrices, dist_get, focal.tree.indices, other.tree.indices, mc.cores = processors)
  
  names <- lapply(names(chains), rep, length(other.tree.indices))
  
  gens <- rep((other.tree.indices)*chains[[1]]$gens.per.tree, length(chains))
  
  dist.df <- data.frame(topological.distance = unlist(distances), chain = unlist(names), generation = gens)
  
  return(dist.df)
  
}
