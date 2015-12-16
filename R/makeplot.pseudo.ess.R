#' Plot the pseudo ESS of tree topologies from MCMC chains.
#'
#' This function takes a list of rwty.trees objects, and plots the
#' pseudo ESS of the tree topologies from each chain, after removing burnin. 
#' Each caulcation is repeated n times, where in each replicate a random
#' tree from the chain is chosen as a 'focal' tree. The calculation works
#' by calculating the path distance of each tree in the chain
#' from the focal tree, and calculating the ESS of the resulting vector
#' of phylogenetic distances using the effectiveSize function from the 
#' coda package. NB this function requires the calculation of many
#' tree distances, so can take some time.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param n The number of replicate analyses to do
#'
#' @return pseudo.ess.plot A ggplot2 plot object, in which each chain is represented by a point
#' which represents the median pseudo ESS from the n replicates, and
#' whiskers representing the upper and lower 95% intervals of the n replicates. 
#'
#' @keywords ESS, path distance
#'
#' @export makeplot.pseudo.ess
#' @examples
#' data(fungus)
#' makeplot.pseudo.ess(fungus, burnin = 20, n = 10)

makeplot.pseudo.ess <- function(chains, burnin = 0, n = 20){

    print(sprintf("Creating pseudo ESS plot"))

    chains <- check.chains(chains)

    dat <- topological.pseudo.ess(chains, burnin, n)
    
    dat <- data.frame(median.ess = apply(dat, 2, FUN = median), 
                      ci.lower = apply(dat, 2, FUN = function(x) quantile(x, .025)), 
                      ci.upper = apply(dat, 2, FUN = function(x) quantile(x, .975)),
                      chain = names(chains))
    
    pseudo.ess.plot <- ggplot(dat, aes(x=chain, y=median.ess, colour = chain)) + 
      geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper)) +
      geom_point() +
      xlab("Chain") +
      ylab("Pseudo ESS") +      
      expand_limits(y=0) +
      ggtitle(sprintf("Pseudo ESS with %d replicates", n))
    
    return(list("pseudo.ess.plot" = pseudo.ess.plot))
    
}
