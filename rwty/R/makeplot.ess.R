#' Plot the approximate ESS of tree topologies from MCMC chains.
#'
#' This function takes a list of rwty.trees objects, and plots the
#' approximate ESS of the trees from each chain, after removing burnin. 
#' Each caulcation is repeated n times, where in each replicate a random
#' tree from the chain is chosen as a 'focal' tree. The calculation works
#' by calculating the path distance of each tree in the chain
#' from the focal tree, and calculating the ESS of the resulting vector
#' of phylogenetic distances using the effectiveSize function from the 
#' coda package. NB this function requires the calculation of many many
#' tree distances, so can take some time.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param n The number of replicate analyses to do 
#'
#' @return A ggplot2 plot object, in which each chain is represented by a point
#' which represents the median approximate ESS from the n replicates, and
#' whiskers representing the upper and lower 95% intervals of the n replicates. 
#'
#' @keywords ESS, path distance
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' makeplot.ess(fungus, burnin = 250, n = 10)
#' # demostrates severe issues with run1!

makeplot.ess <- function(chains, burnin = 0, n = 50){

    chains <- check.chains(chains)

    dat <- topological.ess(chains, burnin, n)
    
    # Attaching color information to the table of ESS so we can plot the geom_segments in color
    dat <- cbind(dat, rep(rainbow(length(chains)), each = n))
    
    colnames(dat) <- c("ESS", "chain", "color")
    
    ess.plot <- ggplot(data=dat)+
      geom_violin(aes(x=chain,y=ESS,fill=chain, s=0.5, v=0.9), trim=F, scale="width") +
      geom_segment(aes(
        x=match(chain,levels(chain))-0.05,
        xend=match(chain,levels(chain))+0.05,
        y=ESS,yend=ESS),
        colour=dat$color
      ) +
      xlab("Chain") +
      ylab("Approximate ESS") +
      theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5)) +
      expand_limits(y=0)

    #ess.plot <- ggplot(dat, aes(x = chain,  
    #  ymin = ci.95.lower, lower = ci.50.lower, middle = median.ess, upper = ci.50.upper, ymax = ci.95.upper)) + 
    #  geom_boxplot(stat="identity", colour=rainbow(length(dat$chain), s=1, v=0.5), 
    #               fill=rainbow(length(dat$chain), s=0.5, v=0.9)) +
    #  xlab("Chain") +
    #  ylab("Approximate ESS") +
    #  theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5)) +
    #  expand_limits(y=0)
    
    # for some reason these plots need to be returned as lists, or analyze.simple flattens them and puts
    # the plot components, rather than the plot itself, into the final object
    ess.plot <- list(ess.plot = ess.plot)

    return(ess.plot)
    
}
