#' Plotting of tree ESS data
#'
#' @param tree.ess.object 
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords phylogeny, topology, effective sample size
#'
#' @export
#' 
#' @examples
#' plot.tree.ess(essresults)

plot.tree.ess <- function(tree.ess.object){
    
    dat <- tree.ess.object$tree.distances
    
    t <- ggplot(data=dat, aes(x=gapsize, y=average.distance)) + 
        geom_hline(yintercept=tree.ess.object$reference.distance, linetype='dashed') +
        geom_hline(yintercept=tree.ess.object$reference.distance.lowerCI, linetype='dashed', colour="gray") +
        geom_point(size = 2, aes(color=lower)) + 
        theme(legend.position="none")
    
    
    t
    
}
