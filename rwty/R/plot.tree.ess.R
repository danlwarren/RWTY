#' A one sentence description of what your function does
#' 
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces. 
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

plot.tree.ess <- function(tree.ess.object){
    
    dat <- tree.ess.object$tree.distances
    
    t <- ggplot(data=dat, aes(x=gapsize, y=average.distance)) + 
        geom_hline(yintercept=tree.ess.object$reference.distance, linetype='dashed') +
        geom_hline(yintercept=tree.ess.object$reference.distance.lowerCI, linetype='dashed', colour="gray") +
        geom_point(size = 2, aes(color=lower)) + 
        theme(legend.position="none")
    
    
    t
    
}
