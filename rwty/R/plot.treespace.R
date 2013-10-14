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

plot.treespace <- function(points){
    # This fundtion will take an mds.treespace object and produce plots 
    # of chains in treespace
    
    if(!is.null(points$LnL)){
        p <- ggplot(data=points, aes(x=x,y=y,fill=LnL)) + 
            geom_path(alpha=0.2, linetype='dashed') + 
            geom_point(shape=21, size=4, alpha=0.85, colour='white') + 
            scale_fill_gradient() +
            geom_point(data=points, aes(x=x, y=y, colour=mcmc.sample), size=2)  + 
            scale_colour_gradient(low='red', high='white')
    }
    else{
        p <- ggplot(data=points, aes(x=x,y=y)) + 
            geom_path(alpha=0.2, linetype='dashed') + 
            geom_point(data=points, aes(x=x, y=y, colour=mcmc.sample), size=2)  + 
            scale_colour_gradient(low='red', high='white')
    }
    p
}