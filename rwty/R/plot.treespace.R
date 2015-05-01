#' Plot chains in treespace.
#' 
#' This function will take an mds.treespace object and produce plots of chains in treespace.
#'
#' @param points A set of points to plot \code{points}
#'
#' @return p A ggplot object
#'
#' @keywords plot, treespace, rwty
#'
#' @export
#' 

plot.treespace <- function(points){

    if(!is.null(points$lnL)){
        p <- ggplot(data=points, aes(x=x,y=y,fill=lnL)) + 
            geom_path(alpha=0.3, aes(colour=Generation)) + 
            geom_point(shape=21, size=4, alpha=0.75, colour='white') + 
            scale_fill_gradient() + 
            scale_colour_gradient(low='red', high='yellow') + 
            theme(panel.background = element_blank(), axis.line = element_line(color='grey'))
    }
    else{
        p <- ggplot(data=points, aes(x=x,y=y)) + 
            geom_path(alpha=0.3, aes(colour=Generation)) + 
            geom_point(size=4, alpha=0.75) + 
            scale_colour_gradient(low='red', high='yellow') +
            theme(panel.background = element_blank(), axis.line = element_line(color='grey'))
    }
    p
}