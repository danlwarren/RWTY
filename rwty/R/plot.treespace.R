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
        points.plot <- ggplot(data=points, aes(x=x,y=y,fill=lnL)) + 
            geom_path(alpha=0.25, aes(colour=Generation), size=0.75) + 
            geom_point(shape=21, size=4, colour='white') + 
            scale_fill_gradient(low='black', high='light blue') + 
            scale_colour_gradient(low='red', high='yellow') + 
            theme(panel.background = element_blank(), axis.line = element_line(color='grey')) +
            facet_wrap(~chain, nrow=round(sqrt(length(unique(points$chain)))))    
        }
    else{
        points.plot <- ggplot(data=points, aes(x=x,y=y)) + 
            geom_path(alpha=0.25, aes(colour=TreeNum), size=0.75) + 
            geom_point(size=4) + 
            scale_colour_gradient(low='red', high='yellow') +
            theme(panel.background = element_blank(), axis.line = element_line(color='grey')) +
            facet_wrap(~chain)
    }

    # only make a heatmap if we have > 1 point to look at
    if(length(unique(c(points$x, points$y))) == 1){
        heatmap = NA
    }else{
        heatmap <- ggplot(data=points, aes(x=x,y=y)) + 
            stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
            theme(panel.background = element_blank(), axis.line = element_line(color='grey')) +
            facet_wrap(~chain, nrow=round(sqrt(length(unique(points$chain))))) + 
            scale_fill_gradient(low='white', high='black')
    }

    return(list('plot' = points.plot, 'heatmap' = heatmap))
}