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

plot.treespace <- function(chains, n.points, burnin = 0, likelihood = NA){


    # Pre - compute checks. Since the calculations can take a while...
    comparisons = (n.points * length(chains)) * (n.points * length(chains))

    print(sprintf("Creating treespace plot"))
    print(sprintf("This will require the calculation of %s pairwise tree distances", comparisons))

    if(n.points > (length(chains[[1]]$trees) - burnin)) {
        stop("The number of trees (after removing burnin) is smaller than the number of points you have specified")
    }

    if(comparisons > 1000000){
        print(sprintf("WARNING: Calculating %s pairwise tree distances may take a long time, consider plotting fewer points.", comparisons))
    }

    # now go and get the x,y coordinates from the trees
    points = treespace(chains, n.points, burnin, likelihood)

    points.plot <- ggplot(data=points, aes(x=x, y=y)) + 
                    geom_path(alpha=0.25, aes(colour = generation), size=0.75) + 
                    scale_colour_gradient(low='red', high='yellow') +
                    theme(panel.background = element_blank(), axis.line = element_line(color='grey')) +
                    facet_wrap(~chain, nrow=round(sqrt(length(unique(points$chain)))))    


    if(!is.na(likelihood)){
        points.plot <- points.plot + 
                    geom_point(shape = 21, size=4, colour = 'white', aes(fill = lnL)) + 
                    scale_fill_gradient(low='black', high='light blue')
    } else {
        points.plot <- points.plot + geom_point(size=4) 
    }

    # only make a heatmap if we have > 1 unique point to look at
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