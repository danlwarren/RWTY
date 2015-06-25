#' Plot chains in treespace.
#' 
#' This function will take an mds.treespace object and produce plots of chains in treespace.
#'
#' @param chains A list of one or more rwty.trees objects
#' @param n.points The number of points on each plot
#' @param burnin The number of samples to remove from the start of the chain as burnin
#' @param parameter The name of any column in your parameter file that you would like to plot on the treespace plot
#'
#' @return A list of two ggplot objects: one plots the points in treespace, the other shows a heatmap of the same points
#'
#' @keywords plot, treespace, rwty
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' 
#' # Treespace plot for all the fungus data
#' p <- makeplot.treespace(fungus, burnin = 100, parameter = 'LnL')
#' 
#' # NB: these data indicate significant problems: the chains are sampling very different parts of tree space
#' # View the points plotted in treespace (these data indicate significant problems)
#' p$points.plot
#' 
#' # View the heatmap of the same data
#' # Note that this data is so pathologically bad that the heatmap is not
#' # very useful. It is more useful on better behaved datasets
#' p$heatmap
#' 
#' # we can also plot different parameters as the fill colour.
#' # e.g. we can plot the first two fungus chains with likelihood as the fill
#' makeplot.treespace(fungus[1:2], burnin = 100, parameter = 'LnL')
#' 
#' # or with tree length as the fill
#' makeplot.treespace(fungus[1:2], burnin = 100, parameter = 'TL')
#'
#' # you can colour the plot with any parameter in your ptable
#' # to see which parameters you have you can simply do this:
#' head(fungus[[1]]$ptable)


makeplot.treespace <- function(chains, n.points = 100, burnin = 0, parameter = NA){


    # Pre - compute checks. Since the calculations can take a while...
    comparisons = ((n.points * length(chains)) * (n.points * length(chains)) / 2.0) - (n.points * length(chains))

    print(sprintf("Creating treespace plot"))
    print(sprintf("This will require the calculation of %s pairwise tree distances", comparisons))

    if(n.points < 2) {
      stop("You need at least two points to make a meaningful treespace plot")
    }
    
    if(n.points > (length(chains[[1]]$trees) - burnin)) {
        stop("The number of trees (after removing burnin) is smaller than the number of points you have specified")
    }

    if(comparisons > 100000){
        print(sprintf("WARNING: Calculating %s pairwise tree distances may take a long time, consider plotting fewer points.", comparisons))
    }

    # now go and get the x,y coordinates from the trees
    points = treespace(chains, n.points, burnin, parameter)

    points.plot <- ggplot(data=points, aes(x=x, y=y)) + 
                    geom_path(alpha=0.25, aes(colour = generation), size=0.75) + 
                    scale_colour_gradient(low='red', high='yellow') +
                    theme(panel.background = element_blank(), axis.line = element_line(color='grey')) +
                    theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5)) +
                    facet_wrap(~chain, nrow=round(sqrt(length(unique(points$chain)))))    


    if(!is.na(parameter)){
        points.plot <- points.plot + 
                    geom_point(shape = 21, size=4, colour = 'white', aes_string(fill = parameter)) + 
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

    return(list('heatmap' = heatmap, 'points.plot' = points.plot))
}