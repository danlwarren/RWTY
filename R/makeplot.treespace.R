#' Plot chains in treespace.
#' 
#' This function will take list of rwty.chains objects and produce plots of chains in treespace.
#'
#' @param chains A list of one or more rwty.chain objects
#' @param burnin The number of samples to remove from the start of the chain as burnin
#' @param n.points The number of points on each plot
#' @param fill.color The name of any column in your parameter file that you would like to use as a fill colour for the points of the plot.
#'
#' @return A list of two ggplot objects: one plots the points in treespace, the other shows a heatmap of the same points
#'
#' @keywords plot, treespace, rwty
#'
#' @export makeplot.treespace
#' @examples
#' \dontrun{
#' data(fungus)
#' 
#' p <- makeplot.treespace(fungus, burnin = 20, fill.color = 'LnL')
#' # Treespace plot for all the fungus data
#' 
#' # NB: these data indicate significant problems: the chains are sampling very 
#' # different parts of tree space.
#' #
#' # View the points plotted in treespace (these data indicate significant problems)
#' p$treespace.points.plot
#' 
#' # View the heatmap of the same data
#' # Note that this data is so pathologically bad that the heatmap is not
#' # very useful. It is more useful on better behaved datasets
#' p$treespace.heatmap
#' 
#' # we can also plot different parameters as the fill colour.
#' # e.g. we can plot the first two fungus chains with likelihood as the fill
#' makeplot.treespace(fungus[1:2], burnin = 100, fill.color = 'LnL')
#' 
#' # or with tree length as the fill
#' makeplot.treespace(fungus[1:2], burnin = 100, fill.color = 'TL')
#'
#' # you can colour the plot with any parameter in your ptable
#' # to see which parameters you have you can simply do this:
#' names(fungus[[1]]$ptable)
#' }


makeplot.treespace <- function(chains, burnin = 0, n.points = 100,  fill.color = NA){


    print(sprintf("Creating treespace plots"))

    # Pre - compute checks. Since the calculations can take a while...
    comparisons = ((n.points * length(chains)) * (n.points * length(chains)) / 2.0) - (n.points * length(chains))

    if(n.points < 2) {
      stop("You need at least two points to make a meaningful treespace plot")
    }
    
    if("rwty.chain" %in% class(chains)){
      if(n.points > (length(chains$trees) - burnin)) {
        stop("The number of trees (after removing burnin) is smaller than the number of points you have specified")
      }
    } else {
      if(n.points > (length(chains[[1]]$trees) - burnin)) {
        stop("The number of trees (after removing burnin) is smaller than the number of points you have specified")
      }
    }

    if(comparisons > 1000000){
        print(sprintf("WARNING: Calculating %s pairwise tree distances for the treespace plot may take a long time, consider plotting fewer points.", comparisons))
    }

    # now go and get the x,y coordinates from the trees
    points = treespace(chains, n.points, burnin, fill.color)

    points.plot <- ggplot(data=points, aes_string(x="x", y="y")) + 
      geom_path(alpha=0.25, aes_string(colour = "generation"), size=0.75) + 
      scale_colour_gradient(low='red', high='yellow') +
      theme(panel.background = element_blank(), axis.line = element_line(color='grey'), panel.margin = unit(0.1, "lines")) +
      theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5)) +
      facet_wrap(~chain, nrow=round(sqrt(length(unique(points$chain))))) +
      ggtitle(sprintf("Tree space for %d trees", n.points))


    if(!is.na(fill.color)){
      points.plot <- points.plot + 
        geom_point(shape = 21, size=4, colour = 'white', aes_string(fill = fill.color)) + 
        scale_fill_gradientn(colours = viridis(256))
    } else {
      points.plot <- points.plot + geom_point(size=4) 
    }

    # only make a heatmap if we have > 1 unique point to look at
    if(length(unique(c(points$x, points$y))) == 1){
        heatmap = NA
    }else{
        heatmap <- ggplot(data=points, aes_string(x="x",y="y")) + 
          stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + 
          theme(panel.background = element_blank(), axis.line = element_line(color='grey'), panel.margin = unit(0.1, "lines")) +
          facet_wrap(~chain, nrow=round(sqrt(length(unique(points$chain))))) + 
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_fill_gradientn(colours = viridis(256)) +
          ggtitle(sprintf("Tree space heatmap for %d trees", n.points))
    }

    return(list('treespace.heatmap' = heatmap, 'treespace.points.plot' = points.plot))
}