#' Plot chains in treespace.
#' 
#' This function will take list of rwty.chains objects and produce plots of chains in treespace.
#'
#' @param chains A list of one or more rwty.chain objects
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
#' @param min.points The minimum number of points on each plot. The function will automatically choose the thinning value which gets you the smallest number of trees that is at least as much as this value. The default (200) is usually sufficient to get a good idea of what is happening in your chains. 
#' @param fill.color The name of the column from the log table that that you would like to use to colour the points in the plot. The default is to colour the points by LnL.
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
#' p <- makeplot.treespace(fungus)
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
#' makeplot.treespace(fungus[1:2], fill.color = 'LnL')
#' 
#' # or with tree length as the fill
#' makeplot.treespace(fungus[1:2], fill.color = 'TL')
#'
#' # you can colour the plot with any parameter in your ptable
#' # to see which parameters you have you can simply do this:
#' names(fungus[[1]]$ptable)
#' }


makeplot.treespace <- function(chains, burnin = NA, min.points = 200,  fill.color = "LnL"){

    chains = check.chains(chains)
  
    # set burnin to the maximum from across all chains
    if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
  
    print(sprintf("Creating treespace plots"))

    if(min.points < 20) {
      stop("You need at least twenty points to make a meaningful treespace plot")
    }
    
    # now go and get the x,y coordinates from the trees
    ts = treespace(chains, min.points, burnin, fill.color)
    
    points = ts$points
    mcc.xy = ts$mcc.xy
    
    points.plot <- ggplot(data=points, aes_string(x="x", y="y")) + 
      geom_path(alpha=0.25, aes_string(colour = "generation"), size=0.75) + 
      scale_colour_gradient(low='red', high='yellow') +
      theme_minimal() +
      facet_wrap(~chain, nrow=round(sqrt(length(unique(points$chain))))) +
      labs(title = sprintf("Tree space for %d trees per chain", length(unique(points$generation))),
           subtitle = "MCC tree shown as bullseye")


    if(!is.na(fill.color)){
      points.plot <- points.plot + 
        geom_point(shape = 21, size=4, colour = 'white', aes_string(fill = fill.color)) + 
        scale_fill_gradientn(colours = viridis(256))
    } else {
      points.plot <- points.plot + geom_point(size=4) 
    }

    # add a bullseye for the mcc tree
    points.plot = points.plot + 
      geom_point(data=mcc.xy, color = "red", size = 4) + 
      geom_point(data=mcc.xy, color = "white", size = 3.125) + 
      geom_point(data=mcc.xy, color = "red", size = 2.25) + 
      geom_point(data=mcc.xy, color = "white", size = 1.375) + 
      geom_point(data=mcc.xy, color = "red", size = 0.5)
    
    # only make a heatmap if we have > 1 unique point to look at
    if(length(unique(c(points$x, points$y))) == 1){
        heatmap = NA
    }else{
        heatmap <- ggplot(data=points, aes_string(x="x",y="y")) + 
          stat_density2d(geom="tile", aes_string(fill = "..density.."), contour = FALSE) + 
          theme(panel.background = element_blank(), axis.line = element_line(color='grey'), panel.spacing = unit(0.1, "lines")) +
          facet_wrap(~chain, nrow=round(sqrt(length(unique(points$chain))))) + 
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_fill_gradientn(colours = viridis(256)) +
          ggtitle(sprintf("Tree space heatmap for %d trees", nrow(points)))
    }

    return(list('treespace.heatmap' = heatmap, 'treespace.points.plot' = points.plot))
}