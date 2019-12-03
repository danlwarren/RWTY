#' MDS scaling of treespace for a single tree list.
#' 
#' This function constructs a distance matrix from a list of trees
#' and uses multi-dimensional scaling to collapse it to a two-
#' dimensional tree space for plotting.
#'
#' @param chains A list of 1 or more rwty.chain objects.
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
#' @param n.points The minimum number of points you want in your plot (default is 100).
#' @param fill.color The name of the column from the log table that that you would like to use to colour the points in the plot. The default is to colour the points by LnL.
#'
#' @return Returns a list containing the points and a plot.
#'
#' @keywords treespace, mds, multi-dimensional scaling
#'
#' @export treespace
#' @examples
#' \dontrun{
#' data(fungus)
#' treespace(fungus, n.points=50, burnin=20, fill.color="LnL")
#' }



treespace <- function(chains, n.points = 100, burnin=NA, fill.color="LnL"){

    chains = check.chains(chains)
    
    chain = chains[[1]]
    
    # set burnin to the maximum from across all chains
    if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
    
    labels = names(chains)
    ptable = combine.ptables(chains, burnin=0) # we deal with burnin later for this 

    # check that the user-supplied parameter variable exists
    if(!is.na(fill.color)){
        if(fill.color %in% names(ptable)){} else
        stop(sprintf("The fill.color name you supplied ('%s') wasn't found in your parameter table", fill.color))
    }

    # subsample down to minimum n.points
    step = as.integer((length(chain$trees) - burnin) / n.points)
    indices = seq(from = burnin + 1, to = length(chain$trees), by = step)   

    # subsample trees and parameters by indices
    trees = lapply(chains, function(x) x[['trees']][indices])

    additional = unlist(lapply(length(chain$trees) * (0:(length(chains) - 1)), function(x) rep(x, length(indices))))
    ptable = ptable[(indices + additional),]
    
    # for now this is hard-coded
    dimensions = 2

    # combine all the trees and get a distance matrix
    alltrees = trees[[1]]
    if(length(trees)>1){
        for(i in 2:length(trees)){
            alltrees = c(alltrees, trees[[i]])
        }
    }

    d <- tree.dist.matrix(alltrees)

    # here's a catch in case all trees are identical, in which case all of d==0
    if(sum(d)==0){
        # all trees are the same, so all points should just sit on 0,0
        x <- rep(0, length(alltrees))
        y <- rep(0, length(alltrees))
        mds <- data.frame(x=x, y=y)
    }else{
        mds <- cmdscale(d, k=dimensions)
    }

    points <- as.data.frame(mds)
    row.names(points) <- seq(nrow(points))
    names(points) <- c("x", "y")

    points$chain = ptable$chain
    points$sample = ptable$sample
    points$generation = ptable$generation

    if(!is.na(fill.color)) points[,fill.color] = as.numeric(ptable[[fill.color]])

    return(points)
        
}

