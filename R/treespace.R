#' MDS scaling of treespace for a single tree list.
#' 
#' This function constructs a distance matrix from a list of trees
#' and uses multi-dimensional scaling to collapse it to a two-
#' dimensional tree space for plotting.
#'
#' @param chains A list of 1 or more rwty.chain objects.
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
#' @param min.points The minimum number of points on each plot. The function will automatically choose the thinning value which gets you the smallest number of trees that is at least as much as this value. The default (200) is usually sufficient to get a good idea of what is happening in your chains. 
#' @param fill.color The name of the column from the log table that that you would like to use to colour the points in the plot. The default is to colour the points by LnL.
#'
#' @return Returns a list of two objects: the `points` dataframe, and the `mcc.xy` dataframe. The former contains x and y coordinates for each tree calculated with mds, as well as the details pertaining to that tree. The latter contains just the x and y coordinates of the MCC tree for the chain(s) in question.
#'
#' @keywords treespace, mds, multi-dimensional scaling
#'
#' @export treespace
#' @examples
#' \dontrun{
#' data(fungus)
#' treespace(fungus)
#' }



treespace <- function(chains, min.points = 200, burnin=NA, fill.color="LnL"){

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

    total.trees = length(chain$trees) - burnin
    if(total.trees < 3){
      stop(sprintf("Cannot calculate treespace coordinates with fewer than 3 trees. Your chains contain %d trees, and your burnin is %d, leaving %d tree(s) to work with, and that's not enough.", length(chain$trees), burnin, total.trees))
    }
    
    # subsample down to minimum min.points
    step = as.integer((length(chain$trees) - burnin) / min.points)
    if(step<1){ step = 1 } # because sometimes the above gives step=0
    
    indices = seq(from = burnin + 1, to = length(chain$trees), by = step)   

    # subsample trees and parameters by indices
    trees = lapply(chains, function(x) x[['trees']][indices])

    # get all the rows from ptable that we want. The 'additional' here accounts for the separate chains
    # by figuring out where in the table each chain starts
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

    # add on the mcc tree
    alltrees = c(alltrees, chains[[1]]$mcc.tree)
    
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
    names(points) <- c("x", "y")
    
    # here we take the mcc tree off the list
    mcc.xy <- points[nrow(points),]
    rownames(mcc.xy) = "mcc.tree"
    points <- points[-nrow(points),]
    
    row.names(points) <- seq(nrow(points))

    points$chain = ptable$chain
    points$sample = ptable$sample
    points$generation = ptable$generation
    points$topo.dist.mcc = ptable$topo.dist.mcc

    if(!is.na(fill.color)) points[,fill.color] = as.numeric(ptable[[fill.color]])

    return(list(points = points, mcc.xy = mcc.xy))
        
}

