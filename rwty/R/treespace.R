#' MDS scaling of treespace for a single tree list.
#' 
#' This function constructs a distance matrix from a list of trees
#' and uses multi-dimensional scaling to collapse it to a two-
#' dimensional tree space for plotting.
#'
#' @param chains A list of 1 or more rwty.trees objects.
#' @param burnin The number of trees to eliminate as burnin. Default is zero. \code{burnin}
#' @param n.points The minimum number of points you want in your plot.
#'
#' @return Returns a list containing the points and a plot.
#'
#' @keywords treespace, mds, multi-dimensional scaling
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' burnin <- 100

treespace <- function(chains, n.points, burnin=0, labels=NA){
    # do MDS on a >1 lists of trees

    # TO DO: some checks, should be outsourced to a different private function, since they
    # will be useful for any user-facing function that uses multiple chains
    # 1. Check that chains is a list of rwty.trees functions
    # 2. Check that all chains in there are the same length, with same generations labels.

    # make sure all chains are labelled
    if(any(is.na(labels))){
        labels <- c(paste("Chain", seq(1:length(chains)), sep="."))
    }

    chain = chains[[1]]

    # subsample down to minimum n.points or thereabouts
    step = as.integer((length(chain$trees) - burnin)/n.points)
    indices = seq(from = burnin+1, to = length(chain$trees), by = step)   

    # let's organise the data into lists of lists
    trees = lapply(chains, function(x) x[['trees']][indices])
    ptable = lapply(chains, function(x) x[['ptable']][indices,])

    
    # for now this is hard-coded, who wants a 3D plot anyway, right?
    dimensions = 2

    # combine all the trees and get a monster distance matrix
    alltrees = trees[[1]]
    if(length(trees)>1){
        for(i in 2:length(trees)){
            alltrees = c(alltrees, trees[[i]])
        }
    }

    d <- tree.dist.matrix(alltrees)

    mds <- cmdscale(d, k=dimensions)
    
    points <- as.data.frame(mds)
    row.names(points) <- seq(nrow(points))
    names(points) <- c("x", "y")
    points$chain = unlist(lapply(labels, function(x) rep(x, length(indices))))


    if(!is.null(ptable)){
        Generation = unlist(lapply(ptable, function(x) x[['Gen']]))
        lnL = unlist(lapply(ptable, function(x) x[['LnL']]))
        points <- cbind(points, lnL, Generation)
    }
    
    p <- plot.treespace.multi(points)
    
    r <- list("points" = points, "plot" = p)
    
}

