#' MDS scaling of treespace for a single tree list.
#' 
#' This function constructs a distance matrix from a list of trees
#' and uses multi-dimensional scaling to collapse it to a two-
#' dimensional tree space for plotting.
#'
#' @param chain A single rwty.trees object.
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

treespace.single <- function(chain, n.points, burnin=0){
    # do MDS on a single list of trees
    # p.file is a list of likelihoods...

    # subsample down to minimum n.points or thereabouts
    step = as.integer((length(chain$trees) - burnin)/n.points)
    indices = seq(from = burnin+1, to = length(chain$trees), by = step)   

    trees = chain$trees[indices]
    ptable = chain$ptable[indices,]
    gens = chain$gens.per.tree
    
    # for now this is hard-coded, who wants a 3D plot anyway, right?
    dimensions = 2
    
    d <- tree.dist.matrix(trees)
    
    mds <- isoMDS(d, k=dimensions)
    
    points <- as.data.frame(mds$points)
    names(points) <- c("x", "y")
    
    if(!is.null(ptable)){
        points <- cbind(points, lnL = ptable$LnL, Generation = ptable$Gen)
    }
    
    p <- plot.treespace(points)
    
    r <- list("points" = points, "plot" = p)
    
}

