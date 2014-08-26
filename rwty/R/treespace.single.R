#' MDS scaling of treespace for a single tree list.
#' 
#' This function constructs a distance matrix from a list of trees
#' and uses multi-dimensional scaling to collapse it to a two-
#' dimensional tree space for plotting.
#'
#' @param trees A multiphylo object \code{trees}
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#' @param p.file a file of probabilities to use for coloring generations \code{p.file}
#'
#' @return Returns a list containing the mds table and a plot.
#'
#' @keywords treespace, mds, multi-dimensional scaling
#'
#' @export
#' 
#' @examples
#' treespace.single(mytrees, burnin=100, p.file="mytrees.p")

treespace.single <- function(trees, gens, ptable=NULL){
    # do MDS on a single list of trees
    # p.file is a list of likelihoods...
    
    # for now this is hard-coded, who wants a 3D plot anyway, right?
    dimensions=2
    
    d <- tree.dist.matrix(trees[1:length(trees)])
    
    mds <- isoMDS(d, k=dimensions)
    
    points <- as.data.frame(mds$points)
    names(points) <- c("x", "y")
    points$Generation <- gens
    mds$points <- points
    
    if(!is.null(ptable)){
        mds$points <- cbind(mds$points, ptable$LnL)
        colnames(mds$points)[length(mds$points)] <- "LnL"
    }
    
    p <- plot.treespace(mds$points)
    
    r <- list("mds" = mds, "plot" = p)
    
}

