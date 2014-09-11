#' MDS scaling of treespace for a single tree list.
#' 
#' This function constructs a distance matrix from a list of trees
#' and uses multi-dimensional scaling to collapse it to a two-
#' dimensional tree space for plotting.
#'
#' @param trees A multiphylo object \code{trees}
#' @param gens A list of the generation represented by each tree, for use in plotting \code{}
#' @param ptable A table of p values to use for plotting likelihoods \code{p.file}
#'
#' @return Returns a list containing the mds table and a plot.
#'
#' @keywords treespace, mds, multi-dimensional scaling
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' burnin <- 100
#' #We have to start by trimming the chain and p table down to a reasonable number of trees
#' mdstrees <- run1$trees[seq((burnin + 1), length(run1$trees), by = 20)]
#' #Now we're going to get a list of generations matching the trees retained from the chain
#' gens <- as.numeric(unlist(regmatches(names(run1$trees), gregexpr('\\(?[0-9]+', names(run1$trees)))))
#' gens <- gens[seq((burnin + 1), length(run1$trees), by = 20)]
#' #Finally we're going to cut down the p table to just the bits we're going to use
#' mdsptable <- run1$ptable[seq((burnin + 1), length(run1$trees), by = 20),]
#' this.treespace <- treespace.single(mdstrees, gens=gens, ptable=mdsptable)

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

