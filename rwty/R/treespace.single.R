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
    
    d <- tree.dist.matrix(trees[1:length(trees)])
    
    mds <- isoMDS(d, k=dimensions)
    
    points <- as.data.frame(mds$points)
    names(points) <- c("x", "y")
    
    if(!is.null(ptable)){
        points <- cbind(points, lnL = ptable$LnL, Generation = ptable$Gen)
    }
    
    p <- plot.treespace(points)
    
    r <- list("mds" = mds, "plot" = p)
    
}

