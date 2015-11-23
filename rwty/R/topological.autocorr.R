#' Calculate data for autocorrelation plots of tree topologies from MCMC analyses
#' 
#' This function takes a list of rwty.trees objects, and calculates the
#' median phylogenetic distance at a series of roughly even sampling intervals.
#' In really well behaved MCMC analyses, the median distance will stay constant
#' as the sampling interval increases. If there is autocorrelation, it will 
#' increase as the sampling interval increases, and is expected to level
#' off when the autocorrelation decreases to zero. The function calculates
#' path distances, though other distances could also be employed.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param autocorr.intervals The maximum number of sampling intervals to use 
#' @param squared TRUE/FALSE use squared tree distances (necessary to calculate approximate ESS)
#'
#' @return A data frame with one row per sampling interval, per chain. 
#' The first column is the sampling interval. The second column is the median 
#' path distance between pairs of trees from that sampling interval. The third
#' column is the chain ID.
#'
#' @keywords autocorrelation, path distance
#'
#' @export topological.autocorr
#' @examples
#' data(fungus)
#' topological.autocorr(fungus, burnin = 20)


topological.autocorr <- function(chains, burnin = 0, autocorr.intervals = 100, squared = FALSE){

    chains = check.chains(chains)

    chain = chains[[1]]

    indices = seq(from = burnin + 1, to = length(chain$trees), by = 1)   

    trees = lapply(chains, function(x) x[['trees']][indices])

    raw.autocorr = lapply(trees, tree.autocorr, autocorr.intervals, squared)

    final.autocorr = do.call("rbind", raw.autocorr)

    final.autocorr$chain = unlist(lapply(names(chains), rep, nrow(raw.autocorr[[1]])))

    rownames(final.autocorr) = NULL

    return(final.autocorr)


}


tree.autocorr <- function(tree.list, max.intervals = 100, squared = FALSE){

    if(!is.numeric(max.intervals)) stop("max.intervals must be a positive integer")
    if(max.intervals<1 | max.intervals%%1!=0) stop("max.intervals must be a positive integer")

    # this ensures that we can tell you if your ESS is < some threshold
    # the max(,2) bit is a fallback for extremely short tree lists
    max.thinning <- max(as.integer(length(tree.list)/21), 2)

    # we analyze up to max.intervals thinnings spread evenly, less if there are non-unique numbers
    thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=max.intervals)))
    r <- lapply(as.list(thinnings), get.sequential.distances, tree.list, squared = squared) 
    r <- data.frame(matrix(unlist(r), ncol=2, byrow=T))
    names(r) = c("topo.distance", "sampling.interval")

    return(r)
}

path.distance <- function(tree1, tree2){

    return(path.dist(list(tree1, tree2)))

}


path.dist.squared <- function (trees, check.labels = TRUE){

    pd = path.dist(trees, check.labels)

    return(pd*pd)

}


path.dist <- function (trees, check.labels = TRUE) 
{
    # a trimmed down version of the phangorn tree.dist function
    tree1 = trees[[1]]
    tree2 = trees[[2]]

    tree1 = unroot(tree1)
    tree2 = unroot(tree2)
    if (check.labels) {
        ind <- match(tree1$tip.label, tree2$tip.label)
        if (any(is.na(ind)) | length(tree1$tip.label) != length(tree2$tip.label)) 
            stop("trees have different labels")
        tree2$tip.label <- tree2$tip.label[ind]
        ind2 <- match(1:length(ind), tree2$edge[, 2])
        tree2$edge[ind2, 2] <- order(ind)
    }
    tree1 = reorder(tree1, "postorder")
    tree2 = reorder(tree2, "postorder")
    path.difference = NULL
    if (!is.binary.tree(tree1) | !is.binary.tree(tree2)) 
        warning("Trees are not binary!")

    tree1$edge.length = rep(1, nrow(tree1$edge))
    tree2$edge.length = rep(1, nrow(tree2$edge))

    # the commented code uses phangorn, which is faster
    # but the coph function is not exported, so we use
    # ape instead.
    #dt1 = phangorn:::coph(tree1)
    #dt2 = phangorn:::coph(tree2)

    dt1 = cophenetic.phylo(tree1)
    dt2 = cophenetic.phylo(tree2)
    dt1[upper.tri(dt1)] = 0
    dt2[upper.tri(dt2)] = 0

    path.difference = sqrt(sum((dt1 - dt2)^2))

    return(path.difference)
}


get.sequential.distances <- function(thinning, tree.list, N=100, squared = FALSE){
    
    # now thin out the input list
    keep <- seq(from=1, to=length(tree.list), by=thinning)
    
    # first we cut off any trailing trees from the input list
    if(length(keep)%%2 != 0) keep <- keep[1:(length(keep)-1)]
    
    # now we get the indices of the odd elements of keep
    # we will use these to make a pairwise list of sequential samples
    odds <- seq(from=1, to=length(keep), by=2)
    
    # we only look at N samples, allows for variation in effciency    
    if((length(odds))>N){
        odds <- sample(odds[1:(length(odds))], N, replace=FALSE)
        evens <- odds + 1 # indices of the tree2 trees in keep
        indices <- sort(c(odds, evens))
        keep <- keep[indices]
    }
    
    tree.list <- tree.list[keep]
    tree.index <- seq_along(tree.list)
    
    # turn the tree list into a list of sequential pairs
    # e.g. c(a, b, c, d, e) -> c(a,b), c(c,d)
    tree.pairs <- split(tree.list, ceiling(tree.index/2))
        
    if(squared == TRUE){
        distances <- lapply(tree.pairs, path.dist.squared)        
    }else{
        distances <- lapply(tree.pairs, path.dist)
    }

    distances <- as.numeric(unlist(distances))
    distances <- as.data.frame(distances)
    result <- apply(distances, 2, median)
    result <- data.frame('distance' = t(t(result)))
    result$sampling.interval <- thinning
    return(result)
}


