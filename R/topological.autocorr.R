#' Calculate data for autocorrelation plots of tree topologies from MCMC analyses
#' 
#' This function takes a list of rwty.trees objects, and calculates the
#' mean phylogenetic distance at a series of roughly even sampling intervals.
#' In really well behaved MCMC analyses, the mean distance will stay constant
#' as the sampling interval increases. If there is autocorrelation, it will 
#' increase as the sampling interval increases, and is expected to level
#' off when the autocorrelation decreases to zero. The function calculates
#' path distances, though other distances could also be employed.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param autocorr.intervals The number of sampling intervals to use. These will be spaced evenly between 1 and the max.sampling.interval 
#' @param max.sampling.interval The largest sampling interval for which you want to calculate the mean distance between pairs of trees (default is 10 percent of the length of the chain).
#' @param squared TRUE/FALSE use squared tree distances (necessary to calculate approximate ESS)
#' @param treedist the type of tree distance metric to use, can be 'PD' for path distance or 'RF' for Robinson Foulds distance
#'
#' @return A data frame with one row per sampling interval, per chain. 
#' The first column is the sampling interval. The second column is the mean 
#' path distance between pairs of trees from that sampling interval. The third
#' column is the chain ID.
#'
#' @keywords autocorrelation, path distance
#'
#' @export topological.autocorr
#' @examples
#' data(fungus)
#' topological.autocorr(fungus, burnin = 20)


topological.autocorr <- function(chains, burnin = 0, max.sampling.interval = NA, autocorr.intervals = 100, squared = FALSE, treedist = 'PD'){

    chains = check.chains(chains)

    chain = chains[[1]]

    if(is.na(max.sampling.interval)){
        max.sampling.interval = floor(length(chain$trees) - (0.9*length(chain$trees)))
    }

    indices = seq(from = burnin + 1, to = length(chain$trees), by = 1)   

    trees = lapply(chains, function(x) x[['trees']][indices])

    raw.autocorr = lapply(trees, tree.autocorr, max.sampling.interval, autocorr.intervals, squared, treedist)

    final.autocorr = do.call("rbind", raw.autocorr)

    final.autocorr$chain = unlist(lapply(names(chains), rep, nrow(raw.autocorr[[1]])))

    rownames(final.autocorr) = NULL

    return(final.autocorr)


}


tree.autocorr <- function(tree.list, max.sampling.interval = NA, autocorr.intervals = 100, squared = FALSE, treedist = 'PD'){

    if(!is.numeric(autocorr.intervals)) stop("autocorr.intervals must be a positive integer")
    if(autocorr.intervals<1 | autocorr.intervals%%1!=0) stop("autocorr.intervals must be a positive integer")

    # this ensures that we can tell you if your ESS is < some threshold
    # the max(,2) bit is a fallback for extremely short tree lists
    max.thinning <- max.sampling.interval

    if(max.thinning > (length(tree.list) - 100)) {
        max.thinning = length(tree.list) - 100
    }

    # we analyze up to autocorr.intervals thinnings spread evenly, less if there are non-unique numbers
    thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=autocorr.intervals)))
    r <- lapply(as.list(thinnings), get.sequential.distances, tree.list, squared = squared, treedist = treedist) 
    r <- data.frame(matrix(unlist(r), ncol=2, byrow=T))
    names(r) = c("topo.distance", "sampling.interval")

    return(r)
}

path.distance <- function(tree1, tree2){

    return(path.dist(list(tree1, tree2)))

}


rf.distance <- function(tree1, tree2){

    return(rf.dist(list(tree1, tree2)))

}

rf.dist.squared <- function(trees){

    rf = rf.dist(trees)

    return(rf*rf)

}


rf.dist <- function(trees){

    tree1 = trees[[1]]
    tree2 = trees[[2]]
    rf = RF.dist(tree1, tree2)
    return(rf)

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


get.sequential.distances <- function(thinning, tree.list, N=100, squared = FALSE, treedist = 'PD'){
    
    starts = 1:(length(tree.list) - thinning)
    ends = starts + thinning
    keep = c(rbind(starts, ends))
    tree.list <- tree.list[keep]
    tree.index <- seq_along(tree.list)
    
    # turn the tree list into a list of sequential pairs
    # e.g. c(a, b, c, d, e) -> c(a,b), c(c,d)
    tree.pairs <- split(tree.list, ceiling(tree.index/2))
        
    if(treedist == 'PD'){
        if(squared == TRUE){
            distances <- lapply(tree.pairs, path.dist.squared)        
        }else{
            distances <- lapply(tree.pairs, path.dist)
        }
    }else if(treedist == 'RF'){
        if(squared == TRUE){
            distances <- lapply(tree.pairs, rf.dist.squared)        
        }else{
            distances <- lapply(tree.pairs, rf.dist)
        }
    }else{
        stop("Unknown option for treedist. Valid options are 'PD' (for path distance) or 'RF' (for Robinson Foulds distance). Please try again")
    }

    distances <- as.numeric(unlist(distances))
    distances <- as.data.frame(distances)
    result <- apply(distances, 2, mean)
    result <- data.frame('distance' = t(t(result)))
    result$sampling.interval <- thinning
    return(result)
}


