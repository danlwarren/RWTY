get.sequential.distances <- function(tree.list, thinning = 1){
    
    # now thin out the input list
    keep <- seq(from=1, to=length(tree.list), by=thinning)
    
    # first we cut off any trailing trees from the input list
    if(length(keep)%%2 != 0) keep <- keep[1:(length(keep)-1)]
    
    # we only want to look at 100 samples, for efficiency
    odds <- seq(from=1, to=length(keep), by=2) #indices of the tree1 trees in keep
    if((length(odds))>100){
        odds <- sample(odds[1:(length(odds))], 100, replace=FALSE)
        evens <- odds + 1 # indices of the tree2 trees in keep
        indices <- sort(c(odds, evens))
        keep <- keep[indices]
    }
    
    tree.list <- tree.list[keep]
    tree.index <- seq_along(tree.list)
    
    # turn the tree list into a list of sequential pairs
    # e.g. c(a, b, c, d, e) -> c(a,b), c(c,d)
    tree.pairs <- split(tree.list, ceiling(tree.index/2))
    
    distances <- lapply(tree.pairs, tree.distance)
    distances <- as.numeric(unlist(distances))
    distances
}