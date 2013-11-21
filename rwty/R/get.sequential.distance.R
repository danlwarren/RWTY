#' A one sentence description of what your function does
#' 
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces. 
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

get.sequential.distance <- function(tree.list, thinning = 1, N=100){
    
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
    
    #print(paste(length(tree.pairs), "pairs of trees"))
    
    distances <- lapply(tree.pairs, tree.distance)
    distances <- as.numeric(unlist(distances))
    distances
}

tree.distance <- function(two.trees){
    #d <- RF.dist(two.trees[[1]], two.trees[[2]])  
    d <- treedist(two.trees[[1]], two.trees[[2]])[3]  
    d
}
