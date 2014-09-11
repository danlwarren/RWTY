#' Sequential RF distances for a list of trees
#' 
#' input a list of trees, get back a list of sequential RF distances
#' n is the number of comparisons you want to make. Useful for making sure you count the same number of instances.
#' the default is to look at all pairs of trees in the list
#'
#' @param trees A multiPhylo object
#' @param n Number of comparisons
#'
#' @return output A vector of distances
#'
#' @keywords MCMC, phylogenetics, distance
#'
#' @export
#' 
#' @examples
#' pairwise.RF(mytrees)

pairwise.RF <- function(trees, n=length(trees)-1){
    
    
    RFs <- c()	
    for(tree.index in 1:(n)){
        print(tree.index)
        tmp <- RF.dist(trees[[tree.index]], trees[[tree.index+1]])
        RFs <- c(RFs, tmp)
    }	
    
    return(RFs)
}
