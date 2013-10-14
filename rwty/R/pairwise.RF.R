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

pairwise.RF <- function(trees, n=length(trees)-1){
    #input a list of trees, get back a list of sequential RF distances
    #n is the number of comparisons you want to make. Useful for making sure you count the same number of instances.
    #the default is to look at all pairs of trees in the list
    
    RFs <- c()	
    for(tree.index in 1:(n)){
        print(tree.index)
        tmp <- RF.dist(trees[[tree.index]], trees[[tree.index+1]])
        RFs <- c(RFs, tmp)
    }	
    
    return(RFs)
}
