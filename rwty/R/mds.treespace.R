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

mds.treespace <- function(trees, filenames=NA, burnin=0, step=1){
    # The argument to trees needs to be either a single multiphylo
    # or a list of phylo objects
    
    # Check for names of multiphylo objects, assign them from filenames
    #  if they aren't already set
    if(is.null(names(trees)) && is.na(filenames)){
        stop("Must provide either a named list of multiphylo objects or a vector of names.")
    }
    else if(is.null(names(trees))){
        names(trees) <- filenames
    }
    
    #Trim multiphylos down to only the trees we need and collapse into a single list
    for(i in 1:length(trees)){
        trees[[i]] <- trees[[i]][seq((burnin + 1), length(trees[[i]]), by = step)]
    }    
    
    trees
    
}


