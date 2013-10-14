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

# This function takes a vector of clades and a list of trees
# and uses the TipLabel attribute of the tree list to return
# clades using tip names instead of numbers.

parse.clades <- function(clades, treelist){
    
    tipnames <- treelist$TipLabel
    
    output <- vector(mode="character", length=length(clades))
    
    for(i in 1:length(clades)){
        # Convert factor to set of indices
        nums <- as.numeric(unlist(strsplit(as.character(clades[i]), split=" ")))
        
        # Convert indices to tip names
        output[i] <- paste(treelist$tip.label[[1]][nums], collapse=", ")
    }
    
    output
}