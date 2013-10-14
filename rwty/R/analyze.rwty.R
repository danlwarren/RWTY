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

# This function will hopefully be the primary user interface.  
# It will take one or more tree files, a few necessary arguments,
# and then automatically run all of the relevant analyzes and 
# generate the plots...

analyze.rwty <- function(chains, burnin, window.size, gens.per.tree, step=1, ...){
    
    # If a single rwty.trees object is passed, it goes to the analyze.single
    # function.  Otherwise it assumes that multiple rwty.trees objects
    # have been passed as a list.
    if(class(chains) == "rwty.trees"){
        print("Analyzing single chain...")
        analyze.single(chains, burnin, window.size,
                       gens.per.tree, step, ...)
    }
    
    else{
        print("Analyzing multiple chains...")
        analyze.multi(chains, burnin, window.size, gens.per.tree, step, ...)
    }
}