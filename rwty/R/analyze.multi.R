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

analyze.multi <- function(chains, burnin, window.size, gens.per.tree, step=1, labels=NA, ...){
    
    output <- list()
    
    # Name chains by list order if labels aren't supplied
    if(any(is.na(labels))){labels <- seq(1, length(chains))}
    
    # Run analyze single on each chain
    for(i in 1: length(chains)){
        output[[labels[i]]] <- c(output, analyze.single(chains[[i]], burnin, window.size, gens.per.tree, step, ... ))
    }
    
    output[["compare.n"]] <- compare.n(chains, setnames=labels, burnin)
    
    output
}