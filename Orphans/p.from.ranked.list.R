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

p.from.ranked.list <- function(null.dist, obs, type='smaller'){
    
    null.dist <- sort(null.dist)
    
    if(obs %in% null.dist){
        pos <- mean(which(null.dist==obs))
    } 
    else if(obs<min(null.dist)){
        pos <- 1
    }
    else if(obs>max(null.dist)){
        pos <- length(null.dist)+1
    }
    else{
        pos <- min(which(null.dist>obs))
        
    }
    
    if(type=='greater'){
        pos <- length(null.dist)+1 - pos
    }
    
    p <- pos/(length(null.dist)+1)
    p
}