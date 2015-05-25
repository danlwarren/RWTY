#' Function for printing rwty.trees objects
#' 
#' This function is automatically called when printing a rwty.trees object
#' 
#'
#' @param x A rwty.trees object
#' 
#' @return A summary of the contents of the chain
#'
#' @keywords MCMC, phylogenetics, convergence, plot, awty, rwty
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' run1

print.rwty.trees <- function(x){
  print(summary(x))
}