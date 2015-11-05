#' Function for printing rwty.trees objects
#' 
#' This function is automatically called when printing a rwty.trees object
#' 
#'
#' @param x A rwty.trees object
#' @param ... Other arguments to be passed on to next function
#' 
#' @return A summary of the contents of the chain
#'
#' @keywords MCMC, phylogenetics, convergence, plot, awty, rwty
#'
#' @export
#' @examples
#' data(fungus)
#' fungus$Fungus.Run1

print.rwty.trees <- function(x, ...){
  print(summary(x))
}