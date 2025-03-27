#' Function for printing rwty.chain objects
#' 
#' This function is automatically called when printing a rwty.chain object
#' 
#'
#' @param x A rwty.chain object
#' @param ... Other arguments to be passed on to next function
#' 
#' @return A summary of the contents of the chain
#'
#' @keywords MCMC phylogenetics convergence plot awty rwty
#'
#' @export
#' @examples
#' data(fungus)
#' fungus$Fungus.Run1

print.rwty.chain <- function(x, ...){
  print(summary(x))
}