#' Returns distance between two trees
#' 
#' Takes a list of two trees, returns Robinson-Foulds distance
#' between them. 
#'
#' @param two.trees A list containing two trees \code{two.trees}
#'
#' @return d A distance measurement
#'
#' @keywords tree distance, Robinson Foulds
#'
#' @export
#' 
#' @examples
#' tree.distance(list(tree1, tree2))

tree.distance <- function(two.trees){
    # type = 1: RF distance
    # type = 2: branch.score.difference
    # type = 3: path difference
    # type = 4: weighted path difference
    type = 1
    d <- treedist(two.trees[[1]], two.trees[[2]])[[type]]	
    d
}