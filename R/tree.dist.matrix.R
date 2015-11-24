#' Tree distance matrix calculation
#' 
#' This function takes a list of trees and returns a distance matrix
#' populated with Robinsoun-Foulds tree distances between all trees
#' in the list.
#'
#' @param trees a multiPhylo object 
#'
#' @return RF A distance matrix of RF distances 
#'
#' @keywords treespace, tree distance, robinson-foulds
#'
#' @export tree.dist.matrix
#' @examples
#' data(fungus)
#' tree.dist.matrix(fungus$Fungus.Run1$trees)

tree.dist.matrix <- function(trees){
    if (class(trees) != "multiPhylo") 
        stop("trees should be an object of class \"multiPhylo\"")

    N <- length(trees)

    treenames <- 1:N
    
    # use the RF.dist function from Phangorn. Thanks to Klaus Vigo for pointing
    # out that we were doing this all wrong before!
    # https://github.com/danlwarren/RWTY/issues/47
    RF <- as.matrix(RF.dist(trees))

    #Row and column names
    rownames(RF) <- treenames
    colnames(RF) <- treenames
    
    RF
}




        
