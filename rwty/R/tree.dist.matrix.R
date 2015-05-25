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
#' @export
#' 
#' @examples
#' data(fungus)
#' tree.dist.matrix(list(run1$trees[[1]], run1$trees[[2]], run1$trees[[3]]))

tree.dist.matrix <- function(trees){
    if (class(trees) != "multiPhylo") 
        stop("trees should be an object of class \"multiPhylo\"")

    N <- length(trees)

    treenames <- 1:N
    
    #Create an empty matrix for results
    RF <- matrix(0, N, N)
        
    for(i in 1:(N-1)){
        #print(paste("Tree", i, "of", N))        
        for(j in (i+1):N){
            RFd <- RF.dist(trees[[i]],trees[[j]])
            RF[i,j]<-RF[j,i]<-RFd
        }
    }

    #Row and column names
    rownames(RF) <- treenames
    colnames(RF) <- treenames
    
    RF
}




        
