#' Calculate the approximate Effective Sample Size (ESS) of tree topologies
#' 
#' This function takes a list of rwty.trees objects, and calculates the
#' approximate ESS of the trees from each chain, after removing burnin. 
#' Each caulcation is repeated n times, where in each replicate a random
#' tree from the chain is chosen as a 'focal' tree. The calculation works
#' by calculating the path distance of each tree in the chain
#' from the focal tree, and calculating the ESS of the resulting vector
#' of phylogenetic distances using the effectiveSize function from the 
#' coda package. NB this function requires the calculation of many many
#' tree distances, so can take some time.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param n The number of replicate analyses to do 
#'
#' @return A data frame with one row per chain, and columns describing the
#' median ESS, the upper and lower 95% confidence intervals based on the
#' replicates performed, and the name of the chain. 
#'
#' @keywords treespace, tree distance, path distance
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' topological.ess(fungus, burnin = 20, n = 10)



topological.ess <- function(chains, burnin = 0, n = 50){
  
    chains <- check.chains(chains)
    
    chain <- chains[[1]]

    indices <- seq(from = burnin + 1, to = length(chain$trees), by = 1)   
    
    trees <- lapply(chains, function(x) x[['trees']][indices])
    
    raw.ess <- lapply(trees, tree.ess.multi, n)
    
    final.ess <- data.frame(t(matrix(unlist(raw.ess), nrow = length(chains), byrow = T)))

    colnames(final.ess) <- names(chains)

    return(final.ess)
  
  
}

tree.ess <- function(tree.list){  
    
    i <- sample(1:length(tree.list), 1)

    distances <- data.frame(matrix(unlist(lapply(tree.list, path.distance, tree.list[[i]])), nrow=length(tree.list), byrow=T))    

    ESS <- apply(distances, 2, effectiveSize)
    return(as.numeric(ESS))
}


tree.ess.multi <- function(tree.list, n=20){  
    
    print(sprintf("Calculating approximate ESS for %s trees and %s replicates, please be patient", length(tree.list), n))

    data <- replicate(n, tree.ess(tree.list))

    return(data)
  
}

