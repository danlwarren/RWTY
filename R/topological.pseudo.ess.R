#' Calculate the pseudo Effective Sample Size (ESS) of tree topologies
#' 
#' This function takes a list of rwty.trees objects, and calculates the
#' pseudo ESS of the trees from each chain, after removing burnin. 
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
#' @param treedist the type of tree distance metric to use, can be 'PD' for path distance or 'RF' for Robinson Foulds distance
#'
#' @return A data frame with one row per chain, and columns describing the
#' median ESS, the upper and lower 95% confidence intervals based on the
#' replicates performed, and the name of the chain. 
#'
#' @keywords treespace, tree distance, path distance
#'
#' @export topological.pseudo.ess
#' @examples
#' data(fungus)
#' topological.pseudo.ess(fungus, burnin = 20, n = 20)



topological.pseudo.ess <- function(chains, burnin = 0, n = 20, treedist = 'PD'){
  
    chains <- check.chains(chains)
    
    chain <- chains[[1]]

    indices <- seq(from = burnin + 1, to = length(chain$trees), by = 1)   
    
    trees <- lapply(chains, function(x) x[['trees']][indices])
    
    raw.ess <- lapply(trees, tree.ess.multi, n, treedist)
    
    final.ess <- data.frame(t(matrix(unlist(raw.ess), nrow = length(chains), byrow = T)))

    colnames(final.ess) <- names(chains)

    return(final.ess)
  
  
}

tree.ess <- function(tree.list, treedist = 'PD'){  
    
    i <- sample(1:length(tree.list), 1)

    distances <- tree.distances(tree.list, i, treedist = treedist)

    ESS <- apply(distances, 2, effectiveSize)
    return(as.numeric(ESS))
}


tree.ess.multi <- function(tree.list, n=20, treedist = 'PD'){  
    
    print(sprintf("Calculating pseudo ESS for %s trees and %s replicates, please be patient", length(tree.list), n))

    data <- replicate(n, tree.ess(tree.list, treedist))

    return(data)
  
}


tree.distances <- function(tree.list, i = 1, treedist = 'PD', focal.tree = NA){

    if(is.na(focal.tree)){
        focal.tree = tree.list[[i]]
    }else{
        focal.tree = focal.tree[[1]]
    }


    if(treedist == 'PD'){
        distances <- data.frame(matrix(unlist(lapply(tree.list, path.distance, focal.tree)), nrow=length(tree.list), byrow=T))
    }else if(treedist == 'RF'){
        distances <- data.frame(matrix(unlist(lapply(tree.list, rf.distance, focal.tree)), nrow=length(tree.list), byrow=T))
    }else{
        stop("Unknown option for treedist. Valid options are 'PD' (for path distance) or 'RF' (for Robinson Foulds distance). Please try again")
    }

    names(distances) = c("topological.distance")
    return(distances)
}


tree.distances.from.first <- function(chains, burnin = 0, n = 50, focal.tree = NA, treedist = 'PD'){
    # return tree distances from the first tree of each chain
    # OR from the focal.tree if that argument is passed


    chains <- check.chains(chains)
    
    chain <- chains[[1]]

    indices <- seq(from = burnin + 1, to = length(chain$trees), by = 1)   
    
    trees <- lapply(chains, function(x) x[['trees']][indices])
    
    if(is.na(focal.tree)){
        distances <- lapply(trees, tree.distances, treedist = treedist)        
    }else{
        distances <- lapply(trees, tree.distances, focal.tree = focal.tree, treedist = treedist)
    }


    names <- lapply(names(chains), rep, length(indices))

    gens <- rep(indices*chains[[1]]$gens.per.tree, length(chains))

    dist.df <- data.frame(topological.distance = unlist(distances), chain = unlist(names), generation = gens)
    
    return(dist.df)
  
  
}

