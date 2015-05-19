#' Various small utility functions
#' 
#' A bunch of tiny utility functions that we need off and on.  Not intended for user interaction.
#'

continuous.distance <- function(two.trees){
    # type = 1: RF distance
    # type = 2: branch.score.difference
    # type = 3: path difference
    # type = 4: weighted path difference
    d <- abs(two.trees[1] - two.trees[2])	
    d
}

cummean <- function(x){
    r <- (cumsum(as.numeric(x)))/seq(1:length(x))
    r
}

abs.diffs <- function(x){
    d <- abs(diff(as.numeric(x)))
    d
}



check.chains <- function(chains, labels = NA){

    # check chains is a list
    if(class(chains)!='list'){
        stop("'chains' must be a list of rwty.trees objects")        
    }

    # check all chains are rwty.trees objects
    if(all(unlist(lapply(chains, FUN = class))!='rwty.trees')){
        stop("Each chain in the list 'chains' must be a rwty.trees object")
    }

    # check all chains are the same length
    if(length(unique(unlist(lapply(chains, FUN = function(x) length(x$trees))))) != 1){
      print("Chains of unequal length, pruning longer chains")
      ptable.min <- min(unlist(lapply(test.chains, function(x) length(x$ptable[,1]))))
      trees.min <- min(unlist(lapply(test.chains, function(x) length(x$trees))))
      for(i in 1:length(chains)){ # May be a way to lapply this, but for right now this works
        chains[[i]]$ptable <- chains[[i]]$ptable[1:ptable.min,]
        chains[[i]]$trees <- chains[[i]]$trees[1:trees.min]
      }
    }
    
    # check to see if ptable and trees are the same length
    if(any(unlist(lapply(test.chains, function(x) length(x$trees))) != 
             unlist(lapply(test.chains, function(x) length(x$ptable[,1]))))){
      stop("All MCMC chains must be the same length as their associated p tables")
    }

    # check all points sampled from the same points in the mcmc
    gens = unique(unlist(lapply(chains, FUN = function(x) x$gens.per.tree)))
    if(length(gens) != 1){
        stop("All MCMC chains in the 'chains' list must be sampled at the same intervals")
    }

    # label the chains, and check user-supplied labels
    if(any((is.na(labels))) | is.null(labels)){
        labels <- c(paste("Chain", seq(1:length(chains)), sep="."))
    }

    if(length(labels) != length(chains)){
        stop("The length of the 'labels' list must be equal to the number of chains you have supplied")
    }

    # replace labels with auto-generated ones if there are not enough unique ones
    if(is.null(names(chains)) | length(unique(names(chains))) != length(chains)){
        names(chains) = labels
    }

    return(chains)

}

merge.ptables <- function(chains, burnin){

    chains = check.chains(chains)
    # N is a vector of chain lengths
    N <- unlist(lapply(chains, function(x) length(x$trees)))

    if(any((N - burnin) < 1 | burnin < 0)){
        stop(sprintf('Burnin must be between 0 and the length of your chains (%s)', N))
    }

    # merge p tables    
    chain <- rep(names(chains), N - (burnin))
    sample <- as.vector(unlist(sapply(N, function(x) seq(burnin + 1, x))))
    generation <- (sample - 1) * chains[[1]]$gens.per.tree

    if(!is.null(chains[[1]]$ptable)){
        ptables <- lapply(seq_along(chains), function(i) chains[[i]][['ptable']][(burnin+1):N[i],])
        ptable = do.call("rbind", ptables)
        ptable$chain = chain
        ptable$sample = sample
        ptable$generation = generation
    }else{
        ptable = data.frame('chain' = chain, 'sample' = sample, 'generation' = generation)
    }

    return(ptable)

}
