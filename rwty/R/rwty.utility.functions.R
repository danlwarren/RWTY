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
        stop("The length of all of the MCMC chains in the 'chains' object must be equal")
    }

    # check all points sampled from the same points in the mcmc
    gens = unique(unlist(lapply(chains, FUN = function(x) x$gens.per.tree)))
    if(length(gens) != 1){
        stop("All MCMC chains in the 'chains' list must be sampled at the same intervals")
    }

    # label the chains, and check user-supplied labels
    if((is.na(labels))){
        labels <- c(paste("Chain", seq(1:length(chains)), sep="."))
    }

    if(length(labels) != length(chains)){
        stop("The length of the 'labels' list must be equal to the number of chains you have supplied")
    }

    names(chains) = labels

    return(chains)

}

merge.ptables <- function(chains, burnin){

    chains = check.chains(chains)
    N = length(chains[[1]]$trees)

    if((N - burnin) < 1 | burnin < 0){
        stop(sprintf('Burnin must be between 0 and the length of your chains (%s)', N))
    }

    # merge p tables    
    chain = unlist(lapply(names(chains), function(x) rep(x, (N - burnin))))
    sample = as.numeric(rep((burnin + 1):N), length(chains))
    generation = (sample - 1) * chains[[1]]$gens.per.tree

    if(!is.null(chains[[1]]$ptable)){
        ptables = lapply(chains, function(x) x[['ptable']][(burnin + 1):N,])
        ptable = do.call("rbind", ptables)
        ptable$chain = chain
        ptable$sample = sample
        ptable$generation = generation
    }else{
        ptable = data.frame('chain' = chain, 'sample' = sample, 'generation' = generation)
    }

    return(ptable)

}