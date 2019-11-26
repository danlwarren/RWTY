#' Function for merging p tables for multiple MCMC chains
#' 
#' This function is automatically called by some of the plot functions.
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
#'
#' @return ptable A data frame of likelihood values and model parameters for the supplied rwty.chain objects
#'
#' @keywords MCMC, phylogenetics, convergence, awty, rwty
#'
#' @export combine.ptables
#' @examples
#' \dontrun{
#' data(fungus)
#' combine.ptables(fungus, burnin=20)
#' }

combine.ptables <- function(chains, burnin=NA){
  
  chains = check.chains(chains)
  # set burnin to the maximum from across all chains
  if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
  
  # N is a vector of chain lengths
  N <- unlist(lapply(chains, function(x) length(x$trees)))
  
  if(burnin < 0){
    stop(sprintf('Burnin must be between 0 and the length of your chains (%s)', N))
  }
  
  if(any((N - burnin) < 1)){
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
