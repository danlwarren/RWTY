#' Plot posterior probabilities of clades over an MCMC chain
#' 
#' This function will take a set of chains and produce plots of posteriors 
#'
#' @param chains A list of one or more rwty.trees objects
#' @param burnin The number of samples to remove from the start of the chain as burnin
#'
#' @return A list of cumulative and sliding window plots
#'
#' @keywords plot, posterior probabilities, rwty
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' 
#' p <- plot.posteriors(chains = list(run1, run2), burnin = 100, likelihood = 'LnL')
#' 
#' # NB: these data indicate significant problems: the two chains are sampling very different parts of tree space
#'



plot.posteriors <- function(chains, burnin, window.num, ...){
  
  output <- list()
  labels <- names(chains)
  
  # Run analyze single on each chain
  for(i in 1: length(chains)){
    
    # window.size could just be calculated once at the moment, but I'm setting it up per-chain
    # in case we decide to allow for chains of different length
    window.size <- (length(chains[[i]]$trees) - burnin)/window.num
    this.pp <- analyze.single(chains[[i]], burnin, window.size, gens.per.tree=chains[[i]]$gens.per.tree, chain.name = labels[i], ... )
    output[[paste(labels[i], "Sliding Window Posterior Probability")]] <- this.pp[["slide.plot"]]
    output[[paste(labels[i], "Cumulative Posterior Probability")]] <- this.pp[["cumulative.plot"]]
    output[[paste(labels[i], "Sliding Window Variance")]] <- this.pp[["slide.variance.plot"]]
    output[[paste(labels[i], "Cumulative Variance")]] <- this.pp[["cumulative.variance.plot"]]
  }
   
  return(output)
}

  

