#' Plot posterior probabilities of clades over an MCMC chain
#' 
#' This function will take a set of chains and produce plots of posteriors 
#'
#' @param chains A list of one or more rwty.trees objects
#' @param burnin The number of samples to remove from the start of the chain as burnin
#' @param window.num The number of windows to use for generating plots
#' 
#' @return A list of cumulative and sliding window plots
#'
#' @keywords plot, posterior probabilities, rwty
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' test.chains <- list(run1, run2)
#' test.chains <- check.chains(test.chains)
#' p <- plot.posteriors(chains = test.chains, burnin = 100, window.num = 20)
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
    this.pp <- analyze.posteriors(chains[[i]], burnin, window.size, gens.per.tree=chains[[i]]$gens.per.tree, chain.name = labels[i], ... )
    output[[paste(labels[i], "Sliding Window Posterior Probability")]] <- this.pp[["slide.plot"]]
    output[[paste(labels[i], "Cumulative Posterior Probability")]] <- this.pp[["cumulative.plot"]]
    output[[paste(labels[i], "Sliding Window Variance")]] <- this.pp[["slide.variance.plot"]]
    output[[paste(labels[i], "Cumulative Variance")]] <- this.pp[["cumulative.variance.plot"]]
  }
   
  return(output)
}


analyze.posteriors <- function(chains, burnin=0, window.size, gens.per.tree=NA, chain.name, ...){
  
  print("Sliding window analysis...")
  slide.data <- slide.freq(chains, burnin, window.size, gens.per.tree)
  slide.plot <- plot.cladeprobs(slide.data$slide.table, ...) + ggtitle("Sliding Window Posterior Probability")
  slide.variance.plot <- plot.cladevar(slide.data$slide.table) + ggtitle("Sliding Window Variance")
  
  print("Cumulative analysis...")
  cumulative.data <- cumulative.freq(chains, burnin, window.size, gens.per.tree,
                                     slide.freq.table=slide.data)
  cumulative.plot <- plot.cladeprobs(cumulative.data$cumulative.table, ...) + ggtitle("Cumulative Posterior Probability")
  cumulative.variance.plot <- plot.cladevar(cumulative.data$cumulative.table) + ggtitle("Cumulative Variance")
  
  slide.plot <- slide.plot + ggtitle(paste(chain.name, "Sliding Window Posterior Probability"))
  slide.variance.plot <- slide.variance.plot + ggtitle(paste(chain.name, "Sliding Window Variance"))
  cumulative.plot <- cumulative.plot + ggtitle(paste(chain.name, "Cumulative Posterior Probability"))
  cumulative.variance.plot <- cumulative.variance.plot + ggtitle(paste(chain.name, "Cumulative Variance"))
  
  output <- list("slide.data" = slide.data, "slide.plot" = slide.plot, "slide.variance.plot" = slide.variance.plot,
                 "cumulative.data" = cumulative.data,"cumulative.plot" = cumulative.plot, "cumulative.variance.plot" = cumulative.variance.plot) 
}
  

