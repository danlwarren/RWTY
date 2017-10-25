#' Plot convergence metrics for nodes on a consensus tree
#' 
#' Takes a list of rwty.chain objects.  
#' Plots the standard deviation or wcsf for nodes in the consensus tree for each chain.  These values represent the consistency of the posterior probability estimate for that node along the chain.
#' The function returns one tree per chain.
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#' @param p The minimum frequency of a bipartition to appear in the consensus tree.
#' @param rank ('wcsf', 'sd'). How to rank the clades? By default, we plot the 20 'worst' clades. This parameter sets the definition of 'worst'. The default is to rank the by the weighted change in split frequencies (rank = 'wcsf'). This works by looking at the change in the cumulative split frequency over the course of the MCMC, and ranks the worst chains as those that do not level off (i.e. those that have changes near the end). We do this because in a well-behaved chain, we expect the cumulative split frequencies to level off once the chain has been run for long enough. So, any cumulative split frequencies which are still changing towards the end of your run are likely to indicate problematic clades. Specifically, we multiply the absolute change in split frequencies for each clade by a set of weights that increase linearly towards the end of the chain (the first observation gets a weight of zero, the final observation gets a weight of one). The original AWTY ranked clades by their standard deviations (higher SD = worse), so we include this as an option too. To do this, just set rank = 'sd'.
#'
#' @return cumulative.tree A consensus tree for each chain with node values representing the wcsf or standard deviation for that node.
#'
#' @keywords convergence, mcmc, phylogenetics, plot
#'
#' @export makeplot.cumulative.tree
#' @examples
#' data(fungus)
#' makeplot.cumulative.tree(fungus, burnin = 20, window.size = 20)

makeplot.cumulative.tree <- function(chains, burnin = 0, window.size = 20, rank = 'wcsf', p = 0.5){ 
  
  print(sprintf("Creating cumulative split convergence trees"))
  if(rank == 'wcsf'){ 
    RANK = "WCSF"
  }else if(rank == 'sd'){ 
    RANK = "StDev"
  }else{
    stop("'rank' must be either 'sd' or 'wcsf'")
  }
  
  chains <- check.chains(chains)
  cumulative.freq.list <- cumulative.freq(chains, burnin = burnin, window.size = window.size)

  convergence.trees <- lapply(chains, function(x) consensus(x$trees, p = p))
  
  for(i in 1:length(convergence.trees)){
    convergence.trees[[i]]$node.label <-sapply(subtrees(convergence.trees[[i]]), 
                                               function(x) get.convergence.diagnostic(x$tip.label, cumulative.freq.list[[names(convergence.trees)[[i]]]], contree, "sd"))
  }
  
  return(convergence.trees)
}


# This function takes a vector of tip labels, a frequency table from cumulative.fre1,
# a tree, and a metric (wcsf or sd), and returns the value of that metric
# for that clade
get.convergence.diagnostic <- function(tip.labels, freqtable, tree, metric = "wcsf"){
  trans <- unlist(freqtable$translation)
  trans.ind <- which(sapply(trans[,"Tip names"], 
                            function(x) all(match(unlist(strsplit(x, ", ")), tip.labels) & 
                                              all(match(tip.labels, unlist(strsplit(x, ", ")))))))
  clade.num <- freqtable$translation[trans.ind, "Clade number"]
  
  if(identical(clade.num, character(0))){
    tip.labels <- tree$tip.label[!tree$tip.label %in% tip.labels] 
    trans.ind <- which(sapply(trans[,"Tip names"], 
                              function(x) all(match(unlist(strsplit(x, ", ")), tip.labels) & 
                                                all(match(tip.labels, unlist(strsplit(x, ", ")))))))
    clade.num <- freqtable$translation[trans.ind, "Clade number"]
  }
  
  unc <- freqtable$cumulative.table[clade.num, metric]
  return(unc)
}


