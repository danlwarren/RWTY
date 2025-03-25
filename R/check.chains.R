#' Function for checking suitability of chains for rwty analyses, auto-generating labels, etc
#' 
#' This function is automatically called by many other functions, but can be run manually as well.
#' It performs a number of tests of chain format, labels, lengths, etc.
#'
#' @param chains A list of rwty.chain objects. 
#'
#' @return chains A list of rwty.chain objects
#'
#' @keywords MCMC, phylogenetics, convergence, awty, rwty
#'
#' @export check.chains
#' @examples
#' \dontrun{
#' data(fungus)
#' check.chains(fungus)
#' }

check.chains <- function(chains){
  
  # if passed a single trees object, turn it into a list
  if(inherits(chains, "rwty.chain")){
    chains <- list(chains)
  }
  
  # check chains is a list
  if(!inherits(chains, 'list')){
    stop("'chains' must be a list of rwty.chain objects")        
  }
  
  # check all chains are rwty.chain objects
  if(all(unlist(lapply(chains, FUN = class))!='rwty.chain')){
    stop("Each chain in the list 'chains' must be a rwty.chain object")
  }
  
  # check all chains have the same tips using first tree in each chain
  if(!all(unlist(lapply(chains, FUN = function (x) setequal(chains[[1]]$trees$tip.label, x$trees$tip.label))))){
    stop("All trees must have the same tip labels")
  }
  
  # check all chains are the same length
  if(length(unique(unlist(lapply(chains, FUN = function(x) length(x$trees))))) != 1){
    print("Chains of unequal length, pruning longer chains")
    ptable.min <- min(unlist(lapply(chains, function(x) length(x$ptable[,1]))))
    trees.min <- min(unlist(lapply(chains, function(x) length(x$trees))))
    for(i in 1:length(chains)){ # May be a way to lapply this, but for right now this works
      chains[[i]]$ptable <- chains[[i]]$ptable[1:ptable.min,]
      chains[[i]]$trees <- chains[[i]]$trees[1:trees.min]
    }
  }
  
  # checks to be run if ptables are supplied
  if(any(unlist(lapply(chains, function(x) length(x$ptable[,1])))) > 0){
    
    # check to see if ptable and trees are the same length
    if(any(unlist(lapply(chains, function(x) length(x$trees))) != 
             unlist(lapply(chains, function(x) length(x$ptable[,1]))))){
      stop("All MCMC chains must be the same length as their associated p tables")
    }
    
    # reduce ptables to the set of shared columns
    keepcols <- Reduce(intersect,lapply(chains, FUN = function(x) colnames(x$ptable)))
    # need to set up the following so it only prints when columns are removed
    col.flag <- FALSE
    for(i in 1:length(chains)){
      if(!setequal(colnames(chains[[i]]$ptable), keepcols)){col.flag <- TRUE}
      chains[[i]]$ptable <- chains[[i]]$ptable[,keepcols]
    }
    if(col.flag){
      print("Some non-shared columns found.  Keeping the following columns:")
      print(paste(keepcols))
    }
  }
  
  # check all points sampled from the same points in the mcmc
  gens = unique(unlist(lapply(chains, FUN = function(x) x$gens.per.tree)))
  if(length(gens) != 1){
    stop("All MCMC chains in the 'chains' list must be sampled at the same intervals")
  }

  # replace labels with auto-generated ones if there are not enough unique ones
  if(is.null(names(chains)) | length(unique(names(chains))) != length(chains)){
    names(chains) <- c(paste("Chain", seq(1:length(chains)), sep="."))
  }
  

  
  return(chains)
  
}