#' Load all matching files from a directory into a list of rwty.chain objects
#'
#' Finds trees and log files based on format definition, returns rwty.chain objects containing both
#' Note that the mcc.tree in each chain will be the global mcc tree calculated from the complete set of post-burnin
#' trees across all chains.
#'
#' @param path The path to the directory containing tree and log files
#' @param format File format, which is used to find tree and log files.
#' Currently accepted values are "mb" for MrBayes, "beast" for BEAST, "*beast" for *BEAST, and "revbayes" for RevBayes.
#' If you would like RWTY to understand additional formats, please contact the authors and send us some sample data.
#' @param labels A vector of names to assign to chains as they are read in.
#' @param ... Further arguments to be passed to load.trees.
#' @return output A list of rwty.chain objects containing the multiPhylos and the tables of values from the log files if available.
#'
#' @keywords Phylogenetics, MCMC, load, trees
#'
#' @export load.multi
#' @examples
#' #load.multi(path = "~/my trees/", format = "*beast")

load.multi <- function(path = ".", format = "mb", labels=NA, ...){

  file.format <- get.format(format)

  output <- list()

  # Convert arguments to regex
  ext.tree <- paste0("\\",file.format$trees.suffix, "$")
  ext.p <- file.format$log.suffix

  # Find t and p files
  tfiles <- list.files(path, pattern=ext.tree, full.names=TRUE)
  pfiles <- unlist(lapply(tfiles, FUN = function(x) sub(ext.tree, ext.p, x)))
  
  if(format == "revbayes"){
    tfiles <- tfiles[-grep("\\.ase\\.", tfiles)]
    pfiles <- pfiles[-grep("\\.ase\\.", pfiles)]
    tfiles <- tfiles[-grep("\\.mcc\\.", tfiles)]
    pfiles <- pfiles[-grep("\\.mcc\\.", pfiles)]
  }

  if(length(tfiles) == 0){
    stop("Couldn't find any tree files")
  }

  # Step through tfiles, find log files if available, load chains
  for(i in 1:length(tfiles)){
    print(basename(tfiles[i]))
    if(file.exists(pfiles[i])){
      output[[i]] <- load.trees(tfiles[i], logfile = pfiles[i], format = format, ...)
    }
    else{
      output[[i]] <- load.trees(tfiles[i], format = format, ...)
    }
  }

  # Add names to chains
  if(is.na(labels)){
    names(output) <- lapply(tfiles, FUN = function(x) basename (x))
  }
  else{
    names(output) <- labels
  }

  
  # calculate global mcc tree and add to slots in chains
  chains = check.chains(output)
  global.mcc.tree = global.mcc.tree(chains)

  for(chain in chains){
    
    chain$mcc.tree = global.mcc.tree
    
  }
    
  beep("complete")
  
  output
}


global.mcc.tree <- function(chains, burnin=NA){
  
  N = length(chains[[1]]$trees)
  
  if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
  
  trees = chains[[1]]$trees[burnin:N]
  
  if(length(chains)>1){
      for(i in 2:length(chains)){
          newtrees = chains[[i]]$trees[burnin:N]
          trees = c(trees, newtrees)
    
      }
  }
  
  return(mcc(trees))
}