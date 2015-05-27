#' Load all matching files from a directory into a list of rwty.trees objects
#' 
#' Loads trees, looks for a .p file of tree likelihoods, returns rwty.trees objects containing both
#'
#' @param path The path to the directory containing tree and log files
#' @param ext.tree The extension to be used in finding tree files.  Usually should be "t" or "trees", or something of that sort.
#' @param ext.p The extension to be used in finding log files.  
#' Basically the function is going to chop ext.tree off of each file name and glue ext.p onto it, so "myfile.t" will look for a logfile "myfile.p", if ext.tree = "t" and ext.p = "p".  
#' @return output A list of rwty.trees objects containing the multiPhylos and the tables of values from the log files if available.
#'
#' @keywords Phylogenetics, MCMC, load, trees
#'
#' @export
#' 
#' @examples
#' #load.multi(path = "~/my trees/", ext.tree = "t", ext.p = "log")

load.multi <- function(path = ".", ext.tree = "t", ext.p = "p", labels=NA, ...){
  
  output <- list()
  
  # Convert arguments to regex
  ext.tree <- paste0("\\.", ext.tree, "$")
  ext.p <- paste0(".", ext.p)
  
  # Find t and p files
  tfiles <- list.files(path, ext.tree)
  pfiles <- unlist(lapply(tfiles, FUN = function(x) sub(ext.tree, ext.p, x)))
  
  if(length(tfiles) == 0){
    stop("Couldn't find any tree files")
  }
  
  # Step through tfiles, find log files if available, load chains
  for(i in 1:length(tfiles)){
    print(tfiles[i])
    if(file.exists(pfiles[i])){
      output[[i]] <- load.trees(tfiles[i], logfile = pfiles[i], ...)
    }
    else{
      output[[i]] <- load.trees(tfiles[i], ...)
    }
  }
  
  # Add names to chains
  if(is.na(labels)){
    names(output) <- tfiles  
  }
  else{
    names(output) <- labels
  }
  
  output
}