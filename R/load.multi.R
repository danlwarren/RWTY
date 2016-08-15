#' Load all matching files from a directory into a list of rwty.trees objects
#'
#' Finds trees and log files based on format definition, returns rwty.trees objects containing both
#'
#' @param path The path to the directory containing tree and log files
#' @param format File format, which is used to find tree and log files.
#' Currently accepted values are "mb" for MrBayes, "beast" for BEAST, and "*beast" for *BEAST.
#' LJH: Trying to add support for format="revbayes"
#' If you would like RWTY to understand additional formats, please contact the authors and send us some sample data.
#' @param labels A vector of names to assign to chains as they are read in.
#' @param ... Further arguments to be passed to load.trees.
#' @return output A list of rwty.trees objects containing the multiPhylos and the tables of values from the log files if available.
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

  output
}
