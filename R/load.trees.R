#' Custom functions to load tree lists so that rwty can do basic processing on the way in.
#' 
#' Loads trees, looks for a .p file of tree likelihoods, returns and rwty.trees object containing both
#'
#' @param file A path to a .t file containing an MCMC chain of trees
#' @param type An argument that designates the type of tree file.  If "nexus",
#' trees are loaded using ape's read.nexus function.  Otherwise, it's read.tree.
#' @param format File format, which is used to find tree and log files.  
#' Currently accepted values are "mb" for MrBayes, "beast" for BEAST, and "*beast" for *BEAST.
#' If you would like RWTY to understand additional formats, please contact the authors and send us some sample data.
#' @param gens.per.tree The number of generations separating trees.  If not provided, RWTY will attempt to calculate it automatically.
#' @param trim Used for thinning the chain.  If a number N is provided, RWTY keeps every Nth tree.
#' @param logfile A path to a file containing model parameters and likelihoods
#' @param skip The number of lines that must be skipped to get to the header of the log file.  
#' MrBayes, for instance, prints a comment line at the top of the log file, so MrBayes files should be
#' read in with a skip value of 1.
#' @return output An rwty.trees object containing the multiPhylo and the table of values from the log file if available.
#'
#' @keywords Phylogenetics, MCMC, load
#'
#' @export load.trees
#' @examples
#' #load.trees(file="mytrees.t", format = "mb")

load.trees <- function(file, type=NA, format = "mb", gens.per.tree=NA, trim=1, logfile=NA, skip=NA){
  
  file.format <- get.format(format)
  
  if(is.na(type)){
    type <- file.format$type 
  }
  
  if(is.na(skip)){
    skip <- file.format$skip
  }

  # Read in trees
  print("Reading trees...")
  if(type == "nexus") {treelist <- read.nexus(file=file)}
  else {treelist <- read.tree(file=file)}
  
  treelist <- treelist[seq(from=1, to=length(treelist), by=trim)]
  
  if(is.na(gens.per.tree)){
    gens.per.tree <- as.numeric(tail(strsplit(x=names(treelist)[3], split="[[:punct:]]")[[1]], 1)) - 
      as.numeric(tail(strsplit(x=names(treelist)[2], split="[[:punct:]]")[[1]], 1))  
  }
  
  print(paste(gens.per.tree, "generations per tree..."))
  
  # Unroot all trees.  Can't use lapply because it was
  # deleting tip labels.
  if(is.rooted(treelist[[1]])){
    print("Unrooting, this may take a while...")
    for(i in 1:length(treelist)){
      treelist[[i]] <- unroot(treelist[[i]])
    }
  }
  else{print("Trees are unrooted...")}
  
  
  # Reset class
  class(treelist) <- "multiPhylo"
  
  ptable <- NULL
  
  # Check whether log file path has been supplied and doesn't exist
  if(!is.na(logfile) && !file.exists(logfile)){
    stop(paste("Logfile not found at", logfile))
  }
  
  # logfile path has been supplied and file exists
  if(!is.na(logfile) && file.exists(logfile)){
    print(paste("Reading parameter values from", basename(logfile)))
    ptable <- read.table(logfile, skip=skip, header=TRUE)
    ptable <- ptable[seq(from=1, to=length(ptable[,1]), by=trim),]
  }
  
  # If logfile hasn't been supplied try to find it by searching
  if(is.na(logfile)){
    
    logfile <- sub(pattern = paste0(file.format$trees.suffix, "$"), file.format$log.suffix, file)
    
    if(!is.na(logfile) && file.exists(logfile)){
      print(paste("Reading parameter values from", basename(logfile)))
      ptable <- read.table(logfile, skip=skip, header=TRUE)
      ptable <- ptable[seq(from=1, to=length(ptable[,1]), by=trim),]
    } else {
      print(paste("Couldn't find", basename(logfile)))
    }

  }
  
  
  output <- list(
    "trees" = treelist, 
    "ptable" = ptable, 
    "gens.per.tree" = gens.per.tree)
  
  class(output) <- "rwty.trees"
  
  output
}

# This function takes the name of a format and returns a list containing important info about file suffixes and whatnot
get.format <- function(format){
  
  # Default behavior for MrBayes files
  if(format == "mb"){
    return(list(
      trees.suffix = ".t",
      log.suffix = ".p",
      type = "nexus",
      skip = 1
    ))
  }
  
  # Default behavior for *BEAST files
  if(format == "*beast"){
    return(list(
      trees.suffix = ".species.trees",
      log.suffix = ".log",
      type = "nexus",
      skip = 2
    ))
  }
  
  # Default behavior for BEAST files
  if(format == "beast"){
    return(list(
      trees.suffix = ".trees",
      log.suffix = ".log",
      type = "nexus",
      skip = 2
    ))
  }
  
}
