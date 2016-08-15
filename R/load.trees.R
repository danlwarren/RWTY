#' Custom functions to load tree lists so that rwty can do basic processing on the way in.
#'
#' Loads trees, looks for a log file of tree likelihoods and parameter values, returns an rwty.trees object containing both
#'
#' @param file A path to a tree file containing an MCMC chain of trees
#' @param type An argument that designates the type of tree file.  If "nexus",
#' trees are loaded using ape's read.nexus function.  Otherwise, it's read.tree.
#' If a "format" argument is passed, type will be determined from the format definition.
#' @param format File format, which is used to find tree and log files.
#' Currently accepted values are "mb" for MrBayes, "beast" for BEAST, and "*beast" for *BEAST.
#' If you would like RWTY to understand additional formats, please contact the authors and send us some sample data.
#' @param gens.per.tree The number of generations separating trees.  If not provided, RWTY will attempt to calculate it automatically.
#' @param trim Used for thinning the chain.  If a number N is provided, RWTY keeps every Nth tree.
#' @param logfile A path to a file containing model parameters and likelihoods.  If no path is provided but
#' a "format" argument is supplied, RWTY will attempt to find the log file automatically based on the format
#' definition.
#' @param skip The number of lines that must be skipped to get to the header of the log file.
#' MrBayes, for instance, prints a comment line at the top of the log file, so MrBayes files should be
#' read in with a skip value of 1.  If no "skip" value is provided but a "format" is supplied, RWTY will
#' attempt to read logs using the skip value from the format definition.
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
  if(type == "nexus") {
    treelist <- read.nexus(file=file)
  } else if(type=="revbayes") {
    tmp <- read.revbayestrees(file=file)
    treelist <- tmp$tree
    rb_ptable <- tmp$param
  } else {
    treelist <- read.tree(file=file)
  }
  
  treelist <- treelist[seq(from=1, to=length(treelist), by=trim)]

  if(is.na(gens.per.tree)){
    if(type=="revbayes") {
      gens.per.tree <- rb_ptable[2,"Iteration"] - rb_ptable[1,"Iteration"]
    } else {
      gens.per.tree <- as.numeric(tail(strsplit(x=names(treelist)[3], split="[[:punct:]]")[[1]], 1)) -
        as.numeric(tail(strsplit(x=names(treelist)[2], split="[[:punct:]]")[[1]], 1))
    }
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
  
    # add any columns from rb_ptable (from treefile) that are not already in ptable (from log)
    if(format=="revbayes") {
      to_add<-!(colnames(rb_ptable) %in% colnames(ptable))
      if(sum(to_add)>0)
        ptable<-cbind(rb_ptable[,to_add], ptable)
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

  # Default behavior for revbayes files
  if(format == "revbayes"){
    return(list(
      trees.suffix = ".trees",
      log.suffix = ".log",
      type = "revbayes",
      skip = 0
    ))
  }

}



read.revbayestrees<-function(file) {
  filelines<-readLines(file)
  column.names<-strsplit(filelines[1], split="\t")[[1]]
  data<-strsplit(filelines[-1], split="\t")
  samplerow<-data[[1]]
  treecheck<-unlist(lapply(samplerow, FUN=isTree))
  param<-matrix(ncol=sum(!treecheck), nrow=length(data))
  colnames(param)<-column.names[!treecheck]
  for(i in 1:length(data))
    param[i,]<-as.numeric(data[[i]][!treecheck])
  tree<-list()
  for(i in 1:length(data)) {
    tree[[i]]<-read.tree(text=data[[i]][treecheck])
  }
  class(tree)<-"multiPhylo"
  return(list(tree=tree, param=param))
}

isTree<-function(x) {
  !is.null(try(read.tree(text=x)))
}