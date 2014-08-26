#' Custom functions to load tree lists so that rwty can do basic processing on the way in.
#' 
#' Loads trees, looks for a .p file of tree likelihoods, returns and rwty.trees object containing both
#'
#' @param file A path to a .t file containing an MCMC chain of trees
#' @param type An argument that designates the type of tree file.  If "nexus",
#' trees are loaded using ape's read.nexus function.  Otherwise, it's read.tree.
#'
#' @return output An rwty.trees object containing the multiPhylo and the table of values from the .p file if available.
#'
#' @keywords Phylogenetics, MCMC, load
#'
#' @export
#' 
#' @examples
#' load.trees(file="mytrees.nex", type="nexus")

#sample that works
load.trees <- function(file, type="nexus", gens.per.tree=NA, trim=1){
    
    # Read in trees
    print("Reading trees...")
    if(type == "nexus") {treelist <- read.nexus(file=file)}
    else {treelist <- read.tree(file=file)}
    treelist <- treelist[seq(from=1, to=length(treelist), by=trim)]
    if(is.na(gens.per.tree)){
        gens.per.tree <- as.numeric(strsplit(x=names(treelist)[3], split="[[:punct:]]")[[1]][-1]) - 
            as.numeric(strsplit(x=names(treelist)[2], split="[[:punct:]]")[[1]][-1])    
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
    pfile <- NA
    # Check for .p file, read it in if it exists
    if(length(grep(".t$", file, perl=TRUE)) > 0){pfile <- sub(".t$", ".p", file, perl=TRUE)}
    if(length(grep(".trees$", file, perl=TRUE)) > 0){pfile <- sub(".trees$", ".p", file, perl=TRUE)}
    if(file.exists(pfile)){
        print(paste("Reading p values from", pfile))
        ptable <- read.table(pfile, skip=1, header=TRUE)
        ptable <- ptable[seq(from=1, to=length(ptable[,1]), by=trim),]
    }
    
    output <- list(
        "trees" = treelist, 
        "ptable" = ptable, 
        "gens.per.tree" = gens.per.tree)
    
    class(output) <- "rwty.trees"
    
    output
}