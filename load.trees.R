#custom functions to load tree lists so that we can do basic processing on the way in

#sample that works
load.trees <- function(file){
    
    # Read in trees
    treelist <- read.nexus(file=file)
    
    # Unroot all trees.  Can't use lapply because it was
    # deleting tip labels.
    for(i in 1:length(treelist)){
        treelist[[i]] <- unroot(treelist[[i]])
    }
    
    # Reset class
    class(treelist) <- "multiPhylo"
    
    ptable <- NA
    
    # Check for .p file, read it in if it exists
    pfile <- sub(".t$", ".p", file, perl=TRUE)
    if(file.exists(pfile)){
        print(paste("Reading p values from", pfile))
        ptable <- read.table(pfile, skip=1, header=TRUE)
    }
    
    output <- list("trees" = treelist, "ptable" = ptable)
    
    class(output) <- "rwty.trees"
    
    output
}