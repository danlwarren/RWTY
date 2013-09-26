#custom functions to load tree lists so that we can do basic processing on the way in

#sample that works
load.trees <- function(file, type="nexus"){
    
    # Read in trees
    print("Reading trees...")
    if(type == "nexus") {treelist <- read.nexus(file=file)}
    else {treelist <- read.tree(file=file)}
    
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