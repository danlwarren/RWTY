#custom functions to load tree lists so that we can do basic processing on the way in

#sample that works
load.trees <- function(x){
    
    # Read in trees
    treelist <- read.nexus(file=x)
    
    # Unroot all trees
    treelist <- lapply(treelist, unroot)
    
    # Reset class
    class(treelist) <- "multiPhylo"
    
    # Check for .p file, read it in if it exists
    pfile <- sub(".t$", ".p", x, perl=TRUE)
    if(file.exists(pfile)){
        print(paste("Reading p values from", pfile))
        ptable <- read.table(pfile, skip=1, header=TRUE)
    }
    
    output <- list("trees" = treelist, "ptable" = ptable)
    
    class(output) <- "rwty.trees"
}