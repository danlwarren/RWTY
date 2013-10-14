mds.treespace <- function(trees, filenames=NA, burnin=0, step=1){
    # The argument to trees needs to be either a single multiphylo
    # or a list of phylo objects
    
    # Check for names of multiphylo objects, assign them from filenames
    #  if they aren't already set
    if(is.null(names(trees)) && is.na(filenames)){
        stop("Must provide either a named list of multiphylo objects or a vector of names.")
    }
    else if(is.null(names(trees))){
        names(trees) <- filenames
    }
    
    #Trim multiphylos down to only the trees we need and collapse into a single list
    for(i in 1:length(trees)){
        trees[[i]] <- trees[[i]][seq((burnin + 1), length(trees[[i]]), by = step)]
    }    
    
    trees
    
}


