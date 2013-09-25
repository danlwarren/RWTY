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


treespace.single <- function(trees, burnin=0, p.file = NULL){
    # do MDS on a single list of trees
    # p.file is a list of likelihoods...

    # for now this is hard-coded, who wants a 3D plot anyway, right?
    dimensions=2

    d <- tree.dist.matrix(trees[burnin+1:length(trees)])

    mds <- isoMDS(d, k=dimensions)

    points <- as.data.frame(mds$points)
    names(points) <- c("x", "y")
    points$mcmc.sample <- seq(from=1, to=nrow(points))
    mds$points <- points

    if(!is.null(p.file)){
        mds$points <- cbind(mds$points, p.file$LnL)
    }

    p <- plot.treespace(mds$points)

    r <- list("mds" = mds, "plot" = p)

}





