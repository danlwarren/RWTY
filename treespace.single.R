treespace.single <- function(trees, burnin=0, p.file = NULL){
    # do MDS on a single list of trees
    # p.file is a list of likelihoods...
    
    # for now this is hard-coded, who wants a 3D plot anyway, right?
    dimensions=2
    
    d <- tree.dist.matrix(trees[(burnin+1):length(trees)])
    
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

