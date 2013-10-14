plot.tree.ess <- function(tree.ess.object){
    
    dat <- tree.ess.object$tree.distances
    
    t <- ggplot(data=dat, aes(x=gapsize, y=average.distance)) + 
        geom_hline(yintercept=tree.ess.object$reference.distance, linetype='dashed') +
        geom_hline(yintercept=tree.ess.object$reference.distance.lowerCI, linetype='dashed', colour="gray") +
        geom_point(size = 2, aes(color=lower)) + 
        theme(legend.position="none")
    
    
    t
    
}
