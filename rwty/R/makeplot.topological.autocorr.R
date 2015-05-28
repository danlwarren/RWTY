
plot.tree.autocorr <- function(tree.list){

    dat <- tree.autocorr(tree.list)

    tree.list.shuffled <- sample(tree.list, length(tree.list), replace=FALSE)   
    random.interval <- get.sequential.distances(thinning=1, tree.list.shuffled)

    p <- ggplot(data=dat, aes(x=sampling.interval, y=distance)) + 
            geom_line(alpha=0.2) + geom_point(size = 2) + 
            geom_hline(aes(yintercept = distance), random.interval, alpha = 0.5, linetype='dashed')

    return(list('plot'=p, 'distances'=dat, 'random distance' = random.interval))
    
}
