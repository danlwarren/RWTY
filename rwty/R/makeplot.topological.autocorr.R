
makeplot.topological.autocorr.R <- function(chains, burnin = 0, max.intervals = 100, facet = FALSE){

    dat <- topological.autocorr(chains, burnin, max.intervals)

    autocorr.plot = ggplot(data=dat, aes(x=sampling.interval, y=Path.distance)) + 
            geom_line(alpha=0.2, aes(colour = chain)) + geom_point(size = 2, aes(colour = chain))

    if(facet) autocorr.plot = autocorr.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")

    return(list('plot'=p, 'distances'=dat, 'random distance' = random.interval))
    
}
