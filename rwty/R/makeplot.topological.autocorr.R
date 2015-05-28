
makeplot.autocorr <- function(chains, burnin = 0, max.intervals = 100, facet = FALSE){

    chains = check.chains(chains)

    dat <- topological.autocorr(chains, burnin, max.intervals)

    autocorr.plot = ggplot(data=dat, aes(x=sampling.interval, y=Path.distance)) + 
            geom_line(alpha=0.2, aes(colour = chain)) + geom_point(size = 2, aes(colour = chain)) + 
            xlab("median path distance") + ylab("sampling interval")

    if(facet) autocorr.plot = autocorr.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")

    return(autocorr.plot)
    
}
