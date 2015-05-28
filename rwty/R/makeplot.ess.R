
makeplot.ess <- function(chains, burnin = 0, n = 50){

    chains = check.chains(chains)

    dat <- topological.ess(chains, burnin, n)

    ess.plot = ggplot(dat, aes(x=chain, y=median.ess, colour = chain)) + 
            geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), width=.1) +
            geom_point() +
            xlab("chain") +
            ylab("approximate ESS") +
            expand_limits(y=0)

    return(ess.plot)
    
}
