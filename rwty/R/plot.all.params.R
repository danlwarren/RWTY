



plot.all.params <- function(chains, burnin = 0, facet=TRUE, strip = 1){

    chains = check.chains(chains)
    chain = chains[[1]]
    if(is.null(chain$ptable)) stop("No parameters associated with your chains")

    params <- names(chain$ptable)[-strip]

    param.plots <- lapply(params, FUN = function(x) plot.param(param = x, burnin = burnin, chains = chains, facet = facet))

    names(param.plots) <- params

    return(param.plots)
}