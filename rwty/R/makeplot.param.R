#' Plotting parameters
#' 
#' Plots parameter values over the length of the MCMC chain
#'
#' @param chains A set of rwty.trees objects.
#' @param burnin The number of trees to omit as burnin. 
#' @param parameter The column name of the parameter to plot.
#' @param facet Boolean denoting whether to make a facet plot.
#'
#' @return param.plot Returns a ggplot object.
#'
#' @keywords parameter, plot, convergence, mcmc, phylogenetics
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' makeplot.param(fungus, burnin=100, parameter="pi.A.")

makeplot.param <- function(chains, burnin = 0, parameter = "LnL", facet=TRUE){ 

    chains = check.chains(chains)
    ptable = merge.ptables(chains, burnin)

    if(parameter %in% names(ptable)){

        # get ESS values
        ess <- unlist(lapply(chains, FUN = function(x) effectiveSize( mcmc(x$ptable[parameter][(burnin+1):length(x$ptable[[parameter]]),]) )))
        ess <- round(ess, digits = 0)
        labels = paste(names(chains), " (ESS=", ess, ")", sep="")
        names(chains) = labels
        ptable = merge.ptables(chains, burnin)

        param.plot =  ggplot(ptable, aes_string(x="generation", y=parameter)) + 
                        geom_line(aes(colour = chain)) + 
                        ggtitle(parameter) +
                        theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5))

        if(facet) param.plot = param.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")

        return(param.plot)

    }else{
        stop(sprintf("The variable '%s' is not a column in the table of parameters you have supplied", parameter))
    }

}