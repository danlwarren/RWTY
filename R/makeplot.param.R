#' Plotting parameters
#' 
#' Plots parameter values over the length of the MCMC chain
#'
#' @param chains A set of rwty.chain objects.
#' @param burnin The number of trees to omit as burnin. 
#' @param parameter The column name of the parameter to plot.
#' @param facet Boolean denoting whether to make a facet plot.
#' @param free_y TRUE/FALSE to turn free y scales on the facetted plots on or off (default FALSE). Only works if facet = TRUE.
#'
#' @return param.plot Returns a ggplot object.
#'
#' @keywords parameter, plot, convergence, mcmc, phylogenetics
#'
#' @export makeplot.param
#' @examples
#' \dontrun{
#' data(fungus)
#' makeplot.param(fungus, burnin=20, parameter="pi.A.")
#' }

makeplot.param <- function(chains, burnin = 0, parameter = "LnL", facet=TRUE, free_y=FALSE){ 

    print(sprintf("Creating trace for %s", parameter))


    chains = check.chains(chains)
    ptable = combine.ptables(chains, burnin)

    if(parameter %in% names(ptable)){

        # get ESS values
        ess <- unlist(lapply(chains, FUN = function(x) effectiveSize( mcmc(x$ptable[parameter][(burnin+1):length(x$ptable[[parameter]]),]) )))
        ess <- round(ess, digits = 0)
        labels = paste(names(chains), " (ESS=", ess, ")", sep="")
        names(chains) = labels
        ptable = combine.ptables(chains, burnin)
        title = paste(parameter, "trace")
        
        # Calculate CIs either by chain or overall
        in.ci <- function(x){
          as.numeric(x > quantile(x, c(0.025)) &  x < quantile(x, c(0.975)))
        }
        
        if(facet){
          fill <- unlist(lapply(unique(ptable$chain), function(x) in.ci(ptable[ptable$chain == x,parameter])))
        } else {
          fill <- in.ci(ptable[,parameter])
        }
        fill[which(fill == 0)] = 'red'  
        fill[which(fill == 1)] = 'blue'

        trace.plot =  ggplot(ptable, aes_string(x="generation", y=parameter)) + 
                        geom_line(aes_string(colour = "chain")) + 
                        ggtitle(title) +
                        xlab("Generation") + 
                        scale_color_viridis(discrete = TRUE, end = 0.85) 
            

        density.plot =  ggplot(ptable, aes_string(x=parameter)) + 
                        geom_histogram(aes(fill = fill)) + 
                        scale_fill_manual(values =plasma(2, end = 0.65), guide = FALSE) +
                        ggtitle(title) 

        if(facet){ 
            if(free_y){
                trace.plot = trace.plot + facet_wrap(~chain, ncol=1, scales = "free_y") + theme(legend.position="none")
                density.plot = density.plot + facet_wrap(~chain, ncol=1, scales = "free_y") + theme(legend.position="none")
            }else{
                trace.plot = trace.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")
                density.plot = density.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")
            }
        }



        return(list(trace.plot = trace.plot, density.plot = density.plot))

    }else{
        stop(sprintf("The variable '%s' is not a column in the table of parameters you have supplied", parameter))
    }

}