#' Plotting parameters
#' 
#' Plots parameter values over the length of the MCMC chain
#'
#' @param chains A set of rwty.chain objects.
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value either when loading the chains or to this function.  
#' 
#' @return param.plot Returns a ggplot object.
#'
#' @keywords parameter, plot, convergence, mcmc, phylogenetics
#'
#' @export makeplot.burnin
#' @examples
#' \dontrun{
#' data(fungus)
#' makeplot.burnin(fungus)
#' }

makeplot.burnin <- function(chains, burnin = NA){ 
    
    print("Creating burnin plot")
    
    
    chains = check.chains(chains)
    ptable = combine.ptables(chains, burnin = 0)
    
    # Since this may be called from analyze.rwty with no likelihood column
    # we should just stop here and give up
    if(!"LnL" %in% colnames(ptable)){
        return(NA)
    }
    
    vlines <- data.frame(burn = unlist(lapply(chains, function(x) x[['burnin']])),
                         chain = names(chains))
    
    gens.per.tree <- data.frame(gens = unlist(lapply(chains, function(x) x[['gens.per.tree']])),
                         chain = names(chains))
    
    vlines$burn = vlines$burn * gens.per.tree$gens
    
    # Use the automatic burnin if one isn't supplied
    if(is.na(burnin)){
        burnin = max(vlines$burn)
    }
       
    burnin.plot <- ggplot(ptable, aes_string(x="generation", y="topo.dist.mcc")) + 
        geom_line(aes_string(colour = "chain")) + 
        geom_vline(xintercept = burnin, linetype = "dashed", alpha = 0.5, size = 3, color = "red") +
        geom_vline(data = vlines, aes(xintercept = burn, color = chain), linetype = "longdash") +
        xlab("Generation") + 
        scale_color_viridis(discrete = TRUE, end = 0.85) + 
        facet_wrap(~chain, ncol=1, scales = "free_y") + 
        theme(legend.position="none")
    
    return(list(burnin.plot = burnin.plot))
    
}
