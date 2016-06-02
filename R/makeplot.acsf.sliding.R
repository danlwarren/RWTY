#' Plot the Chaing in Split Frequencies (CSF) in sliding windows over the course of an MCMC.
#' 
#' This function takes one or more rwty.trees ojects and returns a plot of CSF within each chain as the MCMC progresses.  
#' The solid line with points shows the Average Change in Split Frequencies (ACSF) between this window and the previous window
#' The grey ribbon shows the upper and lower 95% quantiles of the CSFs between this window and the previuos window
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin. Defaults to zero. 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#' @param facet (TRUE/FALSE). TRUE: return a single plot with one facet per chain; FALSE: return a list of individual plots with one plot per chain 
#' 
#' @return output A plof of the CSF between sliding windows over all chains
#'
#' @return acsf.plot A ggplot object, or list of ggplot objects
#'
#' @keywords mcmc, phylogenetics, convergence, uncertainty
#'
#' @export makeplot.acsf.sliding
#' @examples
#' data(fungus)
#' makeplot.acsf.sliding(fungus, burnin=20)

makeplot.acsf.sliding <- function(chains, burnin = 0, window.size = 20, facet = TRUE){ 
    # plot variation in clade frequencies between windows
    
    print(sprintf("Creating sliding window ACSF plot"))

    chains = check.chains(chains)
    slide.freq.list = slide.freq(chains, burnin = burnin, window.size = window.size)
    dat.list = lapply(slide.freq.list, get.acsf)
    dat = do.call("rbind", dat.list)
    dat$Chain = get.dat.list.chain.names(dat.list)
    rownames(dat) = NULL
    title = "Sliding window Change in Split Frequencies"

    if(facet==TRUE){
        acsf.plot <- ggplot(dat, aes(x = as.numeric(as.character(Generation)))) + 
                    geom_ribbon(aes(ymin = min, ymax = lower.95), alpha = 0.15) + 
                    geom_ribbon(aes(ymin = lower.95, ymax = upper.95), alpha = 0.30) + 
                    geom_ribbon(aes(ymin = upper.95, ymax = max), alpha = 0.15) + 
                    geom_line(aes(y = ACSF, colour = Chain)) + 
                    geom_point(aes(y = ACSF, colour = Chain)) +
                    theme(legend.position="none") +                
                    expand_limits(y = 0) +
                    xlab("Generation") +
                    ylab("Change in Split Frequency") + 
                    facet_wrap(~Chain, ncol = 1) +
                    ggtitle(title)
        acsf.plot <- list("acsf.sliding.plot" = acsf.plot)
    }else{
        dat.list = split(dat, f = dat$Chain)
        acsf.plot = lapply(dat.list, single.acsf.plot, type = 'sliding')
        for(i in 1:length(acsf.plot)){
            acsf.plot[[i]] = acsf.plot[[i]] + ggtitle(paste(title, "for", names(acsf.plot)[i]))
            names(acsf.plot)[i] = paste("acsf.sliding.plot.", names(acsf.plot[i]), sep="")
        }

    }

    return(acsf.plot)

}

single.acsf.plot <- function(dat, type){
    acsf.plot <- ggplot(dat, aes(x = as.numeric(as.character(Generation)))) + 
                    geom_ribbon(aes(ymin = min, ymax = lower.95), alpha = 0.15) + 
                    geom_ribbon(aes(ymin = lower.95, ymax = upper.95), alpha = 0.30) + 
                    geom_ribbon(aes(ymin = upper.95, ymax = max), alpha = 0.15) + 
                    geom_line(aes(y = ACSF, colour = Chain)) + 
                    geom_point(aes(y = ACSF, colour = Chain)) +
                    theme(legend.position="none") +                
                    xlab("Generation") +
                    ylab("Change in Split Frequency") + 
                    facet_wrap(~Chain, ncol = 1) +
                    ggtitle(title)

    if(type == 'sliding'){ acsf.plot = acsf.plot + expand_limits(y = 0)}
    if(type == 'cumulative'){ acsf.plot = acsf.plot + coord_cartesian(ylim=c(0,max(dat$ACSF))) }

    return(acsf.plot)
}


