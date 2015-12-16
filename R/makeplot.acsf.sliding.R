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
    
    chains = check.chains(chains)
    slide.freq.list = slide.freq(chains, burnin = burnin, window.size = window.size)
    dat.list = lapply(slide.freq.list, get.acsf)
    dat = do.call("rbind", dat.list)
    dat$Chain = get.dat.list.chain.names(dat.list)
    rownames(dat) = NULL

    if(facet==TRUE){
        acsf.plot <- ggplot(dat, aes(x = as.numeric(as.character(Generation)))) + 
                    geom_ribbon(aes(ymin = lower.95, ymax = upper.95), alpha = 0.25) + 
                    geom_line(aes(y = ACSF, colour = Chain)) + 
                    geom_point(aes(y = ACSF, colour = Chain)) +
                    theme(legend.position="none") +                
                    expand_limits(y=0) +
                    xlab("Generation") + 
                    ylab("Change in Split Frequencies") + 
                    facet_wrap(~Chain, ncol = 1)
    }else{
        dat.list = split(dat, f = dat$Chain)
        acsf.plot = lapply(dat.list, single.acsf.plot)
    }

    return(acsf.plot)

}

single.acsf.plot <- function(dat){
    acsf.plot <- ggplot(dat, aes(x = as.numeric(as.character(Generation)))) + 
                geom_ribbon(aes(ymin = lower.95, ymax = upper.95), alpha = 0.25) + 
                geom_line(aes(y = ACSF)) + 
                geom_point(aes(y = ACSF)) +
                theme(legend.position="none") +                
                expand_limits(y=0) +
                xlab("Generation") + 
                ylab("Change in Split Frequencies")
}


get.acsf <- function(freq.table){

    # get a df of absolute differences in pp variation between windows
    if(class(freq.table) == "rwty.slide"){
        dat = freq.table$slide.table
    }else if(class(freq.table) == "rwty.cumulative"){
        dat = freq.table$cumulative.table
    }else{
        stop("ERROR: unknown type of frequency table passed to process.freq.table()")
    }

    dat = dat[,!(names(dat) %in% c("mean", "sd"))] #Remove mean and sd

    d <- t(apply(dat, 1, function(z) abs(diff(as.numeric(z)))))
    d <- as.data.frame(d)
    colnames(d) <- colnames(dat)[2:ncol(dat)] # differences of previous window
    d$clade <- rownames(d)

    d <- melt(d, id.vars="clade")
    colnames(d) <- c("Clade", "Generation", "CSF")
    d$Clade <- as.factor(d$Clade)

    dat = ddply(d, .(Generation), summarize, 
                ACSF = mean(CSF), 
                upper.95 = quantile(CSF, c(0.975)), 
                lower.95 = quantile(CSF, c(0.025)), 
                min = min(CSF), 
                max = max(CSF))

    return(dat)
}