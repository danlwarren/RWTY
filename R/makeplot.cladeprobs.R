#' Plot clade probabilities in sliding windows over the course of an MCMC
#' 
#' Takes a list of rwty.trees objects.  
#' Plots the posterior probabilities of clades over the course of the MCMC, calculated from windows of a specified size.
#' Only plots the n.clades most variable clades, as measured by the standard deviation of the posterior probabilities of each clade across all windows.  
#' Each line in the plot represents a single clade. The colour of the line represents the standard deviation of the posterior probabilities of that clade across all sliding windows.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param n.clades The number of clades to plot 
#' @param window.size The number of trees per window (default 20) 
#' @param facet (TRUE/FALSE). TRUE: return a single plot with one facet per chain; FALSE: return a list of individual plots with one plot per chain 
#'
#' @return cladeprobs.plot Either a single ggplot2 object or a list of ggplot2 objects.
#'
#' @keywords sliding window, mcmc, phylogenetics, plot
#'
#' @export makeplot.cladeprobs.sliding
#' @examples
#' data(fungus)
#' makeplot.cladeprobs.sliding(fungus, burnin = 20, n.clades=25)

makeplot.cladeprobs.sliding <- function(chains, burnin = 0, n.clades=20, window.size = 20, facet = TRUE){ 


    chains = check.chains(chains)
    slide.freq.list = slide.freq(chains, burnin = burnin, window.size = window.size)
    dat.list = lapply(slide.freq.list, process.slide.freq.table, n.clades = n.clades)
    dat = do.call("rbind", dat.list)
    dat$Chain = rep(names(dat.list), each = nrow(dat.list[[1]]))
    rownames(dat) = NULL
    
    if(facet==TRUE){
        cladeprobs.plot <- ggplot(data=dat, aes(x=as.numeric(as.character(Generations)), y=Posterior.Probability, group = Clade, color = StDev)) +
            facet_wrap(~Chain, ncol = 1) +
            geom_line() +
            xlab("Generations")
    }else{
        dat.list = split(dat, f = dat$Chain)
        cladeprobs.plot = lapply(dat.list, single.plot)
    }

    return(cladeprobs.plot)
}


single.plot <- function(dat){

    cladeprob.plot <- ggplot(data=dat, aes(x=as.numeric(as.character(Generations)), y=Posterior.Probability, group = Clade, color = StDev)) + 
        geom_line() +
        xlab("Generations")    

    return(cladeprob.plot)

}


process.slide.freq.table <- function(slide.freq.table, n.clades){

    # strip out just the parts of a slide.freq.table that we need

    dat = slide.freq.table$slide.table
    dat = dat[1:n.clades,2:length(dat) - 1] #Stripping off mean
    dat$clade = rownames(dat)
    dat = melt(dat, id.vars=c("clade", "sd"))
    colnames(dat) = c("Clade", "StDev", "Generations", "Posterior.Probability")
    dat$Clade = as.factor(dat$Clade)
    dat$id = rep(1:length(unique(dat$Clade)), length.out = nrow(dat))

    return(dat)
}