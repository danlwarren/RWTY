#' Plot cumulative clade probabilities over the course of an MCMC
#' 
#' Takes a list of rwty.trees objects.  
#' Plots the cumulative posterior probabilities of clades over the course of the MCMC. Stationarity is indicated by clade probabilities levelling out.
#' Only plots the n.clades most variable clades, as measured by the standard deviation of the posterior probabilities of each clade across all windows.  
#' Each line in the plot represents a single clade. The colour of the line represents the standard deviation of the posterior probabilities of that clade across all sliding windows.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param n.clades The number of clades to plot 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#' @param facet (TRUE/FALSE). TRUE: return a single plot with one facet per chain; FALSE: return a list of individual plots with one plot per chain 
#'
#' @return cladeprobs.plot Either a single ggplot2 object or a list of ggplot2 objects.
#'
#' @keywords sliding window, mcmc, phylogenetics, plot
#'
#' @export makeplot.cladeprobs.cumulative
#' @examples
#' data(fungus)
#' makeplot.cladeprobs.cumulative(fungus, burnin = 20, n.clades=25)

makeplot.cladeprobs.cumulative <- function(chains, burnin = 0, n.clades=20, window.size = 20, facet = TRUE){ 

    print(sprintf("Creating cumulative split frequency plot for %d clades", n.clades))

    chains = check.chains(chains)
    cumulative.freq.list = cumulative.freq(chains, burnin = burnin, window.size = window.size)
    dat.list = lapply(cumulative.freq.list, process.freq.table, n.clades = n.clades)
    dat = do.call("rbind", dat.list)
    dat$Chain = get.dat.list.chain.names(dat.list)
    rownames(dat) = NULL
    title = sprintf("Cumulative Split Frequencies for %d clades", n.clades)

    if(facet==TRUE){
        cladeprobs.plot <- ggplot(data=dat, aes(x=as.numeric(as.character(Generations)), y=Posterior.Probability, group = Clade, color = StDev)) +
            facet_wrap(~Chain, ncol = 1) +
            geom_line() +
            xlab("Generation") +
            ylab("Split Frequency") +
            ggtitle(title)
        cladeprobs.plot = list("cladeprobs.cumulative.plot" = cladeprobs.plot)

    }else{
        dat.list = split(dat, f = dat$Chain)
        cladeprobs.plot = lapply(dat.list, single.cladeprob.plot)
        for(i in 1:length(cladeprobs.plot)){
            cladeprobs.plot[[i]] = cladeprobs.plot[[i]] + ggtitle(paste(title, "from", names(cladeprobs.plot)[i]))
            names(cladeprobs.plot)[i] = paste("cladeprobs.cumulative.plot.", names(cladeprobs.plot[i]), sep="")
        }
    }


    return(cladeprobs.plot)
}

