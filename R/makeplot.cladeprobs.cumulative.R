#' Plot cumulative split frequencies over the course of an MCMC
#' 
#' Takes a list of rwty.trees objects.  
#' Plots the cumulative split frequencies of clades over the course of the MCMC. Stationarity is indicated by split frequencies levelling out.
#' Only plots the n.clades most variable clades, as measured by the standard deviation of the split frequencies of each clade across all windows.  
#' Each line in the plot represents a single clade. The colour of the line represents the standard deviation of the split frequencies of that clade across all sliding windows.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param n.clades The number of clades to plot 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#' @param facet (TRUE/FALSE). TRUE: return a single plot with one facet per chain; FALSE: return a list of individual plots with one plot per chain 
#'
#' @return splitfreqs.plot Either a single ggplot2 object or a list of ggplot2 objects.
#'
#' @keywords sliding window, mcmc, phylogenetics, plot
#'
#' @export makeplot.splitfreqs.cumulative
#' @examples
#' data(fungus)
#' makeplot.splitfreqs.cumulative(fungus, burnin = 20, n.clades=25)

makeplot.splitfreqs.cumulative <- function(chains, burnin = 0, n.clades=20, window.size = 20, facet = TRUE){ 

    print(sprintf("Creating cumulative split frequency plot for %d clades", n.clades))

    chains = check.chains(chains)
    cumulative.freq.list = cumulative.freq(chains, burnin = burnin, window.size = window.size)
    dat.list = lapply(cumulative.freq.list, process.freq.table, n.clades = n.clades)
    dat = do.call("rbind", dat.list)
    dat$Chain = get.dat.list.chain.names(dat.list)
    rownames(dat) = NULL
    title = sprintf("Cumulative Split Frequencies for %d clades", n.clades)

    if(facet==TRUE){
        splitfreqs.plot <- ggplot(data=dat, aes(x=as.numeric(as.character(Generations)), y=Posterior.Probability, group = Clade, color = StDev)) +
            facet_wrap(~Chain, ncol = 1) +
            geom_line() +
            xlab("Generation") +
            ylab("Split Frequency") +
            ggtitle(title)
        splitfreqs.plot = list("splitfreqs.cumulative.plot" = splitfreqs.plot)

    }else{
        dat.list = split(dat, f = dat$Chain)
        splitfreqs.plot = lapply(dat.list, single.splitfreq.plot)
        for(i in 1:length(splitfreqs.plot)){
            splitfreqs.plot[[i]] = splitfreqs.plot[[i]] + ggtitle(paste(title, "from", names(splitfreqs.plot)[i]))
            names(splitfreqs.plot)[i] = paste("splitfreqs.cumulative.plot.", names(splitfreqs.plot[i]), sep="")
        }
    }


    return(splitfreqs.plot)
}

