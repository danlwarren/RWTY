#' Plot cumulative split frequencies over the course of an MCMC
#' 
#' Takes a list of rwty.chain objects.  
#' Plots the cumulative split frequencies of clades over the course of the MCMC. Stationarity is indicated by split frequencies levelling out.
#' Only plots the n.clades most variable clades, as measured by the standard deviation of the split frequencies of each clade across all windows.  
#' Each line in the plot represents a single clade. The colour of the line represents the standard deviation of the split frequencies of that clade across all sliding windows.
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param n.clades The number of clades to plot 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#' @param facet (TRUE/FALSE). TRUE: return a single plot with one facet per chain; FALSE: return a list of individual plots with one plot per chain 
#' @param rank ('wcsf', 'sd'). How to rank the clades? By default, we plot the 20 'worst' clades. This parameter sets the definition of 'worst'. The default is to rank the by the weighted change in split frequencies (rank = 'wcsf'). This works by looking at the change in the cumulative split frequency over the course of the MCMC, and ranks the worst chains as those that do not level off (i.e. those that have changes near the end). We do this because in a well-behaved chain, we expect the cumulative split frequencies to level off once the chain has been run for long enough. So, any cumulative split frequencies which are still changing towards the end of your run are likely to indicate problematic clades. Specifically, we multiply the absolute change in split frequencies for each clade by a set of weights that increase linearly towards the end of the chain (the first observation gets a weight of zero, the final observation gets a weight of one). The original AWTY ranked clades by their standard deviations (higher SD = worse), so we include this as an option too. To do this, just set rank = 'sd'.
#'
#' @return splitfreqs.plot Either a single ggplot2 object or a list of ggplot2 objects.
#'
#' @keywords mcmc phylogenetics plot
#'
#' @export makeplot.splitfreqs.cumulative
#' @examples
#' \dontrun{
#' data(fungus)
#' makeplot.splitfreqs.cumulative(fungus, burnin = 20, n.clades=25)
#' }

makeplot.splitfreqs.cumulative <- function(chains, burnin = 0, n.clades=20, window.size = 20, facet = TRUE, rank = 'wcsf'){ 

    print(sprintf("Creating cumulative split frequency plot for %d clades", n.clades))
    if(rank == 'wcsf'){ 
        RANK = "WCSF"
    }else if(rank == 'sd'){ 
        RANK = "StDev"
    }else{
        stop("'rank' must be either 'sd' or 'wcsf'")
    }

    chains = check.chains(chains)
    cumulative.freq.list = cumulative.freq(chains, burnin = burnin, window.size = window.size)
    dat.list = lapply(cumulative.freq.list, process.freq.table, n.clades = n.clades, rank = rank)
    dat = do.call("rbind", dat.list)
    dat$Chain = get.dat.list.chain.names(dat.list)
    rownames(dat) = NULL
    title = sprintf("Cumulative Split Frequencies for %d clades", n.clades)


    if(facet==TRUE){
        splitfreqs.plot <- ggplot(data=dat, aes(x=as.numeric(as.character(Generations)), y=Split.Frequency, group = Clade)) +
            facet_wrap(~Chain, ncol = 1) +
            geom_line(aes_string(colour = RANK)) +
            scale_color_viridis(option = "C", end = 0.85) +
            xlab("Generation") +
            ylab("Split Frequency") +
            ggtitle(title)
        splitfreqs.plot = list("splitfreqs.cumulative.plot" = splitfreqs.plot)

    }else{
        dat.list = split(dat, f = dat$Chain)
        splitfreqs.plot = lapply(dat.list, single.splitfreq.plot, rank = RANK)
        for(i in 1:length(splitfreqs.plot)){
            splitfreqs.plot[[i]] = splitfreqs.plot[[i]] + ggtitle(paste(title, "from", names(splitfreqs.plot)[i]))
            names(splitfreqs.plot)[i] = paste("splitfreqs.cumulative.plot.", names(splitfreqs.plot[i]), sep="")
        }
    }


    return(splitfreqs.plot)
}


