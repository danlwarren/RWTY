#' Plot split frequencies in sliding windows over the course of an MCMC
#' 
#' Takes a list of rwty.chain objects.  
#' Plots the split frequencies of clades over the course of the MCMC, calculated from windows of a specified size.
#' Only plots the n.clades most variable clades, as measured by the standard deviation of the split frequencies of each clade across the MCMC.  
#' Each line in the plot represents a single clade. The colour of the line represents the standard deviation of the split frequencies of that clade across the MCMC.
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin 
#' @param n.clades The number of clades to plot 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#' @param facet (TRUE/FALSE). TRUE: return a single plot with one facet per chain; FALSE: return a list of individual plots with one plot per chain 
#' @param rank ('ess', 'sd'). How to rank the clades? By default, we plot the 20 'worst' clades. This parameter sets the definition of 'worst'. The default is to rank the clades by increasing Effective Sample Size (i.e. the 20 worst clades are those with the lowest ESS), since in a sliding window plot we expect well-sampled splits to have a high value (rank = "ess").  The original AWTY ranked clades by their standard deviations. To do this, just set rank = 'sd'.
#'
#' @return splitfreqs.plot Either a single ggplot2 object or a list of ggplot2 objects.
#'
#' @keywords sliding window, mcmc, phylogenetics, plot
#'
#' @export makeplot.splitfreqs.sliding
#' @examples
#' data(fungus)
#' makeplot.splitfreqs.sliding(fungus, burnin = 20, n.clades=25)

makeplot.splitfreqs.sliding <- function(chains, burnin = 0, n.clades=20, window.size = 20, facet = TRUE, rank = 'ess'){ 

    print(sprintf("Creating sliding window split frequency plot for %d clades", n.clades))

    chains = check.chains(chains)
    slide.freq.list = slide.freq(chains, burnin = burnin, window.size = window.size)
    dat.list = lapply(slide.freq.list, process.freq.table, n.clades = n.clades, rank = rank)
    dat = do.call("rbind", dat.list)
    dat$Chain = get.dat.list.chain.names(dat.list)
    rownames(dat) = NULL
    title = sprintf("Sliding Window Split Frequencies for %d clades", n.clades)
    if(rank == 'ess') RANK = "ESS"
    if(rank == 'sd') RANK = "StDev"

    if(facet==TRUE){
        splitfreqs.plot <- ggplot(data=dat, aes(x=as.numeric(as.character(Generations)), y=Split.Frequency, group = Clade)) +
            facet_wrap(~Chain, ncol = 1) +
            geom_line(aes_string(colour = RANK)) + 
            scale_color_viridis(option = "C", end = 0.85) +
            xlab("Generation") +
            ylab("Split frequency") +
            ggtitle(title)

        splitfreqs.plot = list("splitfreqs.sliding.plot" = splitfreqs.plot)

    }else{
        dat.list = split(dat, f = dat$Chain)
        splitfreqs.plot = lapply(dat.list, single.splitfreq.plot, rank = RANK)
        for(i in 1:length(splitfreqs.plot)){
            splitfreqs.plot[[i]] = splitfreqs.plot[[i]] + ggtitle(paste(title, "from", names(splitfreqs.plot)[i]))
            names(splitfreqs.plot)[i] = paste("splitfreqs.sliding.plot.", names(splitfreqs.plot[i]), sep="")
        }
    }


    return(splitfreqs.plot)
}

get.dat.list.chain.names <- function(dat.list){

    names = c()

    for(i in 1:length(dat.list)){
        name = names(dat.list)[[i]]
        N = nrow(dat.list[[i]])
        names = c(names, rep(name, N))

    }

    return(names)
}


single.splitfreq.plot <- function(dat, rank){

    splitfreq.plot <- ggplot(data=dat, aes(x=as.numeric(as.character(Generations)), y=Split.Frequency, group = Clade)) + 
        geom_line(aes_string(colour = rank)) +
        scale_color_viridis(option = "C", end = 0.85) +
        xlab("Generation") +
        ylab("Split frequency")

    return(splitfreq.plot)

}


process.freq.table <- function(freq.table, n.clades, rank){

    # strip out just the parts of a slide.freq.table or a cumulative.freq.table that we need
    if(class(freq.table) == "rwty.slide"){
        dat = freq.table$slide.table
    }else if(class(freq.table) == "rwty.cumulative"){
        dat = freq.table$cumulative.table
    }else{
        stop("ERROR: unknown type of frequency table passed to process.freq.table()")
    }

    if(rank == 'sd'){ 
        dat = dat[order(dat$sd, decreasing=TRUE),]
    }else if(rank == 'ess'){
        dat = dat[order(dat$ess, decreasing=FALSE),]   
        dat = subset(dat, ess>0)     
    }else if(rank == 'wcsf'){
        dat = dat[order(dat$wcsf, decreasing=TRUE),]   
    }

    # take the top 20 worst clades
    dat = dat[1:n.clades,]

    dat = dat[,!(names(dat) %in% c("mean"))] #Stripping off mean
    dat$clade = rownames(dat)


    if(class(freq.table) == "rwty.slide"){
        dat = melt(dat, id.vars=c("clade", "sd", "ess"))
        colnames(dat) = c("Clade", "StDev", "ESS", "Generations", "Split.Frequency")
    }else if(class(freq.table) == "rwty.cumulative"){
        dat = melt(dat, id.vars=c("clade", "sd", "wcsf"))
        colnames(dat) = c("Clade", "StDev", "WCSF", "Generations", "Split.Frequency")
    }


    dat$Clade = as.factor(dat$Clade)
    dat$id = rep(1:length(unique(dat$Clade)), length.out = nrow(dat))

    return(dat)
}