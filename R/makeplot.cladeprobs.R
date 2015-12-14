#' New style plotting of cumulative and slide objects
#' 
#' Takes a table from cumulative.freq or slide.freq as input.  
#' Numclades gives the number of clades to plot, starting from the
#' top.  Since cumulative.freq and slide.freq both sort by sd these
#' will by default be the most variable clades.
#'
#' @param input.table An rwty.slide or rwty.cumulative object 
#' @param numclades The number of clades to plot.  The clades with the highest sd in clade frequency are plotted first, so numclades = 10 will be the 10 most variable clades in the chain. 
#'
#' @return thisplot Returns a ggplot object.
#'
#' @keywords cumulative, sliding window, mcmc, phylogenetics, plot
#'
#' @export makeplot.cladeprobs
#' @examples
#' data(fungus)
#' makeplot.cladeprobs(fungus, burnin = 20, numclades=25)

makeplot.cladeprobs <- function(chains, burnin = 0, n.clades=20, window.size = 20, facet = TRUE){ 


    chains = check.chains(chains)
    slide.freq.list = slide.freq(chains, burnin = burnin, window.size = window.size)
    dat.list = lapply(slide.freq.list, process.slide.freq.table, n.clades = n.clades)
    dat = do.call("rbind", dat.list)
    dat$Chain = rep(names(dat.list), each = nrow(dat.list[[1]]))
    rownames(dat) = NULL
    
    if(facet==TRUE){
        cladeprob.plot <- ggplot(data=dat, aes(x=as.numeric(as.character(Generations)), y=Posterior.Probability, group = Clade, color = StDev)) + 
            facet_wrap(~Chain, ncol = 1) +
            geom_line() +
            xlab("Generations")    
    }else{
        dat.list = split(dat, f = dat$Chain)
        cladeprob.plot = lapply(dat.list, single.plot)
    }

    return(cladeprob.plot)
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