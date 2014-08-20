#' Function for running rwty analyses on single chains.
#' 
#' This function is automatically called when analyze.rwty is called with one chain.
#' It runs analyze.single on the chain and returns an object with the appropriate tables
#' and plots.
#'
#' @param chains A single rwty.trees object. \code{chains}
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#' @param window.size The length of window (in trees) for the sliding window plot \code{window.size}
#' @param gens.per.tree The number of generations per tree in the .t file.
#' @param step The number of trees to skip for each step in the treespace plot.  Step=5 would amount to reading every fifth tree.
#
#' @return output A list with tables and plots for LnL (when .p file exists), sliding window,
#' cumulative, and treespace plots.
#'
#' @keywords MCMC, phylogenetics, convergence, plot, awty, rwty
#'
#' @export
#' 
#' @examples
#' analyze.single(mytrees, burnin=100, window.size=100, gens.per.tree=1000, step=5)

analyze.single <- function(chains, burnin, window.size, gens.per.tree=NA, step=1, filename = NA, labels=NA, ...){
    
    if(is.na(gens.per.tree)){gens.per.tree = chains$gens.per.tree}
    lnl.plot <- NA
    if(exists("chains$ptable")){
        print("Making LnL plot...")
        lnl.plot <- ggplot(chains$ptable[burnin:length(chains$ptable[,1]),], aes(x=Gen, y=LnL)) + geom_line()
    }
    
    print("Sliding window analysis...")
    slide.data <- slide.freq(chains, burnin, window.size, gens.per.tree)
    slide.plot <- plot.cladeprobs(slide.data$slide.table, ...)
    
    print("Cumulative analysis...")
    cumulative.data <- cumulative.freq(chains, burnin, window.size, gens.per.tree,
                                       slide.freq.table=slide.data)
    cumulative.plot <- plot.cladeprobs(cumulative.data$cumulative.table, ...)
    
    print("Plotting trees in tree space...")
    print(step)
    mdstrees <- chains$trees[seq((burnin + 1), length(chains$trees), by = step)]
    treespace <- treespace.single(mdstrees)
    treespace.data <- treespace$mds
    treespace.plot <- treespace$plot
    
    if(!is.na(labels)){
        if(!is.na(lnl.plot)){
            lnl.plot <- lnl.plot + ggtitle(labels)
        }
        slide.plot <- slide.plot + ggtitle(labels)
        cumulative.plot <- cumulative.plot + ggtitle(labels)
        treespace.plot <- treespace.plot + ggtitle(labels)
    }
    
    if(!is.na(filename)){
        pdf(file=filename)
        print(lnl.plot)
        print(slide.plot)
        print(cumulative.plot)
        print(treespace.plot)
        dev.off()
    }
    
    output <- list("LnL.plot" = lnl.plot, "slide.data" = slide.data,
                   "slide.plot" = slide.plot, "cumulative.data" = cumulative.data,
                   "cumulative.plot" = cumulative.plot, "treespace.data" = treespace.data,
                   "treespace.plot" = treespace.plot)
}