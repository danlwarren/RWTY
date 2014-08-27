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

analyze.single <- function(chains, burnin=0, window.size, gens.per.tree=NA, treespace.points=100, filename = NA, labels=NA, treespace=TRUE, ...){
    step = as.integer((length(chains$trees) - burnin)/treespace.points)
    if(is.na(gens.per.tree)){gens.per.tree = chains$gens.per.tree}
    lnl.plot <- NA
    if(!is.null(chains$ptable)){
        print("Making LnL plot...")
        if(nrow(chains$ptable) > 1000){ #Keeping the number of points plotted reasonable
            temp.ptable <- chains$ptable[burnin:length(chains$ptable[,1]),]
            temp.ptable <- temp.ptable[seq(from=1, to=length(temp.ptable[,1]), length.out=1000),]
            lnl.plot <- ggplot(temp.ptable, aes(x=Gen, y=LnL)) + geom_line() + ggtitle("Likelihood")
        }
        else{
            lnl.plot <- ggplot(chains$ptable[burnin:length(chains$ptable[,1]),], aes(x=Gen, y=LnL)) + geom_line() + ggtitle("Likelihood")
        }
    }
    
    print("Sliding window analysis...")
    slide.data <- slide.freq(chains, burnin, window.size, gens.per.tree)
    slide.plot <- plot.cladeprobs(slide.data$slide.table, ...) + ggtitle("Sliding Window Posterior Probability")
    slide.variance.plot <- plot.cladevar(slide.data$slide.table) + ggtitle("Sliding Window Variance")
    
    print("Cumulative analysis...")
    cumulative.data <- cumulative.freq(chains, burnin, window.size, gens.per.tree,
                                       slide.freq.table=slide.data)
    cumulative.plot <- plot.cladeprobs(cumulative.data$cumulative.table, ...) + ggtitle("Cumulative Posterior Probability")
    cumulative.variance.plot <- plot.cladevar(cumulative.data$cumulative.table) + ggtitle("Cumulative Variance")
    
    
    #print(step)
    treespace.data <- NA
    treespace.plot <- NA
    if(treespace==TRUE){
        print("Plotting trees in tree space...")
        mdstrees <- chains$trees[seq((burnin + 1), length(chains$trees), by = step)]
        mdsptable <- NULL
        if(!is.null(chains$ptable)){
            mdsptable <- chains$ptable[seq((burnin + 1), length(chains$trees), by = step),]
        }
        # The following massive ball of shit is intended to get a list of numeric values for the
        # generation represented by each tree
        gens <- as.numeric(unlist(regmatches(names(chains$trees), gregexpr('\\(?[0-9]+', names(chains$trees)))))
        gens <- gens[seq((burnin + 1), length(chains$trees), by = step)]
        treespace <- treespace.single(mdstrees, gens, mdsptable)
        treespace.data <- treespace$mds
        treespace.plot <- treespace$plot + ggtitle("Tree Space")
    }
    
    if(!is.na(labels)){
        if(!is.na(lnl.plot[1])){
            lnl.plot <- lnl.plot + ggtitle(paste(labels, "Likelihood"))
        }
        slide.plot <- slide.plot + ggtitle(paste(labels, "Sliding Window Posterior Probability"))
        slide.variance.plot <- slide.variance.plot + ggtitle(paste(labels, "Sliding Window Variance"))
        cumulative.plot <- cumulative.plot + ggtitle(paste(labels, "Cumulative Posterior Probability"))
        cumulative.variance.plot <- cumulative.variance.plot + ggtitle(paste(labels, "Cumulative Variance"))
        treespace.plot <- treespace.plot + ggtitle(paste(labels, "Tree Space"))
    }
    
    if(!is.na(filename)){
        pdf(file=filename)
        print(lnl.plot)
        print(slide.plot)
        print(slide.variance.plot)
        print(cumulative.plot)
        print(cumulative.variance.plot)
        print(treespace.plot)
        dev.off()
    }
    
    output <- list("LnL.plot" = lnl.plot, "slide.data" = slide.data,
                   "slide.plot" = slide.plot, "slide.variance.plot" = slide.variance.plot,
                    "cumulative.data" = cumulative.data,"cumulative.plot" = cumulative.plot, 
                   "cumulative.variance.plot" = cumulative.variance.plot, "treespace.data" = treespace.data,
                   "treespace.plot" = treespace.plot)
}