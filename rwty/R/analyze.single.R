#' Function for running rwty analyses on single chains.
#' 
#' This function is automatically called when analyze.rwty is called with one chain.
#' It runs analyze.single on the chain and returns an object with the appropriate tables
#' and plots.
#'
#' @param chains A single rwty.trees object. \code{chains}
#' @param burnin The number of trees to eliminate as burnin. Default is zero. \code{burnin}
#' @param window.size The length of window (in trees) for the sliding window plot.  Default is 20.   \code{window.size}
#' @param gens.per.tree The number of generations per tree in the .t file. If no value is provided, RWTY will try to figure it out from the tree names. \code{gens.per.tree}
#' @param treespace.points The number of trees to plot in the treespace plot. Default is 100. \code{treespace.points}
#' @param filename A name to be used for generating pdfs of output.  \code{filename}
#' @param labels The name to use on plots and in generating output files.  \code{labels}
#' @param treespace Boolean to determine whether or not treespace plots are made. \code{treespace}
#
#' @return output A list with tables and plots for LnL (when .p file exists), sliding window,
#' cumulative, and treespace plots.
#'
#' @keywords MCMC, phylogenetics, convergence, plot, awty, rwty
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' analyze.single(run1, burnin=100, window.size=20, treespace.points=100, filename="fungus.pdf", labels="Chain1")

analyze.single <- function(chains, burnin=0, window.size, gens.per.tree=NA, chain.name, ...){
    
    print("Sliding window analysis...")
    slide.data <- slide.freq(chains, burnin, window.size, gens.per.tree)
    slide.plot <- plot.cladeprobs(slide.data$slide.table, ...) + ggtitle("Sliding Window Posterior Probability")
    slide.variance.plot <- plot.cladevar(slide.data$slide.table) + ggtitle("Sliding Window Variance")
    
    print("Cumulative analysis...")
    cumulative.data <- cumulative.freq(chains, burnin, window.size, gens.per.tree,
                                       slide.freq.table=slide.data)
    cumulative.plot <- plot.cladeprobs(cumulative.data$cumulative.table, ...) + ggtitle("Cumulative Posterior Probability")
    cumulative.variance.plot <- plot.cladevar(cumulative.data$cumulative.table) + ggtitle("Cumulative Variance")
    
    slide.plot <- slide.plot + ggtitle(paste(chain.name, "Sliding Window Posterior Probability"))
    slide.variance.plot <- slide.variance.plot + ggtitle(paste(chain.name, "Sliding Window Variance"))
    cumulative.plot <- cumulative.plot + ggtitle(paste(chain.name, "Cumulative Posterior Probability"))
    cumulative.variance.plot <- cumulative.variance.plot + ggtitle(paste(chain.name, "Cumulative Variance"))

    output <- list("slide.data" = slide.data, "slide.plot" = slide.plot, "slide.variance.plot" = slide.variance.plot,
                    "cumulative.data" = cumulative.data,"cumulative.plot" = cumulative.plot, "cumulative.variance.plot" = cumulative.variance.plot) 
}