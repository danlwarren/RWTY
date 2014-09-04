#' Analyze.rwty, the main interface for rwty analyses and plots.
#' 
#' This is the main user interface to rwty.  It allows users to chuck in arguments for
#' chains, burnin, window size, gens per tree, and "step", and returns an object that
#' contains sliding window and cumulative posterior probability plots, treespace plots,
#' and multi-chain diagnostic plots when multiple chains are provided.
#'
#' @param chains A list of rwty.trees objects. \code{chains}
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#' @param window.size The length of window (in trees) for the sliding window plot \code{window.size}
#' @param gens.per.tree The number of generations per tree in the .t file.
#' @param treespace.points The number of trees to plot in the treespace plot.  
#'
#' @return output A list of outputs from the analyze.single runs on each chain, as well as a compare.n run for all chains.  Eventually we will add more multi-chain analyses.
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' single <- analyze.rwty(run1, burnin=100, window.size=20, treespace.points=50, filename="Run1.pdf")
#' multi <- analyze.rwty(list(run1, run2), burnin=100, window.size=20, treespace.points=50, labels=c("Chain1", "Chain2"), filename="multi analysis.pdf")

analyze.rwty <- function(chains, burnin, window.size, gens.per.tree=NA, treespace.points = 100,  ...){
    
    # If a single rwty.trees object is passed, it goes to the analyze.single
    # function.  Otherwise it assumes that multiple rwty.trees objects
    # have been passed as a list.
    if(class(chains) == "rwty.trees"){
        print("Analyzing single chain...")
        analyze.single(chains, burnin, window.size,
                       gens.per.tree, treespace.points, ...)
    }
    
    else{
        print("Analyzing multiple chains...")
        analyze.multi(chains, burnin, window.size, gens.per.tree, treespace.points, ...)
    }
}