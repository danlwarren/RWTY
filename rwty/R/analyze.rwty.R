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
#' @param step The number of trees to skip for each step in the treespace plot.  Step=5 would amount to reading every fifth tree.
#'
#' @return output A list of outputs from the analyze.single runs on each chain, as well as a compare.n run for all chains.  Eventually we will add more multi-chain analyses.
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' analyze.rwty(chain1, burnin=100, window.size=100, gens.per.tree=1000, step=5)
#' analyze.rwty(list(chain1, chain2, chain3), burnin=100, window.size=100, gens.per.tree=1000, step=5, labels=c("Chain 1", "Chain 2", "Chain 3"))

analyze.rwty <- function(chains, burnin, window.size, gens.per.tree=NA, step=1, ...){
    
    # If a single rwty.trees object is passed, it goes to the analyze.single
    # function.  Otherwise it assumes that multiple rwty.trees objects
    # have been passed as a list.
    if(class(chains) == "rwty.trees"){
        print("Analyzing single chain...")
        analyze.single(chains, burnin, window.size,
                       gens.per.tree, step, ...)
    }
    
    else{
        print("Analyzing multiple chains...")
        analyze.multi(chains, burnin, window.size, gens.per.tree, step, ...)
    }
}