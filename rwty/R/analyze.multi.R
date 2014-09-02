#' Function for running rwty analyses on multiple chains.
#' 
#' This function is automatically called when analyze.rwty is called with multple chains.
#' It runs analyze.single for each chain, and then does multi-chain analyses as well. 
#'
#' @param chains A list of rwty.trees objects. \code{chains}
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#' @param window.size The length of window (in trees) for the sliding window plot \code{window.size}
#' @param gens.per.tree The number of generations per tree in the .t file.
#' @param treespace.points The number of trees to plot in the treespace plot.  
#' @param filename A name to be used for generating pdfs of output.  \code{filename}
#' @param labels The name to use on plots and in generating output files.  \code{labels}
#'
#' @return output A list of outputs from the analyze.single runs on each chain, as well as a compare.n run for all chains.  Eventually we will add more multi-chain analyses.
#'
#' @keywords MCMC, phylogenetics, convergence, plot, awty, rwty
#'
#' @export
#' 
#' @examples
#' analyze.multi(list(chain1, chain2, chain3), burnin=100, window.size=100, gens.per.tree=1000, treespace.points=100, filename="Run1.pdf", labels=c("Chain 1", "Chain 2", "Chain 3"))

analyze.multi <- function(chains, burnin, window.size, gens.per.tree=NA, treespace.points=100, filename=NA,  labels=NA, ...){
    
    output <- list()
    
    # Name chains by list order if labels aren't supplied
    if(any(is.na(labels))){labels <- seq(1, length(chains))}
    
    # Run analyze single on each chain
    for(i in 1: length(chains)){
        if(!is.na(filename)){
            thisfilename <- paste( labels[i], filename)
            print(thisfilename)
        }
        output[[labels[i]]] <- c(output, analyze.single(chains[[i]], burnin, window.size, 
                                gens.per.tree=chains[[i]]$gens.per.tree, 
                                treespace.points, labels=labels[[i]], filename = thisfilename, ... ))
    }
    
    output[["compare.n"]] <- compare.n(chains, setnames=labels, burnin)
    
    pdf(file = paste("Compare", filename))
    print(output$compare.n$compare.plot)
    dev.off()
    
    output
}