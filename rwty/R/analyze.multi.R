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
#' data(fungus)
#' analyze.multi(list(run1, run2), burnin=100, window.size=20, treespace.points=100, filename="fungus.pdf", labels=c("Chain1", "Chain2"))

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
    #
    output[["discordance.n"]] <- discordance.n(output, setnames=labels)
    #
    pdf(file = paste("Compare", filename))
    print(output$compare.n$compare.plot)
    plot(output$compare.n$discordance)
    axisPhylo()
    lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    mtext("Discordance", side=1, line=2, at=max(lastPP$xx)/2)
    dev.off()
    
    output
}