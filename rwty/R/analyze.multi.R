#' Function for running rwty analyses on multiple chains.
#' 
#' This function is automatically called when analyze.rwty is called with multple chains.
#' It runs analyze.single for each chain, and then does multi-chain analyses as well. 
#'
#' @param chains A list of rwty.trees objects. \code{chains}
#' @param burnin The number of trees to eliminate as burnin.  Default is zero. \code{burnin}
#' @param window.size The length of window (in trees) for the sliding window plot \code{window.size}
#' @param gens.per.tree The number of generations per tree in the .t file.  If no value is provided, RWTY tries to figure it out from the tree names. \code{gens.per.tree}
#' @param treespace.points The number of trees to plot in the treespace plot.  Default is 100. \code{treespace.points} 
#' @param filename A name to be used for generating pdfs of output.  If a name is not provided, one will be automatically generated from the labels.  \code{filename}
#' @param labels The name to use on plots and in generating output files.  If none are provided, defaults are created using Chain 1, Chain 2, etc.  \code{labels}
#' @param min.freq The minimum frequency for a node to be used for calculating discordance.  \code{min.freq}
#'
#' @return output A list of outputs from the analyze.single runs on each chain, as well as a compare.n run and discordance data for all chains. 
#'
#' @keywords MCMC, phylogenetics, convergence, plot, awty, rwty
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' analyze.multi(list(run1, run2), burnin=100, window.size=20, treespace.points=100, filename="fungus.pdf", labels=c("Chain1", "Chain2"))

analyze.multi <- function(chains, burnin, window.size, gens.per.tree=NA, treespace.points=100, filename=NA,  labels=NA, min.freq=0,...){
    
    output <- list()
    
    # Name chains by variable name if labels aren't supplied for all chains
    if(any(is.na(labels))){
        labels <- c(paste("Chain", seq(1:length(chains)), sep="."))
        print(labels)
    }
    
    # Run analyze single on each chain, without treespace plots (these come in the multi analysis)
    for(i in 1: length(chains)){
        if(!is.na(filename)){
            thisfilename <- paste( labels[i], filename)
            print(thisfilename)
        }
        else{
            thisfilename = NA
        }
        output[[labels[i]]] <- c(output, analyze.single(chains[[i]], burnin, window.size, 
                                                        gens.per.tree=chains[[i]]$gens.per.tree, 
                                                        treespace = FALSE, labels=labels[i], filename = thisfilename, ... ))
    }

    output[["compare.n"]] <- compare.n(chains, setnames=labels, burnin, min.freq=min.freq)
    output$compare.n$discordance.tree <- as.phylo(hclust(output$compare.n$discordance))
    output[["discordance.n"]] <- discordance.n(output, setnames=labels, min.freq=min.freq)
    output[["treespace"]] <- treespace(chains, n.points=treespace.points, burnin=burnin)
    
    pdf(file = paste("Compare", filename), height=2*attr(output$compare.n$discordance, "Size"), width=2*attr(output$compare.n$discordance, "Size"))
    print(output$compare.n$compare.plot)    
    dev.control(displaylist="enable")
    plot.phylo(output$compare.n$discordance.tree, main="Chains clustered by discordance")
    axisPhylo()
    lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    mtext(paste("Discordance (minimum clade frequency = ",min.freq,")", sep=""), side=1, line=2, at=max(lastPP$xx)/2)
    output$compare.n$discordance.plot <- recordPlot()
    dev.off()
    
    output
}
