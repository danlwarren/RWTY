#' Function for running rwty analyses on multiple chains.
#' 
#' This function is automatically called when analyze.simple is called with multple chains.
#' It runs analyze.single for each chain, and then does multi-chain analyses as well. 
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin.  Default is zero. 
#' @param window.num The number of windows along the chain to use for analyses.
#' @param min.freq The minimum frequency for a node to be used for calculating ASDSF. 
#'
#' @return output A list of plots illustrating agreement between chains.
#'
#' @keywords MCMC, phylogenetics, convergence, plot, awty, rwty
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' makeplot.multichain(fungus, burnin=100,  window.num = 20, min.freq = 0.1 )

makeplot.multichain <- function(chains, burnin, window.num, min.freq = 0.1){
  
  output <- list()
  
  compn <- compare.n(chains, setnames=names(chains), burnin, min.freq=min.freq)
  output[["cumulative.asdsf"]] <- makeplot.cumulative.asdsf(chains, burnin, window.num, min.freq)
  output[["compare.plot"]] <- compn$compare.plot
  
  if(all(compn$asdsf  == 0)){
    print("No non-zero ASDSF values, skipping ASDSF tree")  
  }
  else{
    dtree <- as.phylo(hclust(compn$asdsf))
    plot.phylo(dtree, main="Chains clustered by ASDSF")
    axisPhylo()
    lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    mtext(paste("Pairwise ASDSF (minimum clade frequency = ",min.freq,")", sep=""), side=1, line=2, at=max(lastPP$xx)/2)
    output[["asdsf.tree"]] <- recordPlot()
  }
  
  output
}
