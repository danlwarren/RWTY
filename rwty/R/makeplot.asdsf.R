#' Plot the Average Standard Deviation of Split Frequencies in sliding windows.
#' 
#' This function takes two or more rwty.trees ojects and returns a sliding window plot of ASDSF 
#'
#' @param chains A list of two or more rwty.trees objects
#' @param burnin The number of samples to remove from the start of the chain as burnin
#' @param window.num The number of windows to use for generating plots
#' @param min.freq The minimum frequency for a node to be used for calculating ASDSF  
#' 
#' @return output A sliding window plot of ASDSF across all chains
#'
#' @keywords MCMC, phylogenetics, ASDSF, sliding window
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' test.chains <- list(run1, run2)
#' test.chains <- check.chains(test.chains)
#' p <- makeplot.asdsf(chains = test.chains, burnin = 100, window.num = 20)
#' p

makeplot.asdsf <- function(chains, burnin, window.num, min.freq=0.1, ...){
  
  labels <- names(chains)
  x <- list()
  
  # Calculate frequencies for each window for each chain
  for(m in 1: length(chains)){ 
    window.size <- (length(chains[[m]]$trees) - burnin)/window.num
    gens.per.tree=chains[[m]]$gens.per.tree
    slide.data <- NULL
    slide.data <- slide.freq(chains[[m]]$trees, burnin, window.size, gens.per.tree)  
    x[[labels[m]]] <- slide.data
  }
  
  # set initial values
  sets <- length(x)
  slide.wins <- ncol(x[[1]]$slide.table)-2
  output <- data.frame(matrix(ncol=2, nrow=slide.wins))
  colnames(output) <- c("Generations","ASDSF")
  output[,1] <- as.numeric(as.character(names(slide.data$slide.table[1:slide.wins])))

  #populate clade frequency tables, one for each sliding window
  for(i in 1:slide.wins){
    #attach tip names to clade frequencies for the first chain(necessary because clade name/numbers may not match among chains)
    winTable <- data.frame(cbind(x[[1]]$translation[,3],x[[1]]$slide.table[,i]), stringsAsFactors=FALSE)
    colnames(winTable) <- c("Tip names",names(x)[1])
    class(winTable[,2]) <- "numeric"
    # Populate the rest of the table, one chain at a time #CHANGE "SETS" TO "CHAINS"
    for (j in 2:sets){
      thisTable <- data.frame(cbind(x[[j]]$translation[,3],x[[j]]$slide.table[,i]), stringsAsFactors=FALSE)
      colnames(thisTable) <- c("Tip names",names(x)[j])
      winTable <- merge(winTable,thisTable, by="Tip names", all=TRUE)
      winTable[is.na(winTable)] <- 0
      class(winTable[,j+1]) <- "numeric"
    }
    # calculate ASDSF across all chains for each sliding window
    # filter for clades below min freq then save ASDSF for that window
    winTable <- winTable[,-1]
    winTable <- winTable[apply(winTable, MARGIN = 1, function(x) all(x > min.freq)), ]
    output[i,2] <- mean(apply(winTable,1, sd))
  }
  
  #now that we have ASDSF for all windows, return a pretty ggplot figure
  thisplot <- ggplot(data=output, aes(x=Generations, y=ASDSF, color="black")) + 
    geom_line(stat = "identity") +
    theme(legend.position="none") +
    xlab("Generations") + ggtitle("Sliding Window ASDSF (all chains)")
  thisplot
}
