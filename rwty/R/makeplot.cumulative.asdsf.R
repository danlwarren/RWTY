#' Plot the variance and cumulative Average Standard Deviation of Split Frequencies.
#' 
#' This function takes two or more rwty.trees ojects and returns a cumulative plot of ASDSF 
#'
#' @param chains A list of two or more rwty.trees objects
#' @param burnin The number of samples to remove from the start of the chain as burnin
#' @param window.num The number of windows to use for generating plots
#' @param min.freq The minimum frequency for a node to be used for calculating ASDSF  
#' 
#' @return output A cumulative plot of ASDSF across all chains
#'
#' @keywords MCMC, phylogenetics, ASDSF, cumulative
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' test.chains <- check.chains(fungus)
#' p <- makeplot.asdsf(chains = test.chains, burnin = 100, window.num = 20)
#' p

makeplot.cumulative.asdsf <- function(chains, burnin, window.num, min.freq=0.1){
  
  chains <- check.chains(chains)
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
  #output <- data.frame(matrix(ncol=2, nrow=slide.wins))
  #colnames(output) <- c("Generations","ASDSF")
  #use to label plot
  all_SDs <- list()

  
  #populate clade frequency tables, one for each window
  for(i in 1:slide.wins){
    winTable <- NULL
    
    #attach tip names to clade frequencies for the first chain(necessary because clade name/numbers may not match among chains)
    if(i==1){ #special case of first window
      winTable <- data.frame(cbind(x[[1]]$translation[,3],x[[1]]$slide.table[,i]), stringsAsFactors=FALSE)
    }
    else{ # subsequent windows
      winTable <- data.frame(cbind(x[[1]]$translation[,3],rowMeans(x[[1]]$slide.table[,1:i])), stringsAsFactors=FALSE) 
    }
    colnames(winTable) <- c("Tip names",names(x)[1])
    class(winTable[,2]) <- "numeric"
    # Populate the rest of the table, one chain at a time
    for (j in 2:sets){
      thisTable <- NULL
    
      if(i==1){ #special case of first window
        thisTable <- data.frame(cbind(x[[j]]$translation[,3],x[[j]]$slide.table[,i]), stringsAsFactors=FALSE)
      }
      else{ # subsequent windows
        thisTable <- data.frame(cbind(x[[j]]$translation[,3],rowMeans(x[[j]]$slide.table[,1:i])), stringsAsFactors=FALSE)
      }
      colnames(thisTable) <- c("Tip names",names(x)[j])
      winTable <- merge(winTable,thisTable, by="Tip names", all=TRUE)
      winTable[is.na(winTable)] <- 0
      class(winTable[,j+1]) <- "numeric"
    }
    # calculate ASDSF across all chains for each window
    # filter for clades below min freq then save ASDSF for that window
    winTable <- winTable[,-1]
    winTable <- winTable[apply(winTable, MARGIN = 1, function(x) any(x > min.freq)), ]
    all_SDs[[i]] <- apply(winTable,1, sd, na.rm = TRUE)
  }
  
  #now that we have collected SDSF for all windows, return a pretty ggplot boxplot
generations <- as.numeric(as.character(names(slide.data$slide.table[1:slide.wins])))  
d <- data.frame(x = unlist(all_SDs), 
                grp = rep(generations[1:length(all_SDs)],times = sapply(all_SDs,length)))


ggplot(d,aes(x = grp, y = x, group=grp)) + geom_boxplot()  



  thisplot <- ggplot(data=d, aes(x = grp, y = x, group=grp)) + 
    geom_boxplot() +
    theme(legend.position="none") +
    xlab("Generations") + 
    ylab("Standard Deviation of Split Frequencies") + ggtitle("Cumulative SDSF (all chains)") +
    theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5))
  
  
  
  thisplot
}