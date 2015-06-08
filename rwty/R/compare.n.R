#' Compares posterior probability estimates from multiple MCMC chains.
#' 
#' This function takes a set of tree files, an option array of names for those
#' files, and a burnin argument.  It returns a table with the frequencies of 
#' each clade in each of those tree files, the pairwise and grand mean average
#' and a translation table.
#'
#' @param x A list of rwty.trees objects.
#' @param setnames A list of names for the chains.
#' @param burnin The number of trees to eliminate as burnin 
#' @param min.freq The minimum frequency for a node to be used for calculating Average Standard Deviation of Split Frequncies (ASDSF).  Default value is 0.1.  
#'
#' @return output A list containing a table of frequencies of each clade in each chain along with mean and sd, a distance matrix measuring consensus between chains, a translation table, and a ggpairs plot.
#'
#' @keywords MCMC, phylogenetics, consensus, clade frequency, convergence
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' compare.n(list(run1, run2), setnames=c("Chain1", "Chain2"), burnin=100)

compare.n <- function(x, setnames=NA, burnin, min.freq=0){ # In this case x is a list of rwty.trees objects
  print("Calculating clade posterior probabilities from all runs...")
  
  print(paste("Working on chain", 1))
  
  # Get a starting table from the first chain
  clade.table <- clade.freq(x[[1]], start=burnin, end=length(x[[1]]$trees))
  if(is.na(setnames[1])){colnames(clade.table)[2] <- paste("set", 1, sep=".")}
  else{colnames(clade.table)[2] <- setnames[1]}
  
  # Populate the rest of the table, one chain at a time
  for(i in 2:length(x)){
    print(paste("Working on chain", i))
    thistable <- clade.freq(x[[i]], start = burnin, end  = length(x[[i]]$trees))
    clade.table <- merge(clade.table, thistable, by = "cladenames", all = TRUE) 
    
    # Either name it with the label provided or with the variable name
    if(is.na(setnames[i])){colnames(clade.table)[i+1] <- paste("set", i, sep=".")}
    else{colnames(clade.table)[i+1] <- setnames[i]}
  }
  
  # Set missing clades from each chain to zero
  clade.table[is.na(clade.table)] <- 0
  
  # Calculate the pairwise average standard deviation of split frequencies
  # (ASDSF) for clades occuring at a minimum frequency of min.freq
  # across a pair of chains.  
  d <- matrix(nrow=length(x), ncol=length(x))
  asdsf.clade.table <- clade.table
  asdsf.clade.table <- asdsf.clade.table[apply(asdsf.clade.table, MARGIN = 1, function(x) any(x > min.freq)), ]
  for(i in 1:length(x)){
    for(j in i+1:length(x)){
      if(j <= length(x)){
        temp.pair <- NULL
        temp.pair <- data.frame(cbind(asdsf.clade.table[,i+1],asdsf.clade.table[,j+1]))
        temp.pair <- transform(temp.pair, SD=apply(temp.pair,1, sd, na.rm = TRUE))
        d[j,i] <- mean(temp.pair$SD)
      }
    }
  }
  colnames(d) <- names(clade.table)[2:(length(x)+1)]
  rownames(d) <- names(clade.table)[2:(length(x)+1)]
  d <- as.dist(d)
  
  # Summary stats, sort by SD
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  
  # Create a table that translates clade names
  translation.table <- cbind(as.numeric(clade.table[,1]), as.character(clade.table[,1]), parse.clades(clade.table[,1], x[[1]]))
  clade.table[,1] <- as.numeric(clade.table[,1])
  
  # Make a plot
  assignInNamespace("ggally_cor", ggally_cor, "GGally")
  assign("asdsf.min", min.freq, envir=globalenv())

  plot <- ggpairs(clade.table, columns=2:(length(x) + 1),axisLabels='show',diag=list(continuous="blank",params=c(colour="black")),upper=list(params=list(Size=10)))
  
  output <- list("cladetable" = clade.table, "asdsf" = d, "asdsf.min.freq" = min.freq,
                 "translation" = translation.table,
                 "compare.plot" = plot)
  class(output) = "rwty.comparen"
  output
}
