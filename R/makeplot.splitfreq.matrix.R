#' Plots a matrix of split frequency comparisons between multiple MCMC chains.
#' 
#' This function takes list of rwty.chain objects, and returns a scatterplot matrix
#' in which each plot shows the split frequencies of all clades that appear in one or both 
#' MCMC chains at least once. In the upper diagonal, we show the correlation between the split
#' frequencies (Pearson's R), and the Average Standard Deviation of the split frequencies.
#'
#'
#' @param chains A list of rwty.chain objects.
#' @param burnin The number of trees to eliminate as burnin 
#'
#' @return output A list of two plots: the first is a matrix of scatterplots, where each point is a clade, and the values are the split frequencies of that clade in the post-burnin trees of each chain. The second plot is a tree of the chains clustered by their ASDSFs.
#'
#' @keywords MCMC, phylogenetics, consensus, clade frequency, convergence, ASDSF
#'
#' @export makeplot.splitfreq.matrix
#' @examples
#' \dontrun{
#' data(salamanders)
#' makeplot.splitfreq.matrix(salamanders[1:4], burnin = 20)
#' }

makeplot.splitfreq.matrix <- function(chains, burnin = 0){

  print(sprintf("Creating split frequency matrix and ASDSF clustering plots"))

  dat = get.comparison.table(chains, burnin, min.freq = 0)

  asdsf = dat$asdsf
  dat = dat$cladetable

  dat = dat[,(names(dat) %in% names(chains))] #keep only the chain values 

  # Helper function to make lower diagonal plots for ggpairs
  lower_fn <- function(data, mapping, method="loess", ...){
    p <- ggplot(data = data, mapping = mapping) + 
      geom_point() + 
      geom_smooth(method=method, se = FALSE, color = "black", ...) +
      geom_abline(slope = 1, intercept = 0, linetype = "longdash")
    p
  }
  
  # Building the split frequency plot matrix
  splitfreq.matrix <- ggpairs(dat, lower = list(continuous = lower_fn),
          diag = NULL, axisLabels = "internal") + theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  

  if(all(asdsf  == 0)){
    print("No non-zero ASDSF values, skipping ASDSF tree")  
  }
  else{

    hc <- hclust(asdsf)
    asdsf.tree <- ggdendrogram(hc, rotate=TRUE, theme_dendro = FALSE) + ylab("Pairwise ASDSF") + xlab("")

  }

  return(list("splitfreq.matrix" = splitfreq.matrix, "asdsf.tree" = asdsf.tree))

}


get.comparison.table <- function(chains, burnin, min.freq){
  
  setnames = names(chains)

  # Get a starting table from the first chain
  clade.table <- clade.freq(chains[[1]], start=burnin, end=length(chains[[1]]$trees))
  if(is.na(setnames[1])){colnames(clade.table)[2] <- paste("set", 1, sep=".")}
  else{colnames(clade.table)[2] <- setnames[1]}
  
  # Populate the rest of the table, one chain at a time
  for(i in 2:length(chains)){
    thistable <- clade.freq(chains[[i]], start = burnin, end  = length(chains[[i]]$trees))
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
  d <- matrix(nrow=length(chains), ncol=length(chains))
  asdsf.clade.table <- clade.table

  for(i in 1:length(chains)){
    for(j in i+1:length(chains)){
      if(j <= length(chains)){
        temp.pair <- NULL
        temp.pair <- data.frame(cbind(asdsf.clade.table[,i+1],asdsf.clade.table[,j+1]))
        temp.pair <- temp.pair[apply(temp.pair, MARGIN = 1, function(x) any(x > min.freq)), ]
        temp.pair <- transform(temp.pair, SD=apply(temp.pair,1, sd, na.rm = TRUE))
        d[j,i] <- mean(temp.pair$SD)
      }
    }
  }
  colnames(d) <- names(clade.table)[2:(length(chains)+1)]
  rownames(d) <- names(clade.table)[2:(length(chains)+1)]
  d <- as.dist(d)
  
  # Summary stats, sort by SD
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  
  # Create a table that translates clade names
  translation.table <- cbind(as.numeric(clade.table[,1]), as.character(clade.table[,1]), parse.clades(clade.table[,1], chains[[1]]))
  clade.table[,1] <- as.numeric(clade.table[,1])
  
  output <- list("cladetable" = clade.table, "asdsf" = d, "asdsf.min.freq" = min.freq,
                 "translation" = translation.table)
  class(output) = "rwty.comparen"

  return(output)
}
