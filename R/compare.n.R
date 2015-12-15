#' Compares posterior probability estimates from multiple MCMC chains.
#' 
#' This function takes a set of tree files, an option array of names for those
#' files, and a burnin argument.  It returns a table with the frequencies of 
#' each clade in each of those tree files, the pairwise and grand mean average
#' and a translation table.
#'
#' @param chains A list of rwty.trees objects.
#' @param burnin The number of trees to eliminate as burnin 
#' @param min.freq The minimum frequency for a node to be used for calculating Average Standard Deviation of Split Frequncies (ASDSF).  Default value is 0.0.
#'
#' @return output A list containing a table of frequencies of each clade in each chain along with mean and sd, a distance matrix measuring consensus between chains, a translation table, and a ggpairs plot.
#'
#' @keywords MCMC, phylogenetics, consensus, clade frequency, convergence
#'
#' @export compare.n
#' @examples
#' data(fungus)
#' compare.n(fungus, burnin=20)

compare.n <- function(chains, burnin = 0, min.freq=0){

  dat = get.comparison.table(chains, burnin, min.freq)

  dat = dat$cladetable

  dat = dat[,!(names(dat) %in% c("mean", "sd"))] #Remove mean and sd

  # thanks to Gaston Sanchez here: https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
  gg1 = makePairs(dat[,-1])
 
  # new data frame mega dat
  mega_dat = data.frame(gg1$all, Clade=rep(dat$cladenames, length=nrow(gg1$all)))

  


  # Make a plot
  assignInNamespace("ggally_cor", ggally_cor, "GGally")
  assign("asdsf.min", min.freq, envir=globalenv())
  plot <- ggpairs(clade.table, columns=2:(length(x) + 1),axisLabels='show',diag=list(continuous="blank",params=c(colour="black")),upper=list(params=list(Size=10)))

  return(plot)

}


makePairs <- function(data) 
{
  # thanks to Gaston Sanchez here: https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}
 

get.comparison.table <- function(chains, burnin, min.freq){

  print("Calculating clade posterior probabilities from all runs...")
  
  print(paste("Working on chain", 1))
  
  setnames = names(chains)

  # Get a starting table from the first chain
  clade.table <- clade.freq(chains[[1]], start=burnin, end=length(chains[[1]]$trees))
  if(is.na(setnames[1])){colnames(clade.table)[2] <- paste("set", 1, sep=".")}
  else{colnames(clade.table)[2] <- setnames[1]}
  
  # Populate the rest of the table, one chain at a time
  for(i in 2:length(chains)){
    print(paste("Working on chain", i))
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
