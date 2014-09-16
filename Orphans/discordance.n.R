#' Compares posterior probability estimates from multiple MCMC chains.
#' 
#' This function takes a set of tree files, an option array of names for those
#' files, and a burnin argument.  It returns a plot of the change in discordance
#'
#' @param x A list of rwty.trees objects.
#' @param setnames A list of names for the chains.
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#'
#' @return output A list containing a table of frequencies of each clade in each chain along with mean and sd, a distance matrix measuring consensus between chains, a translation table, and a ggpairs plot.
#'
#' @keywords MCMC, phylogenetics, discordance, sliding window
#'
#' @export
#' 
#' @examples
#' discordance.n(list(set1,set2,set3), setnames=c("Chain 1", "Chain 2", "Chain 3"))


# Calculate a discordance metric, which is just the average 
# of the absolute values of the differences in posteriors
# for each clade across a pair of chains.  Ranges from 0,
# where posteriors are identical, to 1, where they are
# as different as can be.


discordance.n <- function(x, setnames=NA){ # In this case x is a list of rwty.trees objects or alternatively a set of slide.objects

  sets <- length(x)-1
  slide.wins <- ncol(x[[1]]$slide.data$slide.table)-2
  output <- list(length=slide.wins)

  # Populate the rest of the table, one chain at a time
  for(i in 1:slide.wins){
    winTable <- data.frame(cbind(x[[1]]$slide.data$translation[,3],x[[1]]$slide.data$slide.table[,i]), stringsAsFactors=FALSE)
    colnames(winTable) <- c("Tip names",names(x)[1])
    class(winTable[,2]) <- "numeric"
    for (j in 2:sets){
      thisTable <- data.frame(cbind(x[[j]]$slide.data$translation[,3],x[[j]]$slide.data$slide.table[,i]), stringsAsFactors=FALSE)
      colnames(thisTable) <- c("Tip names",names(x)[j])
      winTable <- merge(winTable,thisTable, by="Tip names", all=TRUE)
      winTable[is.na(winTable)] <- 0
      class(winTable[,j+1]) <- "numeric"
    }
    win.out <- list()
    discordance <- discordance.mat(winTable)
    win.out <- list("window.table" = winTable, "discordance.dist" = discordance, "discordance.plot" = plot(hclust(discordance)))
    output[[i]] <- win.out
    names(output)[i] <- paste("window",i,sep=".")
  }     
}




      # Either name it with the label provided or with the variable name
      if(is.na(setnames[i])){colnames(clade.table)[i+1] <- names(x)[i]}
      else{colnames(clade.table)[i+1] <- setnames[i]}
  }
  
  
  
  
  
#SINGLE CHAIN DISCORDANCE  
  
  d <- vector(length=ncol(slide.freq.table)-3)
for(i in 1:length(d)){
  d[i] <- mean(abs(slide.freq.table[,i] - slide.freq.table[,i+1]))
}
x <- as.numeric(as.character(names(clade.freq.list)[2:length(clade.freq.list)]))
df <- data.frame(cbind(x,d))
#plot(d~names(clade.freq.list)[2:length(clade.freq.list)], ylim=c(0,1), ylab="Discordance", xlab="Generation")
plot <- ggplot(df, aes(x = x, y = d, ymin=0, ymax=1)) + geom_line() + geom_line(data = df, aes(y = d)) + xlab("Generation") + ylab("Discordance")
