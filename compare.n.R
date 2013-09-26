# This function takes a set of tree files, an option array of names for those
# files, and a burnin argument.  It returns a table with the frequencies of 
# each clade in each of those tree files, a distance metric indicating the mean
# difference between clade frequencies, a plot of each chain against the others,
# and a translation table.

compare.n <- function(x, setnames=NA, burnin){ # In this case x is a list of treefiles
  print("Populating table...")
  print(paste("Working on set", 1))
  clade.table <- clade.freq(x[[1]], start=burnin, end=length(x[[1]]))
  if(is.na(setnames[1])){colnames(clade.table)[2] <- paste("set", 1, sep=".")}
  else{colnames(clade.table)[2] <- setnames[1]}
  
  for(i in 2:length(x)){
    print(paste("Working on set", i))
    thistable <- clade.freq(x[[i]], start = burnin, end  = length(x[[i]]))
    clade.table <- merge(clade.table, thistable, by = "cladenames", all = TRUE) 
    if(is.na(setnames[i])){colnames(clade.table)[i+1] <- paste("set", i, sep=".")}
    else{colnames(clade.table)[i+1] <- setnames[i]}
  }
  clade.table[is.na(clade.table)] <- 0
  d <- matrix(nrow=length(x), ncol=length(x))
  for(i in 1:length(x)){
    for(j in i+1:length(x)){
      if(j <= length(x)){d[i,j] <- mean(abs(clade.table[,i+1] - clade.table[,j+1]))}
    }
  }
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  
  translation.table <- cbind(as.numeric(clade.table[,1]), as.character(clade.table[,1]), parse.clades(clade.table[,1], x[[1]]))
  clade.table[,1] <- as.numeric(clade.table[,1])
  
  output <- list("cladetable" = clade.table, "dist" = d, "translation" = translation.table)
  class(output) = "rwty.comparen"
  output
}
