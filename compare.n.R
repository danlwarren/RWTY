# This function takes a set of tree files, an option array of names for those
# files, and a burnin argument.  It returns a table with the frequencies of 
# each clade in each of those tree files, a distance metric indicating the mean
# difference between clade frequencies, a plot of each chain against the others,
# and a translation table.

compare.n <- function(x, setnames=NA, burnin){ # In this case x is a list of rwty.trees objects
  print("Populating table...")
  
  print(paste("Working on set", 1))
  
  # Get a starting table from the first chain
  clade.table <- clade.freq(x[[1]], start=burnin, end=length(x[[1]]$trees))
  if(is.na(setnames[1])){colnames(clade.table)[2] <- paste("set", 1, sep=".")}
  else{colnames(clade.table)[2] <- setnames[1]}
  
  # Populate the rest of the table, one chain at a time
  for(i in 2:length(x)){
    print(paste("Working on set", i))
    thistable <- clade.freq(x[[i]], start = burnin, end  = length(x[[i]]$trees))
    clade.table <- merge(clade.table, thistable, by = "cladenames", all = TRUE) 
    
    # Either name it with the label provided or with the variable name
    if(is.na(setnames[i])){colnames(clade.table)[i+1] <- paste("set", i, sep=".")}
    else{colnames(clade.table)[i+1] <- setnames[i]}
  }
  
  # Set missing clades from each chain to zero
  clade.table[is.na(clade.table)] <- 0
  
  # Calculate a distance metric, which is just the average 
  # of the absolte values of the differences in posteriors
  # for each clade across a pair of chains.  Ranges from 0,
  # where posteriors are identical, to 1, where they are
  # as different as can be.
  d <- matrix(nrow=length(x), ncol=length(x))
  for(i in 1:length(x)){
    for(j in i+1:length(x)){
      if(j <= length(x)){d[i,j] <- mean(abs(clade.table[,i+1] - clade.table[,j+1]))}
    }
  }
  
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
  # ggpairs isn't working, not sure why
  plot <- ggpairs(clade.table, columns=2:(length(x) + 1))
  
  #plot <- pairs(clade.table[,2:(length(x) + 1)])
  
  
  output <- list("cladetable" = clade.table, "dist" = d, 
                 "translation" = translation.table,
                 "plot" <- plot)
  class(output) = "rwty.comparen"
  output
}
