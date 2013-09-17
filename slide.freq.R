slide.freq <- function(x, burnin=0, window, gens.per.tree = 1, ...){ #Specify burnin in TREES, not GENERATIONS
  start <- burnin
  n.windows <- as.integer((length(x) - burnin)/window)
  print("Populating table...")
  allnames <- clade.freq(x, start = burnin, end  = length(x))$cladefreqs[,1] #Getting all clades from chain into table
  clade.table <- as.data.frame(matrix(0, ncol=0, nrow = length(allnames)))
  clade.table$cladenames <- allnames
  for(i in 1:n.windows){
    print(paste("window", i, "of", n.windows))
    thiswindow.table <- clade.freq(x, start = burnin + (i-1) * window, end  = burnin + (i) * window)
    clade.table <- merge(clade.table, thiswindow.table$cladefreqs, by = "cladenames", all = TRUE)
    colnames(clade.table)[i+1] <- ((i-1) * window * gens.per.tree) + (burnin * gens.per.tree)
  } 
  clade.table[is.na(clade.table)] <- 0
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  output <- list("cladetable" = clade.table, "plot" = plawty(clade.table, ...))
  class(output) <- "rwty.slide"
  output
}