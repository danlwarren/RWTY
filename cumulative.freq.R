cumulative.freq <- function(x, burnin=0, window, gens.per.tree = 1, ...){ 
    #Specify burnin in TREES, not GENERATIONS

    start <- burnin + 1

    # figure out how many segments to split the list of trees into

    # define number of non-overlapping windows
    n.windows <- as.integer((length(x) - burnin)/window)

    print("Populating table...")

    #Getting all clades from chain into table
    allnames <- clade.freq(x, start = start, end  = length(x))$cladefreqs[,1] 

    clade.table <- as.data.frame(matrix(0, ncol=0, nrow = length(allnames)))
    
    clade.table$cladenames <- allnames

    for(i in 1:n.windows){
        print(paste("window", i, "of", n.windows))
        thiswindow.table <- clade.freq(x, start = start, end  = burnin + (i) * window)
        clade.table <- merge(clade.table, thiswindow.table$cladefreqs, by = "cladenames", all = TRUE)
        colnames(clade.table)[i+1] <- ((i-1) * window * gens.per.tree) + (burnin * gens.per.tree)
    }

    clade.table[is.na(clade.table)] <- 0

    thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
    clade.table$sd <- thissd

    thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
    clade.table$mean <- thismean

    clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]

    output <- list("cladetable" = clade.table, "plot" = plawty(clade.table, ...))

    class(output) <- "rwty.cumulative"

    output
  }