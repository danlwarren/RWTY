slide.freq <- function(tree.list, burnin=0, window.size, gens.per.tree = 1, ...){ 
    #Specify burnin in TREES, not GENERATIONS

    start <- burnin + 1

    n.windows <- as.integer((length(tree.list) - start)/window.size)

    # first we slice up our tree list into smaller lists
    tree.index <- seq_along(tree.list)
    tree.windows <- split(tree.list, ceiling(tree.index/window.size))[1:n.windows]

    # now we calculate clade frequencies on each of the lists of trees
    clade.freq.list <- lapply(tree.windows, clade.freq, start=1, end=window.size)

    # and rename the list to the first tree of each window
    names(clade.freq.list) = prettyNum(seq(1:length(clade.freq.list))*window.size*gens.per.tree, sci=TRUE)    

    # this is the table of frequencies in each window
    slide.freq.table <- clade.freq.list[[1]]
    colnames(slide.freq.table)[-1] <- names(clade.freq.list)[1]

    # now add in the rest
    for(i in 2:length(clade.freq.list)){
        print(i)

        # add data to slide.freq table
        slide.freq.table <- merge(slide.freq.table, clade.freq.list[[i]], by="cladenames", all=TRUE)
        colnames(slide.freq.table)[which(colnames(slide.freq.table)=="cladefreqs")] <- names(clade.freq.list)[i]

    }

    slide.freq.table[is.na(slide.freq.table)] <- 0.0
    rownames(slide.freq.table) <- slide.freq.table$cladenames
    slide.freq.table <- slide.freq.table[,-1]

    slide.freq.table    

}


   