slide.freq <- function(tree.list, burnin=0, window.size, gens.per.tree = 1, ...){ 
    #Specify burnin in TREES, not GENERATIONS
    
    # Peel just the trees off of rwty.trees object
    # so the function can take rwty.trees or multiPhylo
    if(class(tree.list) == "rwty.trees"){tree.list <- tree.list$trees}
    
    print(paste("input list length:", length(tree.list)))
    print(paste("burnin:", burnin))

    tree.list <- tree.list[(burnin+1):length(tree.list)]

    N <- length(tree.list)

    print(paste("post-burnin list length:", length(tree.list)))

    n.windows <- as.integer(N/window.size)

    print(paste("number of windows:", n.windows))

    print("a")


    # first we slice up our tree list into smaller lists
    tree.index <- seq_along(tree.list)
    tree.windows <- split(tree.list, ceiling(tree.index/window.size))[1:n.windows]

    print("b")


    # now we calculate clade frequencies on each of the lists of trees
    clade.freq.list <- lapply(tree.windows, clade.freq, start=1, end=window.size)

    # and rename the list to the first tree of each window
    names(clade.freq.list) = prettyNum(seq(1:length(clade.freq.list))*window.size*gens.per.tree, sci=TRUE)

    # this is the table of frequencies in each window
    print("Working on window 1")
    slide.freq.table <- clade.freq.list[[1]]
    colnames(slide.freq.table)[-1] <- names(clade.freq.list)[1]

    # now add in the rest
    for(i in 2:length(clade.freq.list)){
        print(paste("Working on window", i))
        # add data to slide.freq table
        slide.freq.table <- merge(slide.freq.table, clade.freq.list[[i]], by="cladenames", all=TRUE)
        colnames(slide.freq.table)[which(colnames(slide.freq.table)=="cladefreqs")] <- names(clade.freq.list)[i]
    }

    slide.freq.table[is.na(slide.freq.table)] <- 0.0
    rownames(slide.freq.table) <- slide.freq.table$cladenames
    slide.freq.table <- slide.freq.table[,-1]
    
    # calculate sd and mean of cumulative frequency and mean
    thissd <- apply(slide.freq.table, 1, sd)
    slide.freq.table$sd <- thissd
    
    thismean <- apply(slide.freq.table, 1, mean) 
    slide.freq.table$mean <- thismean
    
    # Sorting by sd, since these are usually the most interesting clades
    slide.freq.table <- slide.freq.table[order(slide.freq.table$sd, decreasing=TRUE),]
    
    # Building a new table that contains parsed clade names
    translation.table <- cbind(as.numeric(as.factor(rownames(slide.freq.table))), 
                                as.character(rownames(slide.freq.table)), 
                                parse.clades(rownames(slide.freq.table), tree.list))
    colnames(translation.table) <- c("Clade number", "Tip numbers", "Tip names")
    
    # Seting slide.freq.table to the same names as the translation table
    rownames(slide.freq.table) <- as.numeric(as.factor(rownames(slide.freq.table)))
    
    output <- list("slide.table" = slide.freq.table, "translation" = translation.table)
    class(output) <- "rwty.slide"
    output
}


   