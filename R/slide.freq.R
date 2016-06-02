#' Sliding window measurements of clade split frequencies.
#' 
#' This function takes sliding windows of a specified length over an MCMC chain
#' and calculates the split frequency of clades within that window.  It
#' allows users to see whether the chain is visiting different areas of treespace.
#'
#' @param chains A list of rwty.trees objects. 
#' @param burnin The number of trees to eliminate as burnin. Defaults to zero. 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#'
#' @return A list of rwty.slide objects, one per chain in the input list of chains.
#' Each rwty.slide object contains the frequencies of clades in the sliding
#' windows, and a translation table that converts clade groupings to factors.
#'
#' @keywords MCMC, split frequency, convergence
#'
#' @export slide.freq
#' @examples
#' data(fungus)
#' slide.data <- slide.freq(fungus, burnin=20)

slide.freq <- function(chains, burnin=0, window.size = 20){ 

    chains = check.chains(chains)
    trees = lapply(chains, function(x) x[['trees']])

    if((length(trees[[1]]) - burnin) < 2 * window.size ){
        stop("ERROR: burnin is too large to make at least two points for the plot, quitting. Try setting a smaller burnin and/or a smaller window size")
    }

    slide.freq.list = lapply(trees, get.slide.freq.table, burnin = burnin, window.size = window.size, gens.per.tree = chains[[1]]$gens.per.tree)
    return(slide.freq.list)

}




get.slide.freq.table <- function(tree.list, burnin = 0, window.size = 20, gens.per.tree = 1){

    tree.list = tree.list[(burnin+1):length(tree.list)]

    # take a list of trees and return a table of split frequencies of clades
    # from a sliding window
    n.windows = as.integer(length(tree.list)/window.size)

    # first we slice up our tree list into smaller lists
    tree.index = seq_along(tree.list)
    tree.windows = split(tree.list, ceiling(tree.index/window.size))[1:n.windows]

    # now we calculate clade frequencies on each of the lists of trees
    clade.freq.list = lapply(tree.windows, clade.freq, start=1, end=window.size)
    
    # and rename the list to the first tree of each window
    names(clade.freq.list) = prettyNum(seq((burnin + 1),(burnin + length(clade.freq.list) * window.size), by=window.size)*gens.per.tree, sci=TRUE)

    # this is the table of frequencies in each window
    slide.freq.table = clade.freq.list[[1]]
    colnames(slide.freq.table)[-1] = names(clade.freq.list)[1]

    # now add in the rest
    for(i in 2:length(clade.freq.list)){
        slide.freq.table = merge(slide.freq.table, clade.freq.list[[i]], by="cladenames", all=TRUE)
        colnames(slide.freq.table)[which(colnames(slide.freq.table)=="cladefreqs")] = names(clade.freq.list)[i]
    }

    # tidy things up
    slide.freq.table[is.na(slide.freq.table)] = 0.0
    rownames(slide.freq.table) = slide.freq.table$cladenames
    slide.freq.table = slide.freq.table[,-1]
    
    # calculate sd and mean of cumulative frequency and mean
    thissd = apply(slide.freq.table, 1, sd)
    thismean = apply(slide.freq.table, 1, mean) 
    thisess = apply(slide.freq.table, 1, effectiveSize)

    slide.freq.table$sd = thissd
    slide.freq.table$mean = thismean
    slide.freq.table$ess = thisess
   
    # Sorting by sd, since these are usually the most interesting clades
    slide.freq.table = slide.freq.table[order(slide.freq.table$sd, decreasing=TRUE),]
    
    # Building a new table that contains parsed clade names
    translation.table = cbind(as.numeric(as.factor(rownames(slide.freq.table))), 
                                as.character(rownames(slide.freq.table)), 
                                parse.clades(rownames(slide.freq.table), tree.list))
    colnames(translation.table) = c("Clade number", "Tip numbers", "Tip names")
    
    # Seting slide.freq.table to the same names as the translation table
    rownames(slide.freq.table) = as.numeric(as.factor(rownames(slide.freq.table)))
    
    output <- list("slide.table" = slide.freq.table, "translation" = translation.table)
    class(output) <- "rwty.slide"
    
    return(output)
}

   