#' Cumulative means of clade split frequencies.
#' 
#' This function calculates the cumulative mean split frequencies of clades as an MCMC progresses.
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin. Defaults to zero. 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#'
#' @return A list of rwty.cumulative objects, one per chain in the input list of chains.
#' Each rwty.cumulative object contains the cumulative mean split frequencies of clades at sp
#' windows, and a translation table that converts clade groupings to factors.
#'
#' @keywords MCMC, split frequency, convergence
#'
#' @export cumulative.freq
#' @examples
#' \dontrun{
#' data(fungus)
#' cumulative.data <- cumulative.freq(fungus, burnin=20)
#' }

cumulative.freq <- function(chains, burnin=0, window.size = 20){ 

    chains = check.chains(chains)
    trees = lapply(chains, function(x) x[['trees']])

    if((length(trees[[1]]) - burnin) < 2 * window.size ){
        stop("ERROR: burnin is too large to make at least two points for the plot, quitting. Try setting a smaller burnin and/or a smaller window size")
    }

    cum.freq.list = lapply(trees, get.cum.freq.table, burnin = burnin, window.size = window.size, gens.per.tree = chains[[1]]$gens.per.tree)
    return(cum.freq.list)

}


get.cum.freq.table <- function(tree.list, burnin = 0, window.size = 20, gens.per.tree = 1){

    # we start with a slide frequency table, and then just calculate cumulative means from that    
    slide.freq.table = get.slide.freq.table(tree.list, burnin, window.size, gens.per.tree)
    
    # Peel the translation table off for later use
    translation.table <- slide.freq.table$translation
    
    # Strip the frequency data out
    slide.freq.table <- slide.freq.table$slide.table
    
    # remove SD and mean from sliding window data
    slide.freq.table <- slide.freq.table[,!(names(slide.freq.table) %in% c("sd", "mean", "ess", "wcsf"))]
    
    #Get cumulative means for each row
    cum.freq.table <- apply(slide.freq.table, 1, cummean)

    cum.freq.table <- as.data.frame(t(cum.freq.table))

    colnames(cum.freq.table) <- colnames(slide.freq.table)

    # calculate sd and mean of cumulative frequency and mean
    thissd <- apply(cum.freq.table, 1, sd)
    thismean <- apply(cum.freq.table, 1, mean) 
    thiswcsf <- get.wcsf(cum.freq.table)

    cum.freq.table$sd <- thissd
    cum.freq.table$mean <- thismean
    cum.freq.table$wcsf <- thiswcsf

    cum.freq.table <- cum.freq.table[order(cum.freq.table$sd, decreasing=TRUE),]
    
    rownames(cum.freq.table) <- as.numeric(as.factor(rownames(cum.freq.table)))
    
    output <- list("cumulative.table" = cum.freq.table, "translation" = translation.table)
    class(output) <- "rwty.cumulative"
    output
}

cummean <- function(x){
  r <- (cumsum(as.numeric(x)))/seq(1:length(x))
  r
}

get.wcsf <- function(dat){

    d <- t(apply(dat, 1, function(z) abs(diff(as.numeric(z)))))

    # multiply all columns by a weighted linear distribution
    N = ncol(d)
    w = 1:N/N
    d <- (t(t(d) * w))

    return(rowSums(d))

}

