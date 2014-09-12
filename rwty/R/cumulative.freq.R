#' Cumulative frequencies of clades in an MCMC chain
#' 
#' Calculates the posterior probability estimate of each clade in an MCMC chain
#' as a function of chain length.
#'
#' @param tree.list A single rwty.trees or multiPhylo object. \code{chains}
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#' @param window.size The length of window (in trees) for the sliding window plot \code{window.size}
#' @param gens.per.tree The number of generations per tree in the .t file.
#' @param slide.freq.table A table from slide.freq.  If a table is not provided, it will be built.
#
#' @return output A list containing a table of cumulative frequencies and a clade translation table
#'
#' @keywords MCMC, phylogenetics, posterior probabilities, convergence
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' cumulative.data <- cumulative.freq(run1$trees, burnin=100, window.size=20, gens.per.tree=10000)

cumulative.freq <- function(tree.list, burnin=0, window.size, gens.per.tree = 1, slide.freq.table = NULL, ...){ 

    # NB if you pass in a slide.freq.table, all the other stats are ignored.
    # TODO: need to make this clear to users.
    
    # Peel just the trees off of rwty.trees object
    # so the function can take rwty.trees or multiPhylo
    if(class(tree.list) == "rwty.trees"){tree.list <- tree.list$trees}

    if(is.null(slide.freq.table)){
        slide.freq.table = slide.freq(tree.list, burnin, window.size, gens.per.tree, ...) 
    }
    
    # Peel the translation table off for later use
    translation.table <- slide.freq.table$translation
    
    # Strip the frequency data out
    slide.freq.table <- slide.freq.table$slide.table
    
    slide.freq.table <- slide.freq.table[,!(names(slide.freq.table) %in% c("sd", "mean"))]
    
    #Get cumulative means for each row
    cum.freq.table <- apply(slide.freq.table, 1, cummean)

    cum.freq.table <- as.data.frame(t(cum.freq.table))

    colnames(cum.freq.table) <- colnames(slide.freq.table)

    # calculate sd and mean of cumulative frequency and mean
    thissd <- apply(cum.freq.table, 1, sd)
    cum.freq.table$sd <- thissd

    thismean <- apply(cum.freq.table, 1, mean) 
    cum.freq.table$sd <- thissd
    cum.freq.table$mean <- thismean

    cum.freq.table <- cum.freq.table[order(cum.freq.table$sd, decreasing=TRUE),]
    
    rownames(cum.freq.table) <- as.numeric(as.factor(rownames(cum.freq.table)))
    
    output <- list("cumulative.table" = cum.freq.table, "translation" = translation.table)
    class(output) <- "rwty.cumulative"
    output
}

