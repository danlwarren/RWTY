cumulative.freq <- function(tree.list, burnin=0, window.size, gens.per.tree = 1, slide.freq.table = NULL, ...){ 

    # NB if you pass in a slide.freq.table, all the other stats are ignored.
    # TODO: need to make this clear to users.

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
    cum.freq.table$mean <- thismean

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