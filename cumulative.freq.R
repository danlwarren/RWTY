cumulative.freq <- function(tree.list, burnin=0, window.size, gens.per.tree = 1, slide.freq.table = NA, ...){ 

    if(is.na(slide.freq.table)){
        slide.freq.table = slide.freq(tree.list, burnin, window.size, gens.per.tree, ...) 
    }
    
    slide.freq.table <- slide.freq.table[,!(names(slide.freq.table) %in% c("sd", "mean"))]
    
    cum.freq.table <- apply(slide.freq.table, 1, cummean)

    cum.freq.table <- as.data.frame(t(cum.freq.table))

    colnames(cum.freq.table) <- colnames(slide.freq.table)

    # calculate sd and mean of cumulative frequency and mean
    thissd <- apply(cum.freq.table, 1, sd)
    cum.freq.table$sd <- thissd

    thismean <- apply(cum.freq.table, 1, mean) 
    cum.freq.table$mean <- thismean

    cum.freq.table <- cum.freq.table[order(cum.freq.table$sd, decreasing=TRUE),]

    cum.freq.table
}

cummean <- function(x){
    r <- (cumsum(as.numeric(x)))/seq(1:length(x))
    r
}