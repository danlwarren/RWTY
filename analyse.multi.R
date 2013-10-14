analyse.multi <- function(chains, burnin, window.size, gens.per.tree, step=1, labels=NA, ...){
    
    output <- list()
    
    # Name chains by list order if labels aren't supplied
    if(any(is.na(labels))){labels <- seq(1, length(chains))}
    
    # Run analyse single on each chain
    for(i in 1: length(chains)){
        output[[labels[i]]] <- c(output, analyse.single(chains[[i]], burnin, window.size, gens.per.tree, step, ... ))
    }
    
    output[["compare.n"]] <- compare.n(chains, setnames=labels, burnin)
    
    output
}