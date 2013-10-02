# This function will hopefully be the primary user interface.  
# It will take one or more tree files, a few necessary arguments,
# and then automatically run all of the relevant analyses and 
# generate the plots.

analyse.rwty <- function(chains, burnin, window.size, gens.per.tree, step=1, ...){
    
    # If a single rwty.trees object is passed, it goes to the analyse.single
    # function.  Otherwise it assumes that multiple rwty.trees objects
    # have been passed as a list.
    if(class(chains) == "rwty.trees"){
        print("Analyzing single chain...")
        analyse.single(chains, burnin, window.size,
                       gens.per.tree, step, ...)
    }
    
    else{
        print("Analyzing multiple chains...")
        analyse.multi(chains, burnin, window.size, gens.per.tree, step, ...)
    }
}




analyse.single <- function(chains, burnin, window.size, gens.per.tree, step=1, ...){
    
    lnl.plot <- NA
    if(exists("chains$ptable")){
        print("Making LnL plot...")
        lnl.plot <- ggplot(chains$ptable[burnin:length(chains$ptable[,1]),], aes(x=Gen, y=LnL)) + geom_line()
    }
    
    print("Sliding window analysis...")
    slide.data <- slide.freq(chains, burnin, window.size, gens.per.tree)
    slide.plot <- plot.cladeprobs(slide.data$slide.table, ...)
    
    print("Cumulative analysis...")
    cumulative.data <- cumulative.freq(chains, burnin, window.size, gens.per.tree,
                                       slide.freq.table=slide.data)
    cumulative.plot <- plot.cladeprobs(cumulative.data$cumulative.table, ...)
    
    print("Plotting trees in tree space...")
    print(step)
    mdstrees <- chains$trees[seq((burnin + 1), length(chains$trees), by = step)]
    treespace <- treespace.single(mdstrees)
    treespace.data <- treespace$mds
    treespace.plot <- treespace$plot
    
    output <- list("LnL.plot" = lnl.plot, "slide.data" = slide.data,
                   "slide.plot" = slide.plot, "cumulative.data" = cumulative.data,
                   "cumulative.plot" = cumulative.plot, "treespace.data" = treespace.data,
                   "treespace.plot" = treespace.plot)
}

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