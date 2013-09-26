# This function will hopefully be the primary user interface.  
# It will take one or more tree files, a few necessary arguments,
# and then automatically run all of the relevant analyses and 
# generate the plots.

analyse.rwty <- function(chains, burnin, window.size, gens.per.tree, step=1, ...){
    
    # If a single rwty.trees object is passed, it goes to the analyse.single
    # function.  Otherwise it assumes that multiple rwty.trees objects
    # have been passed as a list.
    if(class(chains) == "rwty.trees"){
        analyse.single(chains, burnin, window.size,
                       gens.per.tree, step, ...)
    }
    
    else{
        analyse.multi(chains, burnin, gens.per.tree, step, ...)
    }
}




analyse.single <- function(chains, burnin, window.size, gens.per.tree, step=1, ...){
    print("Analyzing single chain...")
    
    lnl.plot <- NA
    if(!any(is.na(chains$ptable))){
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
    mdstrees <- chains$trees[seq((burnin + 1), length(chains$trees), by = step)]
    treespace <- treespace.single(mdstrees)
    treespace.data <- treespace$mds
    treespace.plot <- treespace$plot
    
    output <- list("LnL.plot" = lnl.plot, "slide.data" = slide.data,
                   "slide.plot" = slide.plot, "cumulative.data" = cumulative.data,
                   "cumulative.plot" = cumulative.plot, "treespace.data" = treespace.data,
                   "treespace.plot" = treespace.plot)
}

analyse.multi <- function(chains, burnin, gens.per.tree){
    print("Analyzing multiple chains...")
}