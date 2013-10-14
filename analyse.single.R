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