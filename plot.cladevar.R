

plot.cladevar <- function(input.table, numclades=nrow(input.table)){ 
    # plot variation in clade frequencies between windows
    
    x <- input.table[1:numclades,2:length(input.table) - 2] #Stripping off mean and SD
    
    # this is a df of absolute differences in pp variation between windows
    d <- t(apply(x, 1, abs.diffs))
    d <- as.data.frame(d)
    colnames(d) <- colnames(x)[2:ncol(x)]
    d$clade <- rownames(d)
    d <- melt(d, id.vars="clade")
    colnames(d) <- c("Clade", "Generations", "Variation.in.posterior.probability")
    d$Clade <- as.factor(d$Clade)
    
    thisplot <- ggplot(data=d, aes(x=as.numeric(Generations), y=Variation.in.posterior.probability, group=Generations)) + 
        geom_boxplot() +
        theme(legend.position="none") +
        xlab("Generations")
    
    thisplot
}