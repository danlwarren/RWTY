# Takes a table from cumulative.freq or slide.freq as input.  
# Numclades gives the number of clades to plot, starting from the
# top.  Since cumulative.freq and slide.freq both sort by sd these
# will by default be the most variable clades.

plawty <- function(x, numclades=10){ 
  x <- x[1:numclades,2:length(x) - 2] #Stripping off mean and SD
  y <- as.numeric(colnames(x)[2:length(x)])
  thismax <- max(y)
  for(i in 1:length(x[,1])){
    y <- cbind(y, as.numeric(x[i,2:length(x[1,])]))
  }

  colnames(y) <- c("gen", as.character(x[,1]))
  y <- as.data.frame(y)
  y <- melt(y, id="gen")

  thisplot <- ggplot(data=y, aes(x=gen, y=value, colour=variable)) + 
    ylab("Posterior Probability") + xlab("Generations") + geom_line() + xlim(0, thismax) +
    ylim(0,1) + theme_bw() +  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank()) + theme(legend.position="none", 
    panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
    axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

  thisplot
}

plot.cladeprobs <- function(input.table, numclades=nrow(input.table)){ 
    # clade probability plot over generations

    # TODO should put a check in here to make sure it only accepts slidetest or cumtest objects

    x <- input.table[1:numclades,2:length(input.table) - 2] #Stripping off mean and SD
    x$clade <- rownames(x)
    x <- melt(x, id.vars="clade")
    colnames(x) <- c("Clade", "Generations", "Posterior.Probability")
    x$Clade <- as.factor(x$Clade)

    thisplot <- ggplot(data=x, aes(x=Generations, y=Posterior.Probability, group=Clade, color=Clade)) + 
      geom_line() +
      theme(legend.position="none")

  thisplot
}

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

    thisplot <- ggplot(data=d, aes(x=Generations, y=Variation.in.posterior.probability, group=Generations)) + 
      geom_boxplot() +
      theme(legend.position="none")

  thisplot
}

abs.diffs <- function(x){
    d <- abs(diff(as.numeric(x)))
    d
}


plot.tree.ess <- function(tree.ess.table){


    t <- ggplot(data=tree.ess.table, aes(x=gapsize, y=average.distance)) + 
         geom_point(size = 2, aes(color=significant.difference)) + 
         geom_smooth(size=0, method="loess") +
         theme(legend.position="none")


    t

}

plot.mds.treespace <- function(points, dim){
    # This fundtion will take an mds.treespace object and produce plots 
    # of chains in treespace
    
    p <- ggplot(data=points, aes(x=x,y=y,fill=LnL)) + 
          geom_path(alpha=0.2, linetype='dashed') + 
          geom_point(shape=21, size=7, alpha=0.85, colour='white') + 
          scale_fill_gradient() +
          geom_point(data=points, aes(x=x, y=y, colour=mcmc.sample), size=3)  + 
          scale_colour_gradient(low='red', high='white')
    p

}




    p <- ggplot(data=points) + geom_point(aes(x=x, y=y, colour=mcmc.sample))  + scale_colour_gradient()

















