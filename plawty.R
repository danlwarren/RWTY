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

}