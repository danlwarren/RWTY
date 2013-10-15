#' Old style plotting of cumulative and slide objects
#' 
#' Takes a table from cumulative.freq or slide.freq as input.  
#' Numclades gives the number of clades to plot, starting from the
#' top.  Since cumulative.freq and slide.freq both sort by sd these
#' will by default be the most variable clades.
#'
#' @param x An rwty.slide or rwty.cumulative object
#' @param numclades The number of clades to plot.  The clades with the highest sd in clade frequency are plotted first, so numclades = 10 will be the 10 most variable clades in the chain.
#'
#' @return thisplot Returns a ggplot object.
#'
#' @keywords cumulative, sliding window, mcmc, phylogenetics, plot
#'
#' @export
#' 
#' @examples
#' plawty(slideresults, numclades=25)



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


















