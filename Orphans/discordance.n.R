#' Compares posterior probability estimates from multiple MCMC chains.
#' 
#' This function takes a set of tree files, an option array of names for those
#' files, and a burnin argument.  It returns a plot of the change in discordance
#'
#' @param x A list of rwty.trees objects.
#' @param setnames A list of names for the chains.
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#'
#' @return output A list containing a table of frequencies of each clade in each chain along with mean and sd, a distance matrix measuring consensus between chains, a translation table, and a ggpairs plot.
#'
#' @keywords MCMC, phylogenetics, discordance, sliding window
#'
#' @export
#' 
#' @examples
#' discordance.n(mytrees, setnames=c("Chain 1", "Chain 2", "Chain 3"), burnin=100)


# Calculate a discordance metric, which is just the average 
# of the absolute values of the differences in posteriors
# for each clade across a pair of chains.  Ranges from 0,
# where posteriors are identical, to 1, where they are
# as different as can be.


discordance.n <- function(x, setnames=NA, burnin){ # In this case x is a list of rwty.trees objects

  
  
  
  
  
  
  
  
  d <- vector(length=ncol(slide.freq.table)-3)
for(i in 1:length(d)){
  d[i] <- mean(abs(slide.freq.table[,i] - slide.freq.table[,i+1]))
}


x <- as.numeric(as.character(names(clade.freq.list)[2:length(clade.freq.list)]))
df <- data.frame(cbind(x,d))


#plot(d~names(clade.freq.list)[2:length(clade.freq.list)], ylim=c(0,1), ylab="Discordance", xlab="Generation")


plot <- ggplot(df, aes(x = x, y = d, ymin=0, ymax=1)) + geom_line() + geom_line(data = df, aes(y = d)) + xlab("Generation") + ylab("Discordance")
