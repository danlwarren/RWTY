#' Calculate discordance metric
#' 
# This function calculates a discordance metric, which is just the average 
# of the absolute values of the differences in posteriors
# for each clade across a pair of chains.  Ranges from 0,
# where posteriors are identical, to 1, where they are
# as different as can be. Returns a dist class object of discordance values

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
#' discordance.mat(myfreqTable)

discordance.mat <- function(freqTable){
  N <- ncol(freqTable)-1
  d <- matrix(nrow=N, ncol=N)
  for(i in 1:N){
    for(j in i+1:N){
      if(j <= N){d[j,i] <- mean(abs(freqTable[,i+1] - freqTable[,j+1]))}
    }
  }
  colnames(d) <- names(freqTable)[2:(N+1)]
  rownames(d) <- names(freqTable)[2:(N+1)]
  as.dist(d)
}