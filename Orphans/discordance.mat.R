#' Calculate discordance metric
#' 
# This function calculates a discordance metric, which is just the average 
# of the absolute values of the differences in posteriors
# for each clade across a pair of chains.  Ranges from 0,
# where posteriors are identical, to 1, where they are
# as different as can be. 

#' @param x A table of clade frequencies.
#' 
#' @return output A dist class object of discordance values
#'
#' @keywords MCMC, phylogenetics, discordance
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
  output <- as.dist(d)
  output
}