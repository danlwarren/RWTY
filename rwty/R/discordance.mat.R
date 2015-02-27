#' Calculate discordance metric
#' 
# This function calculates a discordance metric, which is just the average 
# of the absolute values of the differences in posteriors
# for each clade across a pair of chains.  Ranges from 0,
# where posteriors are identical, to 1, where they are
# as different as can be. 

#' @param x A table of clade frequencies.
#' @param min.freq The minimum frequency for a node to be used for calculating discordance  \code{min.freq}
#' 
#' @return output A dist class object of discordance values
#'
#' @keywords MCMC, phylogenetics, discordance
#'
#' @export
#' 
#' @examples
#' discordance.mat(myfreqTable)

discordance.mat <- function(freqTable, min.freq = 0){
  N <- ncol(freqTable)-1
  freqTable <- freqTable[apply(freqTable, MARGIN = 1, function(x) all(x > min.freq)), ]
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