#' Various small utility functions
#' 
#' A bunch of tiny utility functions that we need off and on.  Not intended for user interaction.


continuous.distance <- function(two.trees){
    # type = 1: RF distance
    # type = 2: branch.score.difference
    # type = 3: path difference
    # type = 4: weighted path difference
    d <- abs(two.trees[1] - two.trees[2])	
    d
}

cummean <- function(x){
    r <- (cumsum(as.numeric(x)))/seq(1:length(x))
    r
}

abs.diffs <- function(x){
    d <- abs(diff(as.numeric(x)))
    d
}




