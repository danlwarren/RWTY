continuous.distance <- function(two.trees){
    # type = 1: RF distance
    # type = 2: branch.score.difference
    # type = 3: path difference
    # type = 4: weighted path difference
    d <- abs(two.trees[1] - two.trees[2])	
    d
}
