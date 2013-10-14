tree.distance <- function(two.trees){
    # type = 1: RF distance
    # type = 2: branch.score.difference
    # type = 3: path difference
    # type = 4: weighted path difference
    type = 1
    d <- treedist(two.trees[[1]], two.trees[[2]])[[type]]	
    d
}