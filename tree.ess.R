tree.ess <- function(tree.list){
	# Estimate ESS for a list of trees

	# 1. make a list of thinned out tree lists
	# we define a set of thinning regimes
	# 0 is no sub sampling
	# max the biggest we can do to still get 10 samples
	thinnings <- seq(1:as.integer(length(tree.list)/21))
	r <- data.frame()

	for(jump in thinnings) {
		print(paste("Working on gapsize ", jump))

		d <- get.sequential.distances(tree.list, jump)

		r <- rbind(r, d)
	}

	r

}


tree.distance <- function(two.trees){
	# type = 1: RF distance
	# type = 2: branch.score.difference
	# type = 3: path difference
	# type = 4: weighted path difference
	type = 3
	print(two.trees)
	d <- treedist(two.trees[[1]], two.trees[[2]])[[type]]	
	d
}

get.sequential.distances <- function(tree.list, thinning=1){

	# return two things: the average distance between sequential pairs
	# 				   : the average distance between the same number of random pairs
	#

	# first make a shuffled list of trees
	tree.list.s <- sample(tree.list, length(tree.list), replace=FALSE)

	# now thin out both lists
	keep <- seq(from=1, to=length(tree.list), by=thinning)
	tree.list.original <- tree.list[keep]
	tree.list.shuffled <- tree.list.s[keep]
	tree.index <- seq_along(tree.list.original)

	# turn the tree list into a list of sequential pairs
	# e.g. c(a, b, c, d, e) -> c(a,b), c(c,d)
	tree.pairs <- split(tree.list, ceiling(tree.index/2))
	tree.pairs <- tree.pairs[1:(length(tree.pairs)-1)] # cut the last pair which is often incomplete

	distances.original <- lapply(tree.pairs, tree.distance)
	distances.original <- as.numeric(unlist(distances.original))

	tree.pairs.shuffled <- split(tree.list.shuffled, ceiling(tree.index/2))
	tree.pairs.shuffled <- tree.pairs.shuffled[1:(length(tree.pairs.shuffled)-1)] # cut the last pair which is often incomplete


	distances.shuffled <- lapply(tree.pairs.shuffled, tree.distance)
	distances.shuffled <- as.numeric(unlist(distances.shuffled))

	r <- data.frame("original" = mean(distances.original), "shuffled" = mean(distances.shuffled), "thinning" = thinning)

	#r <- data.frame('original'=distances.original, 'shuffled'=distances.shuffled)

	#r <- melt(r, value.name="tree.dist",  id=c())
	#r$thinning <- thinning

	#r <- melt(r, value.name="tree.dist",  id=c("thinning"))
	
	r
}


pairwise.RF <- function(trees, n=length(trees)-1){
	#input a list of trees, get back a list of sequential RF distances
	#n is the number of comparisons you want to make. Useful for making sure you count the same number of instances.
	#the default is to look at all pairs of trees in the list

	RFs <- c()	
	for(tree.index in 1:(n)){
		print(tree.index)
		tmp <- RF.dist(trees[[tree.index]], trees[[tree.index+1]])
		RFs <- c(RFs, tmp)
	}	
	
	return(RFs)
}
