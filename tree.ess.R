tree.ess <- function(tree.list){
	# Estimate ESS for a list of trees at various subsamplings
	thinnings <- seq(from = 0, to = as.integer(length(tree.list)/21), by=10)

	# first make a shuffled list of trees and get the baseline estimate
	tree.list.shuffled <- sample(tree.list, length(tree.list), replace=FALSE)
	print("Estimating uncorrelated tree distance")
	distances.shuffled <- get.sequential.distances(tree.list.shuffled)
	mean.uncorrelated <- mean(distances.shuffled)

	r <- data.frame()
	for(gapsize in thinnings) {
		print(paste("Working on gapsize ", gapsize))
		d <- get.sequential.distances(tree.list, gapsize+1)
		# use a two-sample Kolmogorov–Smirnov test to determine if
		# the uncorrelated distances are greater than the observed 
		p <- wilcox.test(distances.shuffled, d, exact=FALSE, alternative="greater")$p.value
		e <- c(mean(d), gapsize, p)
		r <- rbind(r, e)
	}

	colnames(r) <- c('mean.distance', 'gapsize', 'p.value')

	r$sig <- r$p.value<0.05

	min.gap <- r$gapsize[min(which(r$p.value>0.05))]
	approx.ESS <- length(tree.list) / min.gap

	r <- list('tree.distances'=r, 'uncorrelated.distance'=mean.uncorrelated, "approx.ESS" = approx.ESS)
	r
}


tree.distance <- function(two.trees){
	# type = 1: RF distance
	# type = 2: branch.score.difference
	# type = 3: path difference
	# type = 4: weighted path difference
	type = 3
	d <- treedist(two.trees[[1]], two.trees[[2]])[[type]]	
	d
}

get.sequential.distances <- function(tree.list, thinning=1){

	# now thin out both lists
	keep <- seq(from=1, to=length(tree.list), by=thinning)
	
	evens <- seq(from=1, to=length(keep), by=2)

	print(paste("length of evens: ", length(evens)))

	# we only want to look at 100 samples, for efficiency
	if((length(evens)-1)>100){
		evens <- sample(evens[1:(length(evens)-1)], 100, replace=FALSE)
	}

	odds <- evens+1

	evens

	keep <- sort(c(odds, evens))

	print(paste("keep", keep))

	tree.list <- tree.list[keep]
	tree.index <- seq_along(tree.list)

	# turn the tree list into a list of sequential pairs
	# e.g. c(a, b, c, d, e) -> c(a,b), c(c,d)
	tree.pairs <- split(tree.list, ceiling(tree.index/2))
	tree.pairs <- tree.pairs[1:(length(tree.pairs)-1)] # cut the last pair which is often incomplete

	distances <- lapply(tree.pairs, tree.distance)
	distances <- as.numeric(unlist(distances))


	distances
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
