tree.ess <- function(tree.list){
	# Estimate ESS for a list of trees at various subsamplings

	# this ensures that there are at least 100 samples in each analysis, which
	# 1. Ensures that all sample sizes are equal (since we reduce >100 to 100)
	# 2. Means we have a minumum detection of approx ESS<100, which is fine.
	max.thinning <- as.integer(length(tree.list)/100)

	# we analyse up to 20 thinnings spread evenly, less if there are non-unique numbers
	thinnings <- unique(as.integer(seq(from = 0, to = max.thinning, length.out=20)))

	# first make a shuffled list of trees and get the baseline estimate
	tree.list.shuffled <- sample(tree.list, length(tree.list), replace=FALSE)

	print("Estimating uncorrelated tree distance")
	distances.shuffled <- get.sequential.distances(tree.list.shuffled)
	mean.uncorrelated <- mean(distances.shuffled)

	r <- data.frame()
	for(gapsize in thinnings) {
		print(paste("Working on gapsize ", gapsize))
		d <- get.sequential.distances(tree.list, gapsize)
		# use a two-sample Kolmogorov–Smirnov test to determine if
		# the uncorrelated distances are greater than the observed 
		p <- wilcox.test(distances.shuffled, d, exact=FALSE, alternative="greater")$p.value
		sig <- as.logical(p<0.05)
		e <- c(mean(d), gapsize, p, sig)
		r <- rbind(r, e)
	}

	colnames(r) <- c('mean.distance', 'gapsize', 'p.value', 'significant.difference')

	# calculate the approximate ESS
	if(FALSE %in% r$significant.difference){
		min.gap <- r$gapsize[min(which(r$sig==FALSE))]
		approx.ESS <- length(tree.list) / min.gap
	}
	else{
		# we know that the ESS is less than that produced by the biggest gap size
		approx.ESS <- length(tree.list) / (max(r$gapsize))
		approx.ESS <- paste("<", approx.ESS)
	}

	r <- list('tree.distances'=r, 'uncorrelated.distance'=mean.uncorrelated, "approx.ESS" = approx.ESS)
	r
}


tree.distance <- function(two.trees){
	# type = 1: RF distance
	# type = 2: branch.score.difference
	# type = 3: path difference
	# type = 4: weighted path difference
	type = 1
	d <- treedist(two.trees[[1]], two.trees[[2]])[[type]]	
	d
}

get.sequential.distances <- function(tree.list, thinning = 1){

	if(thinning==0){ thinning = 1 }


	# now thin out the input list
	keep <- seq(from=1, to=length(tree.list), by=thinning)

	# first we cut off any trailing trees from the input list
	if(length(keep)%%2 != 0){
		keep <- keep[1:(length(keep)-1)]
	}

	# we only want to look at 100 samples, for efficiency
	odds <- seq(from=1, to=length(keep), by=2)
	if((length(odds))>100){
		odds <- sample(odds[1:(length(odds))], 100, replace=FALSE)
		evens <- odds + 1
		keep <- sort(c(odds, evens))
	}

	tree.list <- tree.list[keep]
	tree.index <- seq_along(tree.list)

	# turn the tree list into a list of sequential pairs
	# e.g. c(a, b, c, d, e) -> c(a,b), c(c,d)
	tree.pairs <- split(tree.list, ceiling(tree.index/2))

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
