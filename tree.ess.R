tree.ess <- function(tree.list, burnin=0){
	# Estimate ESS for a list of trees at various subsamplings
	tree.list <- tree.list[(burnin + 1):(length(tree.list))]

	# this ensures that we can tell you if your ESS is <100
	max.thinning <- as.integer(length(tree.list)/100)

	# we analyse up to 20 thinnings spread evenly, less if there are non-unique numbers
	thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=20)))

	# first we get the reference set of distances from a shuffled list
	print("calculating reference")
	tree.list.shuffled <- sample(tree.list, length(tree.list), replace=FALSE)
	d.reference <- get.sequential.distances(tree.list.shuffled, 1)
	d.reference.average <- median(d.reference)

	r <- data.frame()
	for(gapsize in thinnings) {
		print(paste("Working on thinning ", gapsize))
		d <- get.sequential.distances(tree.list, gapsize)
		d.average <- median(d)

		p <- wilcox.test(d.reference, d, exact=FALSE, alternative="greater")$p.value
		sig <- p<0.05

		e <- c(d.average, gapsize, p, sig)
		r <- rbind(r, e)
	}

	colnames(r) <- c('average.distance', 'gapsize', 'p.value', 'significant.difference')
	r$significant.difference <- as.logical(r$significant.difference)

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

	p <- plot.tree.ess(r, d.reference.average)

	r <- list('tree.distances'=r, 
			  'approx.ESS' = approx.ESS,
			  'reference.distance' = d.reference.average,
			  'plot'=p
			  )

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

	# now thin out the input list
	keep <- seq(from=1, to=length(tree.list), by=thinning)

	# first we cut off any trailing trees from the input list
	if(length(keep)%%2 != 0) keep <- keep[1:(length(keep)-1)]

	# we only want to look at 100 samples, for efficiency
	odds <- seq(from=1, to=length(keep), by=2) #indices of the tree1 trees in keep
	if((length(odds))>100){
		odds <- sample(odds[1:(length(odds))], 100, replace=FALSE)
		evens <- odds + 1 # indices of the tree2 trees in keep
		indices <- sort(c(odds, evens))
		keep <- keep[indices]
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


p.from.ranked.list <- function(null.dist, obs, type='smaller'){

	null.dist <- sort(null.dist)

	if(obs %in% null.dist){
		pos <- mean(which(null.dist==obs))
	} 
	else if(obs<min(null.dist)){
		pos <- 1
	}
	else if(obs>max(null.dist)){
		pos <- length(null.dist)+1
	}
	else{
		pos <- min(which(null.dist>obs))

	}

	if(type=='greater'){
		pos <- length(null.dist)+1 - pos
	}

	p <- pos/(length(null.dist)+1)
	p
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
