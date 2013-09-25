continuous.ess <- function(tree.list, burnin=0, N=20){
	# Estimate ESS for a list of trees at various subsamplings
	tree.list <- tree.list[(burnin + 1):(length(tree.list))]

	# this ensures that we can tell you if your ESS is <200
	max.thinning <- as.integer(length(tree.list)/200)

	# we analyse up to 20 thinnings spread evenly, less if there are non-unique numbers
	thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=N)))

	# first we get the reference set of distances from a shuffled list
	tree.list.shuffled <- sample(tree.list, length(tree.list), replace=FALSE)
	samples = as.integer(length(tree.list)/2)
	print(paste("Calculating reference with", samples, "samples"))
	d.reference <- get.sequential.distances.c(tree.list.shuffled, 1, samples)
	d.reference.average <- median(d.reference)

	r <- data.frame()
	for(gapsize in thinnings) {
		print(paste("Working on thinning ", gapsize))
		d <- get.sequential.distances.c(tree.list, gapsize, 500)
		d.average <- median(d)

		p <- NA
		sig <- d.average<d.reference.average

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


continuous.distance <- function(two.trees){
	# type = 1: RF distance
	# type = 2: branch.score.difference
	# type = 3: path difference
	# type = 4: weighted path difference
	d <- abs(two.trees[1] - two.trees[2])	
	d
}

get.sequential.distances.c <- function(tree.list, thinning = 1, N=100){

	# now thin out the input list
	keep <- seq(from=1, to=length(tree.list), by=thinning)

	# first we cut off any trailing trees from the input list
	if(length(keep)%%2 != 0) keep <- keep[1:(length(keep)-1)]

	# we only want to look at 100 samples, for efficiency
	odds <- seq(from=1, to=length(keep), by=2) #indices of the tree1 trees in keep
	if((length(odds))>N){
		odds <- sample(odds[1:(length(odds))], N, replace=FALSE)
		evens <- odds + 1 # indices of the tree2 trees in keep
		indices <- sort(c(odds, evens))
		keep <- keep[indices]
	}

	tree.list <- tree.list[keep]
	tree.index <- seq_along(tree.list)

	# turn the tree list into a list of sequential pairs
	# e.g. c(a, b, c, d, e) -> c(a,b), c(c,d)
	tree.pairs <- split(tree.list, ceiling(tree.index/2))

	print(length(tree.pairs))

	distances <- lapply(tree.pairs, continuous.distance)
	distances <- as.numeric(unlist(distances))
	distances
}

