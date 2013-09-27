continuous.ess <- function(tree.list, burnin=0, N.thinning=20, N.sample=100){
	# Estimate ESS for a list of trees at various subsamplings
	tree.list <- tree.list[(burnin + 1):(length(tree.list))]

	# this ensures that we can tell you if your ESS is <200
	max.thinning <- as.integer(length(tree.list)/200)

	# this ensures that all estimates are taken with equal sample sizes
	if(N.sample > as.integer(length(tree.list)/max.thinning))
		N.sample <- as.integer(length(tree.list/max.thinning))

	# we analyse up to 20 thinnings spread evenly, less if there are non-unique numbers
	thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=N.thinning)))

	# first we get the reference set of distances from a shuffled list
	tree.list.shuffled <- sample(tree.list, length(tree.list), replace=FALSE)
	samples = as.integer(length(tree.list)/2)
	print(paste("Calculating reference with", samples, "samples"))
	d.reference <- get.sequential.distances.c(tree.list.shuffled, 1, samples)
	d.reference.average <- median(d.reference)
	d.reference.boot <- apply(matrix(sample(d.reference,rep=TRUE,10^3*length(d.reference)),nrow=10^3),1,median)
	d.reference.lowerCI <- quantile(d.reference.boot,c(0.05,0.95))[1]

	r <- data.frame()
	for(gapsize in thinnings) {
		print(paste("Working on thinning ", gapsize))
		d <- get.sequential.distances.c(tree.list, gapsize, N.sample)
		e <- c(median(d), gapsize)
		r <- rbind(r, e)
	}

	colnames(r) <- c('average.distance', 'gapsize')
	r$lower <- as.logical(r$average.distance < d.reference.lowerCI)

	# calculate the approximate ESS
	if(FALSE %in% r$lower){
		min.gap <- r$gapsize[min(which(r$lower==FALSE))]
		approx.ESS <- length(tree.list) / min.gap
	}
	else{
		# we know that the ESS is less than that produced by the biggest gap size
		approx.ESS <- length(tree.list) / (max(r$gapsize))
		approx.ESS <- paste("<", approx.ESS)
	}


	r <- list('tree.distances'=r, 
			  'approx.ESS' = approx.ESS,
			  'reference.distance' = d.reference.average,
			  'reference.distance.lowerCI' = d.reference.lowerCI
			  )

	p <- plot.tree.ess(r)

	r$plot <- p

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

	distances <- lapply(tree.pairs, continuous.distance)
	distances <- as.numeric(unlist(distances))
	distances
}

