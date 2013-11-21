#' Estimating effective sample size for tree topology
#' 
#' This function estimates the effective sample size for trees in a 
#' MCMC chain.  It operates by looking at RF distances for trees spaced at
#' different intervals on the chain, and determining what spacing is needed so that
#' adjacent trees are no more correlated than randomly chosen pairs from the whole
#' chain.
#'
#' @param tree.list A multiPhylo object \code{tree.list}
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#' @param N the number of windows to attempt
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords ess, effective sample size, autocorrelation, phylogeny
#'
#' @export
#' 
#' @examples
#' tree.ess(mytrees, burnin=100, N=20)

tree.ess <- function(tree.list, burnin=0, N.thinning=20, N.sample=100){

	# Estimate ESS for a list of trees at various subsamplings
	tree.list <- tree.list[(burnin + 1):(length(tree.list))]

	# this ensures that we can tell you if your ESS is <200
	max.thinning <- as.integer(length(tree.list)/200)
	

	# this ensures that all estimates are taken with equal sample sizes
	if(N.sample > as.integer(length(tree.list)/max.thinning))
		N.sample <- as.integer(length(tree.list)/max.thinning)
		
	print(paste("This tree ESS analysis will use ", N.sample, "samples per gap size"))

	# we analyze up to N.thinning thinnings spread evenly, less if there are non-unique numbers
	thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=N.thinning)))

	# first we get the reference set of distances from a shuffled list with at most 1000 samples
	tree.list.shuffled <- sample(tree.list, length(tree.list), replace=FALSE)
	samples = as.integer(length(tree.list)/2)
	if(samples>2000) samples <- 2000
	print(paste("Calculating reference with", samples, "samples"))
	d.reference <- get.sequential.distance(tree.list.shuffled, 1, samples)

	d.reference.average <- median(d.reference)
	d.reference.boot <- apply(matrix(sample(d.reference,rep=TRUE,10^3*length(d.reference)),nrow=10^3),1,median)
	#d.reference.lowerCI <- d.reference.average
	d.reference.lowerCI <- quantile(d.reference.boot,c(0.05,0.95))[1]

	r <- data.frame()
	for(gapsize in thinnings) {
		print(paste("Working on gap size", gapsize))
		d <- get.sequential.distance(tree.list, gapsize, N.sample)
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



tree.ess.old <- function(tree.list, burnin=0, N=20){
	# Estimate ESS for a list of trees at various subsamplings
	tree.list <- tree.list[(burnin + 1):(length(tree.list))]

	# this ensures that we can tell you if your ESS is <100
	max.thinning <- as.integer(length(tree.list)/100)

	# we analyze up to 20 thinnings spread evenly, less if there are non-unique numbers
	thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=N)))

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

	p <- plot.tree.ess(r)

	r <- list('tree.distances'=r, 
			  'approx.ESS' = approx.ESS,
			  'reference.distance' = d.reference.average,
			  'plot'=p
			  )

	r
}









