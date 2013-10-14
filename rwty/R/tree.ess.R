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

tree.ess <- function(tree.list, burnin=0, N=20){
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

	p <- plot.tree.ess(r, d.reference.average)

	r <- list('tree.distances'=r, 
			  'approx.ESS' = approx.ESS,
			  'reference.distance' = d.reference.average,
			  'plot'=p
			  )

	r
}









