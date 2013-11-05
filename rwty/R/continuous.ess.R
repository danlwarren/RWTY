#' A one sentence description of what your function does
#' 
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces. 
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param burnin The number of trees to eliminate as burnin \code{burnin}
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

continuous.ess <- function(tree.list, burnin=0, N.thinning=20, N.sample=100){

	# Estimate ESS for a list of trees at various subsamplings
	tree.list <- tree.list[(burnin + 1):(length(tree.list))]

	# this ensures that we can tell you if your ESS is <200
	max.thinning <- as.integer(length(tree.list)/200)
	

	# this ensures that all estimates are taken with equal sample sizes
	if(N.sample > as.integer(length(tree.list)/max.thinning))
		N.sample <- as.integer(length(tree.list)/max.thinning)
		
	print(paste("This analysis will use ", N.sample, "samples per gap size"))

	# we analyze up to N.thinning thinnings spread evenly, less if there are non-unique numbers
	thinnings <- unique(as.integer(seq(from = 1, to = max.thinning, length.out=N.thinning)))

	# first we get the reference set of distances from a shuffled list
	tree.list.shuffled <- sample(tree.list, length(tree.list), replace=FALSE)
	samples = as.integer(length(tree.list)/2)

	print(paste("Calculating reference with", samples, "samples"))
	d.reference <- get.sequential.distances.c(tree.list.shuffled, 1, samples)

	d.reference.average <- median(d.reference)
	d.reference.boot <- apply(matrix(sample(d.reference,rep=TRUE,10^3*length(d.reference)),nrow=10^3),1,median)
	#d.reference.lowerCI <- d.reference.average
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




