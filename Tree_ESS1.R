library(ape)
library(phangorn)

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

burnin <- 1000 #the number of trees from the start of a bayesian run to chuck out
all.trees <- read.nexus("~/Desktop/lithomodaexons.nex.run1.t")
trees.to.use <- seq(from=(burnin+1), to=length(all.trees), by=1)
useful.trees <- all.trees[trees.to.use]

# ensures that we have at least 20 samples from this...
N <- 100

#### Tree distance betwen random trees ####
r <- sample(useful.trees, length(useful.trees))
D <- pairwise.RF(r, N)

raw.data = c()
results = data.frame()

thinnings <- seq(from=1, to=as.integer(length(useful.trees)/N)-1, by=20)


for(thinning in thinnings){
	print("***********************")
	print(thinning)
	trees.to.use <- seq(from=1, to=length(all.trees), by=thinning)
	t <- all.trees[trees.to.use]
	RFs <- pairwise.RF(t, N)
	raw.data <- c(raw.data, list(RFs))
	results <- rbind(results, c(mean(RFs), var(RFs)))
	}

names(results) <- c('mean', 'var')
results$thinnings <- thinnings

plot(results$thinnings, results$mean)
abline(a=mean(D), b=0)