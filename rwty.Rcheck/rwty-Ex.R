pkgname <- "rwty"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rwty')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("analyze.multi")
### * analyze.multi

flush(stderr()); flush(stdout())

### Name: analyze.multi
### Title: Function for running rwty analyses on multiple chains.
### Aliases: analyze.multi
### Keywords: MCMC, awty, convergence, phylogenetics, plot, rwty

### ** Examples

data(fungus)
analyze.multi(list(run1, run2), burnin=100, window.size=20, treespace.points=100, filename="fungus.pdf", labels=c("Chain1", "Chain2"))



cleanEx()
nameEx("analyze.rwty")
### * analyze.rwty

flush(stderr()); flush(stdout())

### Name: analyze.rwty
### Title: Analyze.rwty, the main interface for rwty analyses and plots.
### Aliases: analyze.rwty
### Keywords: keywords

### ** Examples

data(fungus)
single <- analyze.rwty(run1, burnin=100, window.size=20, treespace.points=50, filename="Run1.pdf")
multi <- analyze.rwty(list(run1, run2), burnin=100, window.size=20, treespace.points=50, labels=c("Chain1", "Chain2"), filename="multi analysis.pdf")



cleanEx()
nameEx("analyze.single")
### * analyze.single

flush(stderr()); flush(stdout())

### Name: analyze.single
### Title: Function for running rwty analyses on single chains.
### Aliases: analyze.single
### Keywords: MCMC, awty, convergence, phylogenetics, plot, rwty

### ** Examples

data(fungus)
analyze.single(run1, burnin=100, window.size=20, treespace.points=100, filename="fungus.pdf", labels="Chain1")



cleanEx()
nameEx("clade.freq")
### * clade.freq

flush(stderr()); flush(stdout())

### Name: clade.freq
### Title: Returns clade names and frequencies
### Aliases: clade.freq
### Keywords: Clade consensus, frequencies, mcmc, phylogenetics

### ** Examples

data(fungus)
cfreq <- clade.freq(run1, start=10, end=100)



cleanEx()
nameEx("compare.n")
### * compare.n

flush(stderr()); flush(stdout())

### Name: compare.n
### Title: Compares posterior probability estimates from multiple MCMC
###   chains.
### Aliases: compare.n
### Keywords: MCMC, clade consensus, convergence frequency, phylogenetics,

### ** Examples

data(fungus)
compare.n(list(run1, run2), setnames=c("Chain1", "Chain2"), burnin=100)



cleanEx()
nameEx("cumulative.freq")
### * cumulative.freq

flush(stderr()); flush(stdout())

### Name: cumulative.freq
### Title: Cumulative frequencies of clades in an MCMC chain
### Aliases: cumulative.freq
### Keywords: MCMC, convergence phylogenetics, posterior probabilities,

### ** Examples

data(fungus)
cumulative.data <- cumulative.freq(run1$trees, burnin=100, window.size=20, gens.per.tree=10000)



cleanEx()
nameEx("load.trees")
### * load.trees

flush(stderr()); flush(stdout())

### Name: load.trees
### Title: Custom functions to load tree lists so that rwty can do basic
###   processing on the way in.
### Aliases: load.trees
### Keywords: MCMC, Phylogenetics, load

### ** Examples

#load.trees(file="mytrees.nex", type="nexus")



cleanEx()
nameEx("plot.cladeprobs")
### * plot.cladeprobs

flush(stderr()); flush(stdout())

### Name: plot.cladeprobs
### Title: New style plotting of cumulative and slide objects
### Aliases: plot.cladeprobs
### Keywords: cumulative, mcmc, phylogenetics, plot sliding window,

### ** Examples

data(fungus)
slide.data <- slide.freq(run1$trees, burnin=100, window.size=20, gens.per.tree=10000)
cpplot <- plot.cladeprobs(input.table = slide.data$slide.table, numclades=25)



cleanEx()
nameEx("plot.cladevar")
### * plot.cladevar

flush(stderr()); flush(stdout())

### Name: plot.cladevar
### Title: Plots distribution of variance in posterior probability
###   estimates across clades.
### Aliases: plot.cladevar
### Keywords: convergence, mcmc, phylogenetics, uncertainty

### ** Examples

data(fungus)
slide.data <- slide.freq(run1$trees, burnin=100, window.size=20, gens.per.tree=10000)
cvplot <- plot.cladevar(slide.data$slide.table, numclades=100)



cleanEx()
nameEx("slide.freq")
### * slide.freq

flush(stderr()); flush(stdout())

### Name: slide.freq
### Title: Sliding window measurements of clade posterior probabilities.
### Aliases: slide.freq
### Keywords: MCMC, convergence posterior probability,

### ** Examples

data(fungus)
slide.data <- slide.freq(run1$trees, burnin=100, window.size=20, gens.per.tree=10000)



cleanEx()
nameEx("tree.dist.matrix")
### * tree.dist.matrix

flush(stderr()); flush(stdout())

### Name: tree.dist.matrix
### Title: Tree distance matrix calculation
### Aliases: tree.dist.matrix
### Keywords: distance, robinson-foulds tree treespace,

### ** Examples

data(fungus)
tree.dist.matrix(list(run1$trees[[1]], run1$trees[[2]], run1$trees[[3]]), treenames=c("tree1", "tree2", "tree3"))



cleanEx()
nameEx("tree.distance")
### * tree.distance

flush(stderr()); flush(stdout())

### Name: tree.distance
### Title: Returns distance between two trees
### Aliases: tree.distance
### Keywords: Foulds Robinson distance, tree

### ** Examples

data(fungus)
tree.distance(list(run1$trees[[1]], run1$trees[[2]]))



cleanEx()
nameEx("treespace.single")
### * treespace.single

flush(stderr()); flush(stdout())

### Name: treespace.single
### Title: MDS scaling of treespace for a single tree list.
### Aliases: treespace.single
### Keywords: mds, multi-dimensional scaling treespace,

### ** Examples

data(fungus)
burnin <- 100
#We have to start by trimming the chain and p table down to a reasonable number of trees
mdstrees <- run1$trees[seq((burnin + 1), length(run1$trees), by = 20)]
#Now we're going to get a list of generations matching the trees retained from the chain
gens <- as.numeric(unlist(regmatches(names(run1$trees), gregexpr('\\(?[0-9]+', names(run1$trees)))))
gens <- gens[seq((burnin + 1), length(run1$trees), by = 20)]
#Finally we're going to cut down the p table to just the bits we're going to use
mdsptable <- run1$ptable[seq((burnin + 1), length(run1$trees), by = 20),]
this.treespace <- treespace.single(mdstrees, gens=gens, ptable=mdsptable)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
