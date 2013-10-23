\name{analyze.rwty}
\alias{analyze.rwty}
\title{Analyze.rwty, the main interface for rwty analyses and plots.}
\usage{
  analyze.rwty(chains, burnin, window.size, gens.per.tree,
    step = 1, ...)
}
\arguments{
  \item{chains}{A list of rwty.trees objects.
  \code{chains}}

  \item{burnin}{The number of trees to eliminate as burnin
  \code{burnin}}

  \item{window.size}{The length of window (in trees) for
  the sliding window plot \code{window.size}}

  \item{gens.per.tree}{The number of generations per tree
  in the .t file.}

  \item{step}{The number of trees to skip for each step in
  the treespace plot.  Step=5 would amount to reading every
  fifth tree.}
}
\value{
  output A list of outputs from the analyze.single runs on
  each chain, as well as a compare.n run for all chains.
  Eventually we will add more multi-chain analyses.
}
\description{
  This is the main user interface to rwty.  It allows users
  to chuck in arguments for chains, burnin, window size,
  gens per tree, and "step", and returns an object that
  contains sliding window and cumulative posterior
  probability plots, treespace plots, and multi-chain
  diagnostic plots when multiple chains are provided.
}
\examples{
analyze.rwty(chain1, burnin=100, window.size=100, gens.per.tree=1000, step=5)
analyze.rwty(list(chain1, chain2, chain3), burnin=100, window.size=100, gens.per.tree=1000, step=5, labels=c("Chain 1", "Chain 2", "Chain 3"))
}
\keyword{keywords}

