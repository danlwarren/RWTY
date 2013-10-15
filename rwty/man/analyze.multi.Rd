\name{analyze.multi}
\alias{analyze.multi}
\title{Function for running rwty analyses on multiple chains.}
\usage{
  analyze.multi(chains, burnin, window.size, gens.per.tree,
    step = 1, labels = NA, ...)
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

  \item{labels}{Names for each of the trees, to be used in
  plotting and naming objects.}
}
\value{
  output A list of outputs from the analyze.single runs on
  each chain, as well as a compare.n run for all chains.
  Eventually we will add more multi-chain analyses.
}
\description{
  This function is automatically called when analyze.rwty
  is called with multple chains. It runs analyze.single for
  each chain, and then does multi-chain analyses as well.
}
\examples{
analyze.multi(list(chain1, chain2, chain3), burnin=100, window.size=100, gens.per.tree=1000, step=5, labels=c("Chain 1", "Chain 2", "Chain 3"))
}
\keyword{MCMC,}
\keyword{awty,}
\keyword{convergence,}
\keyword{phylogenetics,}
\keyword{plot,}
\keyword{rwty}

