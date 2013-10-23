\name{analyze.single}
\alias{analyze.single}
\title{Function for running rwty analyses on single chains.}
\usage{
  analyze.single(chains, burnin, window.size,
    gens.per.tree, step = 1, ...)
}
\arguments{
  \item{chains}{A single rwty.trees object. \code{chains}}

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
  output A list with tables and plots for LnL (when .p file
  exists), sliding window, cumulative, and treespace plots.
}
\description{
  This function is automatically called when analyze.rwty
  is called with one chain. It runs analyze.single on the
  chain and returns an object with the appropriate tables
  and plots.
}
\examples{
analyze.single(mytrees, burnin=100, window.size=100, gens.per.tree=1000, step=5)
}
\keyword{MCMC,}
\keyword{awty,}
\keyword{convergence,}
\keyword{phylogenetics,}
\keyword{plot,}
\keyword{rwty}

