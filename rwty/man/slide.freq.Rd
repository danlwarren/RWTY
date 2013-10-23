\name{slide.freq}
\alias{slide.freq}
\title{Sliding window measurements of clade posterior probabilities.}
\usage{
  slide.freq(tree.list, burnin = 0, window.size,
    gens.per.tree = 1, ...)
}
\arguments{
  \item{tree.list}{A rwty trees object or multiPhylo
  \code{tree.list}}

  \item{burnin}{The number of trees to eliminate as burnin
  \code{burnin}}

  \item{window.size}{The number of trees to include in each
  window \code{window.size}}

  \item{gens.per.tree}{The number of steps in the MCMC
  chain corresponding to a tree in the tree list.
  \code{gens.per.tree}}
}
\value{
  rwty.slide An object containing the frequencies of clades
  in the sliding windows, a translation table that converts
  clade groupings to factors, and a plot.
}
\description{
  This function takes sliding windows of a specified length
  over an MCMC chain and calculates the posterior
  probability of clades within that window.  It allows
  users to see whether the chain is visiting different
  areas of treespace.
}
\examples{
slide.freq(mytrees, burnin=100, window.size=100, gens.per.tree=1000)
}
\keyword{MCMC,}
\keyword{convergence}
\keyword{posterior}
\keyword{probability,}

