\name{cumulative.freq}
\alias{cumulative.freq}
\title{Cumulative frequencies of clades in an MCMC chain}
\usage{
  cumulative.freq(tree.list, burnin = 0, window.size,
    gens.per.tree = 1, slide.freq.table = NULL, ...)
}
\arguments{
  \item{tree.list}{A single rwty.trees or multiPhylo
  object. \code{chains}}

  \item{burnin}{The number of trees to eliminate as burnin
  \code{burnin}}

  \item{window.size}{The length of window (in trees) for
  the sliding window plot \code{window.size}}

  \item{gens.per.tree}{The number of generations per tree
  in the .t file.}

  \item{slide.freq.table}{A table from slide.freq.  If a
  table is not provided, it will be built.}
}
\value{
  output A list containing a table of cumulative
  frequencies and a clade translation table
}
\description{
  Calculates the posterior probability estimate of each
  clade in an MCMC chain as a function of chain length.
}
\examples{
cumulative.freq(mytrees, burnin=100, window.size=100, gens.per.tree = 1000, slide.freq.table = slidetable)
cumulative.freq(mytrees, burnin=100, window.size=100, gens.per.tree = 1000)
}
\keyword{MCMC,}
\keyword{convergence}
\keyword{phylogenetics,}
\keyword{posterior}
\keyword{probabilities,}

