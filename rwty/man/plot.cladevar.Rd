\name{plot.cladevar}
\alias{plot.cladevar}
\title{Plots distribution of variance in posterior probability estimates across clades.}
\usage{
  plot.cladevar(input.table, numclades = nrow(input.table))
}
\arguments{
  \item{input.table}{Either a rwty.cumulative or rwty.slide
  object, or a cumulative or slide table from one of those
  objects.}

  \item{numclades}{The number of clades to calculate stats
  for.  Defaults to all clades in the table.}
}
\value{
  thisplot A ggplot object
}
\description{
  Plots the distribution of variances in posterior
  probability estimates across clades in a chain as a
  function of chain length.
}
\examples{
plot.cladevar(slideresults, numclades=100)
}
\keyword{convergence,}
\keyword{mcmc,}
\keyword{phylogenetics,}
\keyword{uncertainty}

