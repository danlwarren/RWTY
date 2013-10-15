\name{tree.ess}
\alias{tree.ess}
\title{Estimating effective sample size for tree topology}
\usage{
  tree.ess(tree.list, burnin = 0, N = 20)
}
\arguments{
  \item{tree.list}{A multiPhylo object \code{tree.list}}

  \item{burnin}{The number of trees to eliminate as burnin
  \code{burnin}}

  \item{N}{the number of windows to attempt}
}
\value{
  output A description of the object the function outputs
}
\description{
  This function estimates the effective sample size for
  trees in a MCMC chain.  It operates by looking at RF
  distances for trees spaced at different intervals on the
  chain, and determining what spacing is needed so that
  adjacent trees are no more correlated than randomly
  chosen pairs from the whole chain.
}
\examples{
tree.ess(mytrees, burnin=100, N=20)
}
\keyword{autocorrelation,}
\keyword{effective}
\keyword{ess,}
\keyword{phylogeny}
\keyword{sample}
\keyword{size,}

