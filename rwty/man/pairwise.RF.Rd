\name{pairwise.RF}
\alias{pairwise.RF}
\title{Sequential RF distances for a list of trees}
\usage{
  pairwise.RF(trees, n = length(trees) - 1)
}
\arguments{
  \item{trees}{A multiPhylo object}

  \item{n}{Number of comparisons}
}
\value{
  output A vector of distances
}
\description{
  input a list of trees, get back a list of sequential RF
  distances n is the number of comparisons you want to
  make. Useful for making sure you count the same number of
  instances. the default is to look at all pairs of trees
  in the list
}
\examples{
pairwise.RF(mytrees)
}
\keyword{MCMC,}
\keyword{distance}
\keyword{phylogenetics,}

