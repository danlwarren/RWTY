\name{treespace.single}
\alias{treespace.single}
\title{MDS scaling of treespace for a single tree list.}
\usage{
  treespace.single(trees, burnin = 0, p.file = NULL)
}
\arguments{
  \item{trees}{A multiphylo object \code{inputParameter1}}

  \item{burnin}{The number of trees to eliminate as burnin
  \code{inputParameter2}}

  \item{p.file}{a file of probabilities to use for coloring
  generations \code{inputParameter3}}
}
\value{
  Returns a list containing the mds table and a plot.
}
\description{
  This function constructs a distance matrix from a list of
  trees and uses multi-dimensional scaling to collapse it
  to a two- dimensional tree space for plotting.
}
\examples{
treespace.single(mytrees, burnin=100, p.file="mytrees.p")
}
\keyword{mds,}
\keyword{multi-dimensional}
\keyword{scaling}
\keyword{treespace,}

