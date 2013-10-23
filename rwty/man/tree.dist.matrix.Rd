\name{tree.dist.matrix}
\alias{tree.dist.matrix}
\title{Tree distance matrix calculation}
\usage{
  tree.dist.matrix(trees, treenames = names(trees))
}
\arguments{
  \item{trees}{a multiPhylo object \code{trees}}

  \item{treenames}{A vector of names for the trees
  \code{treenames}}
}
\value{
  RF A distance matrix of RF distances
}
\description{
  This function takes a list of trees and returns a
  distance matrix populated with Robinsoun-Foulds tree
  distances between all trees in the list.
}
\examples{
tree.dist.matrix(mytrees, treenames=c("tree1", "tree2", "tree3"))
}
\keyword{distance,}
\keyword{robinson-foulds}
\keyword{tree}
\keyword{treespace,}

