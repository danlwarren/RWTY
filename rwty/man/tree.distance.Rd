\name{tree.distance}
\alias{tree.distance}
\title{Returns distance between two trees}
\usage{
  tree.distance(two.trees)
}
\arguments{
  \item{two.trees}{A list containing two trees
  \code{two.trees}}
}
\value{
  d A distance measurement
}
\description{
  Takes a list of two trees, returns Robinson-Foulds
  distance between them.
}
\examples{
tree.distance(list(tree1, tree2))
}
\keyword{Foulds}
\keyword{Robinson}
\keyword{distance,}
\keyword{tree}

