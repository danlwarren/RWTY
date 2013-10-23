\name{load.trees}
\alias{load.trees}
\title{Custom functions to load tree lists so that rwty can do basic processing on the way in.}
\usage{
  load.trees(file, type = "nexus")
}
\arguments{
  \item{file}{A path to a .t file containing an MCMC chain
  of trees}

  \item{type}{An argument that designates the type of tree
  file.  If "nexus", trees are loaded using ape's
  read.nexus function.  Otherwise, it's read.tree.}
}
\value{
  output An rwty.trees object containing the multiPhylo and
  the table of values from the .p file if available.
}
\description{
  Loads trees, looks for a .p file of tree likelihoods,
  returns and rwty.trees object containing both
}
\examples{
load.trees(file="mytrees.nex", type="nexus")
}
\keyword{MCMC,}
\keyword{Phylogenetics,}
\keyword{load}

