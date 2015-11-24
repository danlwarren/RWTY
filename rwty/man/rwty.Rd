\name{rwty}
\alias{rwty}
\title{R We There Yet?  An R package for visualizing convergence in MCMC phylogenetics.}
\usage{

}
\description{
This package implements various tests, visualizations, and metrics for diagnosing convergence of MCMC chains in phylogenetics.  It implements and automates many of the functions of the AWTY package in the R environment. It also adds a whole bunch of new functionality.

Most users will only need to directly interact with two functions: \code{\link{load.trees}} load.trees and \code{\link{analyze.rwty}}.  
}
\examples{
data(fungus)
analyze.rwty(fungus, burnin = 100)
}
\keyword{phylogenetics}
\keyword{mcmc}
\keyword{convergence}
\keyword{awty}

