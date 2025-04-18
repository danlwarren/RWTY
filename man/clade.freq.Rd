% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clade.freq.R
\name{clade.freq}
\alias{clade.freq}
\title{Returns clade names and frequencies}
\usage{
clade.freq(x, start, end, rooted = FALSE, ...)
}
\arguments{
\item{x}{A multiPhylo or rwty.chain object}

\item{start}{The index of the first tree to consider in calcuating frequencies}

\item{end}{The index of the last tree to consider in calculating frequencies}

\item{rooted}{(TRUE/FALSE).  Tells RWTY whether your trees are rooted or not.}

\item{...}{Arguments to be passed to ape's prop.part function}
}
\value{
clade.df A data froma containing clade names and frequencies
}
\description{
Uses ape functionality to get the frequencies and names of clades in an MCMC chain
or subset thereof.
}
\examples{
\dontrun{
data(fungus)
clade.freq(fungus$Fungus.Run1, start=10, end=100)
}
}
\keyword{Clade}
\keyword{consensus}
\keyword{frequency}
\keyword{mcmc}
\keyword{phylogenetics}
