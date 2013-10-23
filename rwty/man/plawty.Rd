\name{plawty}
\alias{plawty}
\title{Old style plotting of cumulative and slide objects}
\usage{
  plawty(x, numclades = 10)
}
\arguments{
  \item{x}{An rwty.slide or rwty.cumulative object}

  \item{numclades}{The number of clades to plot.  The
  clades with the highest sd in clade frequency are plotted
  first, so numclades = 10 will be the 10 most variable
  clades in the chain.}
}
\value{
  thisplot Returns a ggplot object.
}
\description{
  Takes a table from cumulative.freq or slide.freq as
  input. Numclades gives the number of clades to plot,
  starting from the top.  Since cumulative.freq and
  slide.freq both sort by sd these will by default be the
  most variable clades.
}
\examples{
plawty(slideresults, numclades=25)
}
\keyword{cumulative,}
\keyword{mcmc,}
\keyword{phylogenetics,}
\keyword{plot}
\keyword{sliding}
\keyword{window,}

