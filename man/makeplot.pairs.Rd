% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeplot.pairs.R
\name{makeplot.pairs}
\alias{makeplot.pairs}
\title{Plotting parameters against each other}
\usage{
makeplot.pairs(chains, burnin = 0, treedist = "PD", params = NA, strip = 1)
}
\arguments{
\item{chains}{A list of rwty.chain objects.}

\item{burnin}{The number of trees to omit as burnin.}

\item{treedist}{the type of tree distance metric to use, can be 'PD' for path distance or 'RF' for Robinson Foulds distance}

\item{params}{'NA', 'all', or a vector of column names to include in the plot. 'NA' gives the default behaviour (see above). 'all' plots all columns (watch out!). Choose specific columns by name with a vector.}

\item{strip}{Number indicating which column to strip off (i.e., strip=1 removes first column, which is necessary for most MCMC outputs in which the first column is just the generation).}
}
\value{
pairs.plot Returns a ggplot object.
}
\description{
Makes a plot matrix of each parameter against each other (including the topology) in your analysis. The default behaviour
is to just plot the first two columns of your parameter file (after removing the column for the generation number) as well
as the topological distance. This usually means that you see a pairs plot with the likelihood, the tree length, and the tree toppology. 
We do this because some parameter files contain so many columns that the plot matrix becomes too 
busy. To include parameters of your choice, use the 'parameters' argument. In this function, the topological distance is
calculate from the first tree in every chain.
}
\examples{
\dontrun{
data(salamanders)
makeplot.pairs(salamanders[1], burnin=20)

# plot all the variables
makeplot.pairs(salamanders[1], burnin=20, params = 'all')

# plot specific the variables (note: you always get the topological distance)
makeplot.pairs(salamanders[1], burnin=20, params = c('pi.A.', 'pi.C.', 'pi.G.', 'pi.T.'))
}
}
\keyword{convergence}
\keyword{mcmc}
\keyword{parameter}
\keyword{phylogenetics}
\keyword{plot}
