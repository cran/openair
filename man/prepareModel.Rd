\name{prepareModel}
\alias{prepareModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pre-process a data frame containing dispersion model-measurement data}
\description{
  Utility function to prepare input data for use in openair functions
}
\usage{
mydata <- prepareModel(mydata, measured = "obs", modelled = "mod",
pollutant = "nox")
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mydata}{A data frame containing a \code{date} field in hourly or
  high resolution format.}

\item{measured}{Name of a variable in \code{mydata} that respresents
  measured values.}

\item{modelled}{Name of a variable in \code{mydata} that respresents
  measured values.}

\item{pollutant}{Name of a pollutant being used for model-measurement comparison.}

}

\details{This is a utility function to prepare data for use in
  \code{openair} functions. It is intended for model-measurement
  comparsions where, typically, there are hourly measured concentrations
  and hourly modelled concentrations. The function adds a new field
  \code{site} that many \code{openair} functions use, and reshapes the
  data into a suitable format for use by them.}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
%\note{ ~~further notes~~


\examples{
## Given, for example, a data frame with the date, mod, meas field names
## it is run as shown below:

## mydata <- prepareModel(mydata, measured = "meas", modelled = "mod",
##   pollutant  = "o3")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{}% __ONLY ONE__ keyword per line
