\name{rollingMean}
\alias{rollingMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate rollingMean values}
\description{
  Calculate rollingMean values taking account of data capture thresholds
}
\usage{
mydata <- rollingMean(mydata, pollutant = "o3", hours = 8, new.name = "rolling",
                     data.capture = 75)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{A data frame containing a \code{date} field.}

  \item{pollutant}{The name of a pollutant e.g. \code{pollutant =
      "o3"}.}

  \item{hours}{The averaging period to use e.g. \code{hours = 8} will
    generate 8-hour rollingMean values.}

  \item{new.name}{The name given to the new rollingMean variable. If
  not supplied it will create a name based on the name of the pollutant
  and the averaging period used.}

  \item{data.capture}{The data capture threshold in \%. No values are
    calculated if data capture over the period of interest is less than
    this value. For example, with \code{hours = 8} and
    \code{data.capture = 75} at least 6 hours are required to calculate
    the mean, else \code{NA} is returned.}
}

\details{This is a utility function mostly designed to calculate rolling
  mean statistics relevent to some pollutant limits e.g. 8 hour rolling
  means for ozone and 24 hour rollingMeans for PM10.

}
\section{Warning }{Note that the function currently only works with a
  single site.}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
%\note{}


\examples{
## rolling 8-hour mean for ozone
mydata <- rollingMean(mydata, pollutant = "o3", hours = 8, new.name =
"rollingo3", data.capture = 75)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{}% __ONLY ONE__ keyword per line
