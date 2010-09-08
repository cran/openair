\name{selectByDate}
\alias{selectByDate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subset a data frame based on date}
\description{
  Utility function to make it easier to select periods from a data frame
  before sending to a function
}
\usage{
mydata <- selectByDate(mydata, start = "1/1/2008", end = "31/12/2008",
year = 2008, month = 1, day = "weekday", hour = 1)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mydata}{A data frame containing a \code{date} field in hourly or
  high resolution format.}

\item{start}{A start date in the form d/m/yyyy e.g. "1/2/1999".}

\item{end}{An end date in the form d/m/yyyy e.g. "31/5/1999".}

\item{year}{A year or years to select e.g. \code{year = 1998:2004} to
  select 1998-2004 inclusive or \code{year = c(1998, 2004)} to select
  1998 and 2004.}

\item{month}{A month or months to select. Can either be numeric
  e.g. \code{month = 1:6} to select months 1-6 (January to June), or by
  name e.g. \code{month = c("January", "December")}. Names can be
  abbreviated to 3 letters and be in lower or upper case.}

\item{day}{A day or or days to select. For example \code{day =
  c("Monday", "Wednesday")}. Names can be abbreviated to 3 letters and
  be in lower or upper case. Also accepts "weekday" (Monday - Friday)
  and "weekend" for convenience.}

\item{hour}{An hour or hours to select from 0-23 e.g. \code{hour = 0:12} to
  select hours 0 to 12 inclusive.}

}

\details{This function makes it much easier to select periods of interest
  from a data frame based on dates in a British format. Selecting date/times
  in R format can be intimidating for new users. This function can be
  used to select quite complex dates simply - see examples below.

Dates are assumed to be inclusive, so \code{start = "1/1/1999"} means
that times are selected from hour zero. Similarly, \code{end =
  "31/12/1999"} will include all hours of the 31st December.

All options are applied in turn making it possible to select quite
complex dates
}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
\note{Times are assumed to be in GMT.}


\examples{
## select all of 1999
data.1999 <- selectByDate(mydata, start = "1/1/1999", end = "31/12/1999")
head(data.1999)
tail(data.1999)

# easier way
data.1999 <- selectByDate(mydata, year = 1999)

# more complex use: select weekdays between the hours of 7 am to 7 pm
sub.data <- selectByDate(mydata, day = "weekday", hour = 7:19)

# select weekends between the hours of 7 am to 7 pm in winter (Dec, Jan, Feb)
sub.data <- selectByDate(mydata, day = "weekend", hour = 7:19, month =
c("dec", "jan", "feb"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{}% __ONLY ONE__ keyword per line
