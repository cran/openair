\name{aqStats}
\alias{aqStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate summary statistics for air pollution data by year}
\description{
  Calculate a range of air pollution-relevant statistics by year and by site.
}
\usage{
aqStats(mydata, pollutant = "no2", data.thresh = 75, percentile = c(95, 99),
                    transpose = FALSE, ...)

}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{A data frame containing a \code{date} field of hourly data.}

  \item{pollutant}{The name of a pollutant e.g. \code{pollutant =
      c("o3", "pm10")}.}

  \item{data.thresh}{The data capture threshold in \%. No values are
    calculated if data capture over the period of interest is less than
    this value. \code{data.thresh} is used for example in the
    calculation of daily mean values from hourly data. If there are less
    than \code{data.thresh} percentage of measurements available in a
    period, \code{NA} is returned.}

  \item{percentile}{Percentile values to calculate for each pollutant.}

  \item{transpose}{The default is to return a data frame with columns
    representing the statistics. If \code{transpose = TRUE} then the
    results have columns for each pollutant-site combination.}
  \item{...}{Other arguments, currently unused.}
}

\details{This function calculates a range of common and air
  pollution-specific statistics from a data frame. The statistics are
  calculated on an annual basis and the input is assumed to be hourly
  data. The function can cope with several sites and years. The user can
  control the output by setting \code{transpose} appropriately.

  Note that the input data is assumed to be in mass units e.g. ug/m3 for
  all species except CO (mg/m3).

  The following statistics are calculated:

  \itemize{
    \item \bold{data.capture} --- percentage data capture over a full year.
    \item \bold{mean} --- annual mean.
    \item \bold{minimum} --- minimum hourly value.
    \item \bold{maximum} --- maximum hourly value.
    \item \bold{median} --- median value.
    \item \bold{max.daily} --- maximum daily mean.
    \item \bold{max.rolling.8} --- maximum 8-hour rolling mean.
    \item \bold{max.rolling.24} --- maximum 24-hour rolling mean.
    \item \bold{percentile.95} --- 95th percentile. Note that several
    percentiles can be calculated.
    \item \bold{roll.8.O3.gt.100} --- number of days when the daily maximum
    rolling 8-hour mean ozone concentration is >100 ug/m3.
     \item \bold{hours.gt.200} --- number of hours NO2 is more than 200
     ug/m3.
     \item \bold{days.gt.50} --- number of days PM10 is more than 50 ug/m3.
}

  

}
\section{Warning }{Note that no rounding of the data is undertaken in
  the calculations. For this reason there may occasionally be some
  difference compared with statistics available through the AURN.}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
%\note{}


\examples{
## Statistics for 2004. NOTE! these data are in ppb/ppm so the
## example is for illustrative purposes only
aqStats(selectByDate(mydata, year = 2004), pollu=c("no2", "pm10"))
## transpose the results:
aqStats(selectByDate(mydata, year = 2004), pollu=c("no2", "pm10"),
transpose = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{}% __ONLY ONE__ keyword per line
