\name{timeAverage}
\alias{timeAverage}
\title{Function to calculate timeAverages for data frames}
\usage{
  timeAverage(mydata, avg.time = "day", data.thresh = 0,
  statistic = "mean", percentile = NA, start.date = NA)
}
\arguments{
  \item{mydata}{A data frame containing a \code{date} field
  . Can be class \code{POSIXct} or \code{Date}.}

  \item{avg.time}{This defines the time period to average
  to. Can be "sec", "min", "hour", "day", "DSTday", "week",
  "month", "quarter" or "year". For much increased
  flexibility a number can precede these options followed
  by a space. For example, a time average of 2 months would
  be \code{avg.time = "2 month"}. See \code{cut.POSIXt} for
  further details on this.

  Note that \code{avg.time} can be \emph{less} than the
  time interval of the original series, in which case the
  series is expanded to the new time interval. This is
  useful, for example, for calculating a 15-minute time
  series from an hourly one where an hourly value is
  repeated for each new 15-minute period. Note that when
  expanding data in this way it is necessary to ensure that
  the time interval of the original series is an exact
  multiple of \code{avg.time} e.g. hour to 10 minutes, day
  to hour.}

  \item{data.thresh}{The data capture threshold to use (%).
  A value of zero means that all available data will be
  used in a particular period regardless if of the number
  of values available. Conversely, a value of 100 will mean
  that all data will need to be present for the average to
  be calculated, else it is recorded as \code{NA}.}

  \item{statistic}{The statistic to apply when aggregating
  the data; default is the mean. Can be one of "mean",
  "max", "min", "median", "sum", "frequency", "sd",
  "percentile". Note that "sd" is the standard deviation
  and "frequency" is the number (frequency) of valid
  records in the period. "percentile" is the percentile
  level (%) between 0-100, which can be set using the
  "percentile" option - see below.}

  \item{percentile}{The percentile level in % used when
  \code{statistic = "percentile"}. The default is 95.}

  \item{start.date}{A string giving a start date to use.
  This is sometimes useful if a time series starts between
  obvious intervals. For example, for a 1-minute time
  series that starts "2009-11-29 12:07:00" that needs to be
  averaged up to 15-minute means, the intervals would be
  "2009-11-29 12:07:00", "2009-11-29 12:22:00" etc. Often,
  however, it is better to round down to a more obvious
  start point e.g. "2009-11-29 12:00:00" such that the
  sequence is then "2009-11-29 12:00:00", "2009-11-29
  12:15:00" \ldots{} \code{start.date} is therefore used to
  force this type of sequence.}
}
\value{
  Returns a data frame with date in class \code{POSIXct}
  and will remove any non-numeric columns except a column
  "site".
}
\description{
  Function to flexibly aggregate or expand data frames by
  different time periods, calculating vector-averaged wind
  direction where appropriate. The averaged periods can
  also take account of data capture rates.
}
\details{
  This function calculates time averages for a data frame.
  It also treats wind direction correctly through
  vector-averaging. For example, the average of 350 degrees
  and 10 degrees is either 0 or 360 - not 180. The
  calculations therefore average the wind components.

  \code{timeAverage} should be useful in many circumstances
  where it is necessary to work with different time average
  data. For example, hourly air pollution data and
  15-minute meteorological data. To merge the two data sets
  \code{timeAverage} can be used to make the meteorological
  data 1-hour means first. Alternatively,
  \code{timeAverage} can be used to expand the hourly data
  to 15 minute data - see example below.

  For the research community \code{timeAverage} should be
  useful for dealing with outputs from instruments where
  there are a range of time periods used.

  It is also very useful for plotting data using
  \code{\link{timePlot}}. Often the data are too dense to
  see patterns and setting different averaging periods
  easily helps with interpretation.
}
\examples{
## daily average values
daily <- timeAverage(mydata, avg.time = "day")

## daily average values ensuring at least 75 \% data capture
## i.e. at least 18 valid hours
daily <- timeAverage(mydata, avg.time = "day", data.thresh = 75)

## 2-weekly averages
fortnight <- timeAverage(mydata, avg.time = "2 week")

## make a 15-minute time series from an hourly one
\dontrun{
min15 <-  timeAverage(mydata, avg.time = "15 min")
}
}
\author{
  David Carslaw
}
\seealso{
  See \code{\link{timePlot}} that plots time series data
  and uses \code{timeAverage} to aggregate data where
  necessary.
}
\keyword{methods}

