\name{cutData}
\alias{cutData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to split data in different ways for conditioning}

\description{Utility function to split data frames up in various ways
  for conditioning plots. Users would generally not be expected to call
  this function directly.  Widely used by many \code{openair} functions
  usually through the option \code{type}.}

\usage{mydata<- cutData(x, type = "default", hemisphere = "northern", n.levels = 4)}


%- maybe also 'usage' for other objects documented here.

\arguments{
\item{x}{A data frame containing a field \code{date}. }

\item{type}{A string giving the way in which the data frame should be
  split. Pre-defined values are: "default", "year", "hour", "month",
  "season", "weekday", "ws", "site", "weekend", "monthyear", "gmtbst" or
  "bstgmt".

\code{type} can also be the name of a numeric or factor. If a numeric
column name is supplied \code{cutData} will split the data into four
quantiles. Factors levels will be used to split the data without any
adjustment.}

\item{hemisphere}{Can be \code{"northern"} or \code{"southern"}. used to
  split data into seasons.}

\item{n.levels}{Number of quantiles to split numeric data into.}  


}

\details{This section give a brief description of each of the define
  levels of \code{type}. Note that all time dependent types require a
  column \code{date}.

 "default" does not split the data but will describe the levels as a
 date range in the format "day month year".

 "year" splits the data by each year.

 "month" splits the data by month of the year.

 "hour" splits the data by hour of the day.

 "monthyear" splits the data by year and month. It differs from month in
 that a level is defined for each month of the data set. This is useful
 sometimes to show an ordered sequence of months if the data set starts
 half way through a year; rather than starting in January.

 "weekend" splits the data by weekday and weekend.

 "weekday" splits the data by day of the week - ordered to start Monday.

 "season" splits data up by season. In the northern hemisphere winter =
 December, January, February; spring = March, April, May etc. These
 defintions will change of \code{hemisphere = "southern"}.

 "gmtbst" or "bstgmt" will split the data by hours that are in GMT
 i.e. mostly winter months) and hours in British summertime. Each of the
 two periods will be in \emph{local time}. The main purpose of this
 option is to test whether there is a shift in the diurnal profile when
 GMT and BST hours are compared. This option is particularly useful with
 the \code{timeVariation} function. For example, close to the source of
 road vehicle emissions, `rush-hour' will tend to occur at the same
 \emph{local time} throughout the year e.g. 8 am and 5 pm. Therefore,
 comparing GMT hours with BST hours will tend to show similar diurnal
 patterns (at least in the timing of the peaks, if not magnitude) when
 expressed in local time. By contrast a variable such as wind speed or
 temperature should show a clear shift when expressed in local time for
 BST vs. GMT. In essence, this option when used with
 \code{timeVariation} may help determine whether the variation in a
 pollutant is driven by man-made emissions or natural processes.

 "wd" splits the data by 8 wind sectors and requires a column \code{wd}:
 "NE", "E", "SE", "S", "SW", "W", "NW", "N".

 "ws" splits the data by 8 quantiles of wind speed and requires a column
 \code{ws}.

 "site" splits the data by site and therefore requires a column \code{site}.
}


\value{Returns a data frame with a column \code{cond} that is defined by
\code{type}.}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}

\examples{
## split data by day of the week
mydata <- cutData(mydata, type = "weekday")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{}% __ONLY ONE__ keyword per line
