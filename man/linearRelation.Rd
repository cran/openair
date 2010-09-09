\name{linearRelation}
\alias{linearRelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear relations between pollutants}

\description{ This function
considers linearRelationships between two pollutants. The relationships
are calculated on different times bases using a linear model. The slope
and 95\% confidence interval in slope relationships by time unit are
plotted in many ways. The function is particularly useful when
considering whether relationships are consistent with emissions
inventories.  }

\usage{ linearRelation(mydata, 
x = "nox",
y = "no2",
period = "monthly",
condition = FALSE,
n = 20,
rsq.thresh = 0,
ylim = c(0, 20),
ylab = paste("slope from ", y, " = m.", x, " + c", sep = ""),
xlab = NULL,
auto.text = TRUE,
main = "",
span = 0.3,...)  }

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mydata}{A data frame minimally containing \code{date} and two
pollutants.}

  \item{x}{First pollutant that when plotted would appear on the x-axis
  of a relationship e.g. \code{x = "nox"}.}

  \item{y}{Second pollutant that when plotted would appear on the y-axis
  of a relationship e.g. \code{y = "pm10"}.}

  \item{period}{A range of different time periods can be
  analysed. "monthly" will plot a monthly time series and "weekly" a
  weekly time series of the relationship between \code{x} and
  \code{y}. "hour" will show the diurnal relationship between \code{x}
  and \code{y} and "weekday" the day of the week relationship between
  \code{x} and \code{y}. "day.hour" will plot the relationship by
  weekday and hour of the day.}

  \item{condition}{For \code{period = "hour"}, \code{period = "day"} and
  \code{period = "day.hour"}, setting \code{condition = TRUE} will plot
  the relationships split by year. This is useful for seeing how the
  relationships may be changing over time.}

  \item{n}{The minimum number of points to be sent to the linear
  model. Because there may only be a few points e.g. hours where two
  pollutants are available over one week, \code{n} can be set to ensure
  that at least \code{n} points are sent to the linear model. If a
  period has hours < \code{n} that period will be ignored. }

  \item{rsq.thresh}{The minimum correlation coefficient (R2) allowed. If
  the relationship between \code{x} and \code{y} is not very good for a
  particular period, setting \code{rsq.thresh} can help to remove those
  periods where the relationship is not strong. Any R2 values below
  \code{rsq.thresh} will not be plotted. If set too high it may not be
  possible to fit a smooth line and warnings will be issues - but the
  plot still produced.}

  \item{ylim}{y-axis limits, specified by the user.}

  \item{ylab}{y-axis title, specified by the user.}

  \item{xlab}{x-axis title, specified by the user.}

  \item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE}
  titles and axis labels will automatically try and format pollutant
  names and units properly e.g.  by subscripting the \sQuote{2} in NO2.}

\item{main}{Title of plot.}

\item{span}{span for \code{loess} fit. Controls the fit line: lower
  values produce a more "wiggly" fit.}

  \item{\dots}{Other graphical parameters. A useful one to remove the
strip with the date range on at the top of the plot is to set
\code{strip = FALSE}.}  }

\details{

The relationships between pollutants can yield some very useful
information about source emissions and how they change. A scatterPlot
between two pollutants is the usual way to investigate the
relationship. A linear regression is useful to test the strength of the
relationship. However, considerably more information can be gleaned by
considering different time periods, such as how the relationship between
two pollutants vary over time, by day of the week, diurnally and so
on. The \code{linearRelation} function does just that - it fits a
linearRelationship between two pollutants over a wide range of time
periods determined by \code{period}.

\code{linearRelation} function is particularly useful if background
concentrations are first removed from roadside concentrations, as the
increment will relate more directly with changes in emissions. In this
respect, using \code{linearRelation} can provide valuable information
on how emissions may have changed over time, by hour of the day
etc. Using the function in this way will require users to do some basic
manipulation with their data first.

If a data frame is supplied that contains \code{nox}, \code{no2} and
\code{o3}, the \code{y} can be chosen as \code{y = "ox"}. In function
will therefore consider total oxidant slope (sum of NO2 + O3), which can
provide valuable information on likely vehicle primary NO
emissions. Note, however, that most roadside sites do not have ozone
measurements and \code{\link{calcFno2}} is the alternative.

A smooth line is added to all plots except \code{period = "weekday"}
using \code{smooth.spline}. \code{smooth.spline} uses Generalized Cross
Validation (GCV) to find a line that is (in laymans terms!) neither too
wiggly nor too smooth. The line tends to highlight both the overall
tendency of a relationship as well as some of the more interesting
features.

}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
%\note{ ~~further notes~~


\seealso{\code{\link{calcFno2}}}
\examples{

# monthly relationship between NOx and SO2 - note rapid fall in ratio at the beginning of the series
linearRelation(mydata, x = "nox", y = "so2")

# monthly oxidant slope - approximately corresponding to f-NO2
# note data frame needs date, nox, no2 and o3
# clear increase in slope at the end of 2002/beginning of 2003
linearRelation(mydata, x = "nox", y = "ox")

# diurnal oxidant slope by year
# clear change in magnitude starting 2003, but the diurnal profile has also changed:
# the morning and evening peak hours are more important, presumably due to change in certain vehicle types
linearRelation(mydata, x = "nox", y = "ox", period = "hour", condition = TRUE)

# PM2.5/PM10 ratio, but only plot where monthly R2 >= 0.8
linearRelation(mydata, x = "pm10", y = "pm25", rsq.thresh = 0.8)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{oxidant}% __ONLY ONE__ keyword per line
