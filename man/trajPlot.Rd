\name{trajLevel}
\alias{trajLevel}
\alias{trajPlot}
\title{Trajectory plots with conditioning}
\usage{
  trajLevel(mydata, lon = "lon", lat = "lat",
    pollutant = "pm10", type = "default", method = "level",
    smooth = FALSE, map = TRUE, lon.inc = 1.5,
    lat.inc = 1.5, ...)

  trajPlot(mydata, lon = "lon", lat = "lat",
    pollutant = "pm10", type = "default",
    method = "scatter", smooth = FALSE, map = TRUE,
    lon.inc = 1.5, lat.inc = 1.5, group = NA, ...)
}
\arguments{
  \item{mydata}{Data frame, the result of importing a
  trajectory file using \code{importTraj}}

  \item{lon}{Column containing the longitude, as a
  decimal.}

  \item{lat}{Column containing the latitude, as a decimal.}

  \item{pollutant}{Pollutant to be plotted.}

  \item{type}{\code{type} determines how the data are split
  i.e. conditioned, and then plotted. The default is will
  produce a single plot using the entire data. Type can be
  one of the built-in types as detailed in \code{cutData}
  e.g. "season", "year", "weekday" and so on. For example,
  \code{type = "season"} will produce four plots --- one
  for each season.

  It is also possible to choose \code{type} as another
  variable in the data frame. If that variable is numeric,
  then the data will be split into four quantiles (if
  possible) and labelled accordingly. If type is an
  existing character or factor variable, then those
  categories/levels will be used directly. This offers
  great flexibility for understanding the variation of
  different variables and how they depend on one another.

  \code{type} can be up length two e.g. \code{type =
  c("season", "weekday")} will produce a 2x2 plot split by
  season and day of the week. Note, when two types are
  provided the first forms the columns and the second the
  rows.}

  \item{method}{For trajectory plots, either "scatter" or
  "level". The latter option bins the data according to the
  values of \code{x.inc} and \code{y.inc}.}

  \item{smooth}{Should the trajectory surface be smoothed?}

  \item{map}{Should a base map be drawn? If \code{TRUE} the
  world base map from the \code{maps} package is used.}

  \item{lon.inc}{The longitude-interval to be used for
  binning data when \code{method = "level"}.}

  \item{lat.inc}{The latitude-interval to be used for
  binning data when \code{method = "level"}.}

  \item{...}{other arguments are passed to \code{cutData}
  and \code{scatterPlot}. This provides access to arguments
  used in both these functions and functions that they in
  turn pass arguments on to. For example, \code{plotTraj}
  passes the argument \code{cex} on to \code{scatterPlot}
  which in turn passes it on to the \code{lattice} function
  \code{xyplot} where it is applied to set the plot symbol
  size.}

  \item{group}{For \code{trajPlot} it is sometimes useful
  to group and colour trajectories according to a grouping
  variable. See example below.}
}
\value{
  NULL
}
\description{
  This function plots back trajectories. There are two
  related functions: \code{trajPlot} and \code{trajLevel}.
  These functions require that data are imported using the
  \code{importTraj} function.
}
\details{
  Several types of trajectory plot are available.
  \code{trajPlot} by default will plot each lat/lon
  location showing the origin of each trajectory, if no
  \code{pollutant} is supplied.

  If a pollutant is given, by merging the trajectory data
  with concentration data (see example below) the
  trajectories are colour-coded by the concentration of
  \code{pollutant}. With a long time series there can be
  lots of overplotting making it difficult to gauge the
  overall concentration pattern. In these cases setting
  \code{alpha} to a low value e.g. 0.1 can help.

  For the investigation of a few days it can be useful to
  use \code{plot.type = "l"}, which shows the back
  trajectories as continuous lines rather than individual
  points. Note that points help to show the duration an air
  mass spend in a particular location, whereas lines do
  not.

  An alternative way of showing the trajectories is to bin
  the points into latitude/longitude intervals and average
  the corresponding concentrations. For these purposes
  \code{trajLevel} should be used. A further useful
  refinement is to smooth the resulting surface, which is
  possible by setting \code{smooth = TRUE}.
}
\note{
  This function is under active development and is likely
  to change
}
\examples{
# show a simple case with no pollutant i.e. just the trajectories
# let's check to see where the trajectories were coming from when
# Heathrow Airport was closed due to the Icelandic volcanic eruption
# 15--21 April 2010.
# import trajectories for London and plot
\dontrun{
lond <- importTraj("london", 2010)
# well, HYSPLIT seems to think there certainly were conditions where trajectories
# orginated from Iceland...
trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"), plot.type = "l")}

# plot by day, need a column that makes a date
\dontrun{
lond$day <- as.Date(lond$date)
trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"), plot.type = "l",
type = "day")
}

# or show each day grouped by colour, with some other options set
\dontrun{
 trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"), plot.type = "l",
group = "day", col = "jet", lwd = 2, key.pos = "right", key.col = 1)
}
# more examples to follow linking with concentration measurements...

# import some measurements from KC1 - London
\dontrun{
kc1 <- importAURN("kc1", year = 2010)
# now merge with trajectory data by 'date'
lond <- merge(lond, kc1, by = "date")

# trajectory plot, no smoothing - and limit lat/lon area of interest
trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20), pollutant = "pm10")

# can smooth surface:
trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20), pollutant = "pm2.5",
smooth = TRUE)

# plot by season:
trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20), pollutant = "pm2.5",
smooth = TRUE, type = "season")
}
}
\author{
  David Carslaw
}
\seealso{
  \code{\link{importTraj}} to import trajectory data from
  the King's College server.
}

