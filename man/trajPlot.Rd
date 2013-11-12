\name{trajLevel}
\alias{trajLevel}
\alias{trajPlot}
\title{Trajectory plots with conditioning}
\usage{
  trajLevel(mydata, lon = "lon", lat = "lat",
    pollutant = "height", type = "default", smooth = FALSE,
    statistic = "frequency", percentile = 90, map = TRUE,
    lon.inc = 1, lat.inc = 1, min.bin = 1, map.fill = TRUE,
    map.res = "default", map.cols = "grey40",
    map.alpha = 0.3, ...)

  trajPlot(mydata, lon = "lon", lat = "lat",
    pollutant = "height", type = "default", smooth = FALSE,
    statistic = "mean", percentile = 90, map = TRUE,
    lon.inc = 1, lat.inc = 1, min.bin = 1, group = NA,
    map.fill = TRUE, map.res = "default",
    map.cols = "grey40", map.alpha = 0.4, ...)
}
\arguments{
  \item{mydata}{Data frame, the result of importing a
  trajectory file using \code{importTraj}}

  \item{lon}{Column containing the longitude, as a
  decimal.}

  \item{lat}{Column containing the latitude, as a decimal.}

  \item{pollutant}{Pollutant to be plotted. By default the
  trajectory height is used.}

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

  \item{smooth}{Should the trajectory surface be smoothed?}

  \item{statistic}{For \code{trajLevel}. By default the
  function will plot the trajectory frequencies.

  For \code{trajLevel}, the argument \code{method =
  "hexbin"} can be used. In this case hexagonal binning of
  the trajectory \emph{points} (i.e. a point every three
  hours along each back trajectory). The plot then shows
  the trajectory frequencies uses hexagonal binning. This
  is an alternative way of viewing trajectory frequencies
  compared with \code{statistic = "frequency"}.

  There are also various ways of plotting concentrations.

  It is also possible to set \code{statistic =
  "difference"}. In this case trajectories where the
  associated concentration is greater than
  \code{percentile} are compared with the the full set of
  trajectories to understand the differences in freqeuncies
  of the origin of air masses. The comparsion is made by
  comparing the percentage change in gridded frequencies.
  For example, such a plot could show that the top 10\% of
  concentrations of PM10 tend to orginate from air-mass
  origins to the east.

  If \code{statistic = "pscf"} then a Potential Source
  Contribution Function map is produced. If \code{statistic
  = "cwt"} then concentration weighted trajectories are
  plotted.

  If \code{statistic = "cwt"} then the Concentration
  Weighted Trajectory approach is used. See details.}

  \item{percentile}{For \code{trajLevel}. The percentile
  concentration of \code{pollutant} against which the all
  trajectories are compared.}

  \item{map}{Should a base map be drawn? If \code{TRUE} the
  world base map from the \code{maps} package is used.}

  \item{lon.inc}{The longitude-interval to be used for
  binning data for \code{trajLevel}.}

  \item{lat.inc}{The latitude-interval to be used for
  binning data when \code{trajLevel}.}

  \item{min.bin}{For \code{trajLevel} the minimum number of
  unique points in a grid cell. Counts below \code{min.bin}
  are set as missing. For \code{trajLevel} gridded
  outputs.}

  \item{map.fill}{Should the base map be a filled polygon?
  Default is to fill countries.}

  \item{map.res}{The resolution of the base map. By default
  teh function uses the \sQuote{world} map from the
  \code{maps} package. If \code{map.res = "hires"} then the
  (much) more detailed base map \sQuote{worldHires} from
  the \code{mapdata} package is used.}

  \item{map.cols}{If \code{map.fill = TRUE} \code{map.cols}
  controls the fill colour. Examples include \code{map.fill
  = "grey40"} and \code{map.fill = openColours("default",
  10)}. The latter colours the countries and can help
  differentiate them.}

  \item{map.alpha}{The transpency level of the filled map
  which takes values from 0 (full transparency) to 1 (full
  opacity). Setting it below 1 can help view trajectories,
  trajectory surfaces etc. \emph{and} a filled base map.}

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
  with concentration data (see example below), the
  trajectories are colour-coded by the concentration of
  \code{pollutant}. With a long time series there can be
  lots of overplotting making it difficult to gauge the
  overall concentration pattern. In these cases setting
  \code{alpha} to a low value e.g. 0.1 can help.

  The user can aslo show points instead of lines by
  \code{plot.type = "p"}.

  Note that \code{trajPlot} will plot only the full length
  trajectories. This should be remembered when selecting
  only part of a year to plot.

  An alternative way of showing the trajectories is to bin
  the points into latitude/longitude intervals For these
  purposes \code{trajLevel} should be used. There are
  several trajectory statistics that can be plotted as
  gridded surfaces. First, \code{statistic} can be set to
  \dQuote{frequency} to show the number of back trajectory
  points in a grid square. Grid squares are by default at 1
  degree intervals, controlled by \code{lat.inc} and
  \code{lon.inc}. Such plots are useful for showing the
  frequency of air mass locations. Note that it is also
  possible to set \code{method = "hexbin"} for plotting
  frequencies (not concentrations), which will produce a
  plot by hexagonal binning.

  If \code{statistic = "difference"} the trajectories
  associated with a concentration greater than
  \code{percentile} are compared with the the full set of
  trajectories to understand the differences in freqeuncies
  of the origin of air masses of the highest concentration
  trajectories compared with the trajectories on average.
  The comparsion is made by comparing the percentage change
  in gridded frequencies. For example, such a plot could
  show that the top 10\% of concentrations of PM10 tend to
  orginate from air-mass origins to the east.

  If \code{statistic = "pscf"} then the Potential Source
  Contribution Function is plotted. The PSCF calculates the
  probability that a source is located at latitude \eqn{i}
  and longitude \eqn{j} (Pekney et al., 2006).The basis of
  PSCF is that if a source is located at (i,j), an air
  parcel back trajectory passing through that location
  indicates that material from the source can be collected
  and transported along the trajectory to the receptor
  site. PSCF solves \deqn{PSCF = m_{ij}/n_{ij}} where
  \eqn{n_{ij}} is the number of times that the trajectories
  passed through the cell (i,j) and \eqn{m_{ij}} is the
  number of times that a source concentration was high when
  the trajectories passed through the cell (i,j). The
  criterion for de-termining \eqn{m_{ij}} is controlled by
  \code{percentile}, which by default is 90. Note also that
  cells with few data have a weighting factor applied to
  reduce their effect.

  A limitation of the PSCF method is that grid cells can
  have the same PSCF value when sample concentrations are
  either only slightly higher or much higher than the
  criterion. As a result, it can be difficult to
  distinguish moderate sources from strong ones. Seibert et
  al. (1994) computed concentration fields to identify
  source areas of pollutants. The Concentration Weighted
  Trajectory (CWT) approach considers the concentration of
  a species together with its residence time in a grid
  cell. The CWT approach has been shown to yield similar
  results to the PSCF approach. The openair manual has more
  details and examples of these approaches.

  A further useful refinement is to smooth the resulting
  surface, which is possible by setting \code{smooth =
  TRUE}.
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
# use PSCF
trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20),
pollutant = "pm10", statistic = "pscf")

# can smooth surface, suing CWT approach:
trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20),
pollutant = "pm2.5", statistic = "cwt",  smooth = TRUE)

# plot by season:
trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20), pollutant = "pm2.5",
statistic = "pscf", type = "season")
}
}
\author{
  David Carslaw
}
\references{
  Pekney, N. J., Davidson, C. I., Zhou, L., & Hopke, P. K.
  (2006). Application of PSCF and CPF to PMF-Modeled
  Sources of PM 2.5 in Pittsburgh. Aerosol Science and
  Technology, 40(10), 952-961.

  Seibert, P., Kromp-Kolb, H., Baltensperger, U., Jost, D.,
  1994. Trajectory analysis of high-alpine air pollution
  data. NATO Challenges of Modern Society 18, 595-595.

  Xie, Y., & Berkowitz, C. M. (2007). The use of
  conditional probability functions and potential source
  contribution functions to identify source regions and
  advection pathways of hydrocarbon emissions in Houston,
  Texas. Atmospheric Environment, 41(28), 5831-5847.
}
\seealso{
  \code{\link{importTraj}} to import trajectory data from
  the King's College server.
}

