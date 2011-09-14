\name{percentileRose}
\alias{percentileRose}
\title{Function to plot percentiles by wind direction}
\usage{
  percentileRose(mydata, pollutant = "nox", type =
  "default", percentile = c(25, 50, 75, 90, 95), cols =
  "default", fill = TRUE, angle.scale = 45, auto.text =
  TRUE, key.header = NULL, key.footer = "percentile",
  key.position = "bottom", key = TRUE, ...)
}
\arguments{
  \item{mydata}{A data frame minimally containing \code{wd}
  and a numeric field to plot --- \code{pollutant}.}

  \item{pollutant}{Mandatory. A pollutant name
  corresponding to a variable in a data frame should be
  supplied e.g. \code{pollutant = "nox"}. More than one
  pollutant can be supplied e.g. \code{pollutant = c("no2",
  "o3")} provided there is only one \code{type}.}

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

  Type can be up length two e.g. \code{type = c("season",
  "weekday")} will produce a 2x2 plot split by season and
  day of the week. Note, when two types are provided the
  first forms the columns and the second the rows.}

  \item{percentile}{The percentile value(s) to plot. Must
  be between 0--100.}

  \item{cols}{Colours to be used for plotting. Options
  include "default", "increment", "heat", "jet" and user
  defined. For user defined the user can supply a list of
  colour names recognised by R (type \code{colours()} to
  see the full list). An example would be \code{cols =
  c("yellow", "green", "blue")}.}

  \item{fill}{Should the percentile intervals be filled
  (default) or should lines be drawn (\code{fill =
  FALSE}).}

  \item{angle.scale}{The pollutant scale is by default
  shown at a 45 degree angle. Sometimes the placement of
  the scale may interfere with an interesting feature. The
  user can therefore set \code{angle.scale} to another
  value (between 0 and 360 degrees) to mitigate such
  problems. For example \code{angle.scale = 315} will draw
  the scale heading in a NW direction.}

  \item{auto.text}{Either \code{TRUE} (default) or
  \code{FALSE}. If \code{TRUE} titles and axis labels will
  automatically try and format pollutant names and units
  properly e.g.  by subscripting the \sQuote{2} in NO2.}

  \item{key.header,key.footer}{Adds additional text/labels
  to the scale key. For example, passing options
  \code{key.header = "header", key.footer = "footer"} adds
  addition text above and below the scale key. These
  arguments are passed to \code{drawOpenKey} via
  \code{quickText}, applying the \code{auto.text} argument,
  to handle formatting.}

  \item{key.position}{Location where the scale key is to
  plotted.  Allowed arguments currently include
  \code{"top"}, \code{"right"}, \code{"bottom"} and
  \code{"left"}.}

  \item{key}{Fine control of the scale key via
  \code{drawOpenKey}. See \code{drawOpenKey} for further
  details.}

  \item{\dots}{Other graphical parameters are passed onto
  \code{cutData} and \code{lattice:xyplot}. For example,
  \code{percentileRose} passes the option \code{hemisphere
  = "southern"} on to \code{cutData} to provide southern
  (rather than default northern) hemisphere handling of
  \code{type = "season"}. Similarly, common graphical
  arguments, such as \code{xlim} and \code{ylim} for
  plotting ranges and \code{lwd} for line thickness when
  using \code{fill = FALSE}, are passed on \code{xyplot},
  although some local modifications may be applied by
  openair. For example, axis and title labelling options
  (such as \code{xlab}, \code{ylab} and \code{main}) are
  passed to \code{xyplot} via \code{quickText} to handle
  routine formatting.}
}
\value{
  As well as generating the plot itself,
  \code{percentileRose} also returns an object of class
  ``openair''. The object includes three main components:
  \code{call}, the command used to generate the plot;
  \code{data}, the data frame of summarised information
  used to make the plot; and \code{plot}, the plot itself.
  If retained, e.g. using \code{output <-
  percentileRose(mydata, "nox")}, this output can be used
  to recover the data, reproduce or rework the original
  plot or undertake further analysis.

  An openair output can be manipulated using a number of
  generic operations, including \code{print}, \code{plot}
  and \code{summary}. See \code{\link{openair.generics}}
  for further details.
}
\description{
  \code{percentileRose} plots percentiles by wind direction
  with flexible conditioning. The plot can display mutiple
  percentile lines or filled areas.
}
\details{
  \code{percentileRose} calculates percentile levels of a
  pollutant and plots them by wind direction. One or more
  percentile levels can be calculated and these are
  displayed as either filled areas or as lines.

  The levels by wind direction are calculated using a
  cyclic smooth cubic spline. The wind directions are
  rounded to the nearest 10 degrees, consistent with
  surface data from the UK Met Office before a smooth is
  fitted.

  The \code{percentileRose} function compliments other
  similar functions including \code{\link{windRose}},
  \code{\link{pollutionRose}}, \code{\link{polarFreq}} or
  \code{\link{polarPlot}}. It is most useful for showing
  the distribution of concentrations by wind direction and
  often can reveal different sources e.g. those that only
  affect high percentile concentrations such as a chimney
  stack.

  Similar to other functions, flexible conditioning is
  available through the \code{type} option. It is easy for
  example to consider multiple percentile values for a
  pollutant by season, year and so on. See examples below.

  \code{percentileRose} also offers great flexibility with
  the scale used and the user has fine control over both
  the range, interval and colour.
}
\examples{
# basic percentile plot
percentileRose(mydata, pollutant = "o3")

# 50/95th percentiles of ozone, with different colours
percentileRose(mydata, pollutant = "o3", percentile = c(50, 95), col = "brewer1")

# percentiles of ozone by year, with different colours
percentileRose(mydata, type = "year", pollutant = "o3", col = "brewer1")

# percentile concentrations by season and day/nighttime..
percentileRose(mydata, type = c("season", "daylight"), pollutant = "o3", col = "brewer1")
}
\author{
  David Carslaw
}
\seealso{
  See Also as \code{\link{windRose}},
  \code{\link{pollutionRose}}, \code{\link{polarFreq}},
  \code{\link{polarPlot}}
}
\keyword{methods}

