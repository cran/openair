\name{calendarPlot}
\alias{calendarPlot}
\title{Plot time series values in convential calendar format}
\usage{
  calendarPlot(mydata, pollutant = "nox", year = 2003,
    type = "default", annotate = "date",
    statistic = "mean", cols = "heat", limits = c(0, 100),
    lim = NULL, col.lim = c("grey30", "black"),
    font.lim = c(1, 2), cex.lim = c(0.6, 1), digits = 0,
    data.thresh = 0, main = paste(pollutant, "in", year),
    key.header = "", key.footer = "",
    key.position = "right", key = TRUE, auto.text = TRUE,
    ...)
}
\arguments{
  \item{mydata}{A data frame minimally containing
  \code{date} and at least one other numeric variable and a
  year. The date should be in either \code{Date} format or
  class \code{POSIXct}.}

  \item{pollutant}{Mandatory. A pollutant name
  corresponding to a variable in a data frame should be
  supplied e.g. \code{pollutant = "nox". }}

  \item{year}{Year to plot e.g. \code{year = 2003}.}

  \item{type}{Not yet implemented.}

  \item{annotate}{This option controls what appears on each
  day of the calendar. Can be: "date" - shows day of the
  month; "wd" - shows vector-averaged wind direction, or
  "ws" - shows vector-averaged wind direction scaled by
  wind speed. Finally it can be "value" which shows the
  daily mean value.}

  \item{statistic}{Statistic passed to \code{timeAverage}.}

  \item{cols}{Colours to be used for plotting. Options
  include "default", "increment", "heat", "jet" and user
  defined. For user defined the user can supply a list of
  colour names recognised by R (type \code{colours()} to
  see the full list). An example would be \code{cols =
  c("yellow", "green", "blue")}}

  \item{limits}{Use this option to manually set the colour
  scale limits. This is useful in the case when there is a
  need for two or more plots and a consistent scale is
  needed on each. Set the limits to cover the maximimum
  range of the data for all plots of interest. For example,
  if one plot had data covering 0--60 and another 0--100,
  then set \code{limits = c(0, 100)}. Note that data will
  be ignored if outside the limits range.}

  \item{lim}{A threshold value to help differentiate values
  above and below \code{lim}. It is used when
  \code{annotate = "value"}. See next few options for
  control over the labels used.}

  \item{col.lim}{For the annotation of concentration labels
  on each day. The first sets the colour of the text below
  \code{lim} and the second sets the colour of the text
  above \code{lim}.}

  \item{font.lim}{For the annotation of concentration
  labels on each day. The first sets the font of the text
  below \code{lim} and the second sets the font of the text
  above \code{lim}. Note that font = 1 is normal text and
  font = 2 is bold text.}

  \item{cex.lim}{For the annotation of concentration labels
  on each day. The first sets the size of the text below
  \code{lim} and the second sets the size of the text above
  \code{lim}.}

  \item{digits}{The number of digits used to display
  concentration values when \code{annotate = "value"}.}

  \item{data.thresh}{Data capture threshold passed to
  \code{timeAverage}. For example, \code{data.thresh = 75}
  means that at least 75\% of the data must be available in
  a day for the value to be calculate, else the data is
  removed.}

  \item{main}{The plot title; default is pollutant and
  year.}

  \item{key.header}{Adds additional text/labels to the
  scale key.  For example, passing
  \code{calendarPlot(mydata, key.header = "header",
  key.footer = "footer")} adds addition text above and
  below the scale key.  These arguments are passed to
  \code{drawOpenKey} via \code{quickText}, applying the
  \code{auto.text} argument, to handle formatting.}

  \item{key.footer}{see \code{key.header}.}

  \item{key.position}{Location where the scale key is to
  plotted.  Allowed arguments currently include
  \code{"top"}, \code{"right"}, \code{"bottom"} and
  \code{"left"}.}

  \item{key}{Fine control of the scale key via
  \code{drawOpenKey}. See \code{drawOpenKey} for further
  details.}

  \item{auto.text}{Either \code{TRUE} (default) or
  \code{FALSE}. If \code{TRUE} titles and axis labels will
  automatically try and format pollutant names and units
  properly e.g.  by subscripting the `2' in NO2.}

  \item{...}{Other graphical parameters are passed onto the
  \code{lattice} function \code{lattice:levelplot}, with
  common axis and title labelling options (such as
  \code{xlab}, \code{ylab}, \code{main}) being passed to
  via \code{quickText} to handle routine formatting.}
}
\value{
  As well as generating the plot itself,
  \code{calendarPlot} also returns an object of class
  ``openair''. The object includes three main components:
  \code{call}, the command used to generate the plot;
  \code{data}, the data frame of summarised information
  used to make the plot; and \code{plot}, the plot itself.
  If retained, e.g. using \code{output <-
  calendarPlot(mydata, "nox")}, this output can be used to
  recover the data, reproduce or rework the original plot
  or undertake further analysis.

  An openair output can be manipulated using a number of
  generic operations, including \code{print}, \code{plot}
  and \code{summary}. See \code{\link{openair.generics}}
  for further details.
}
\description{
  This function will plot one year of data by month laid
  out in a conventional calendar format. The main purpose
  is to help rapidly visualise potentially complex data in
  a familiar way. Users can also choose to show daily mean
  wind vectors if wind speed and direction are available.
}
\details{
  \code{calendarPlot} will plot one year of data in a
  conventional calendar format i.e. by month and day of the
  week. The main purpose of this function is to make it
  easy to visualise data in a familiar way. Currently the
  mean value of a variable is plotted using a colour scale.
  Further statistics will be added in due course.

  If wind direction are available it is then possible to
  plot the wind direction vector on each day. This is very
  useful for getting a feel for the meteorological
  conditions that affect pollutant concentrations. Note
  that if hourly or higher time resolution are supplied,
  then \code{calendarPlot} will calculate daily averages
  using \code{\link{timeAverage}}, which ensures that wind
  directions are vector-averaged.

  If wind speed is also available, then setting the option
  \code{annotate = "ws"} will plot the wind vectors whose
  length is scaled to the wind speed. Thus information on
  the daily mean wind speed and direction are available.
}
\examples{
# load example data from package
data(mydata)

# basic plot
calendarPlot(mydata, pollutant = "o3", year = 2003)

# show wind vectors
calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "wd")

# show wind vectors scaled by wind speed and different colours
calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "ws",
cols = "heat")

# show only specific months with selectByDate
calendarPlot(selectByDate(mydata, month = c(3,6,10), year = 2003),
pollutant = "o3", year = 2003, annotate = "ws", cols = "heat")
}
\author{
  David Carslaw
}
\seealso{
  \code{\link{timePlot}}, \code{\link{timeVariation}}
}
\keyword{methods}

