\name{windRose}
\alias{windRose}
\alias{pollutionRose}

\title{Traditional wind rose plot and pollution rose variation}

\description{The traditional wind rose plot that plots wind speed and
wind direction by different intervals. The pollution rose applies the
same plot structure but substitutes other measurements, most commonly a
pollutant time series, for wind speed. }

\usage{
windRose(mydata,
    ws.int = 2, angle = 30, type = "default",
    cols = "default", main = "", grid.line = 5,
    width = 1, auto.text = TRUE, breaks = 4, offset = 10,
    paddle = TRUE, key.header = NULL, key.footer = "(m/s)",
    key.position = "bottom", key = TRUE, dig.lab = 5,
    statistic = "prop.count", pollutant = NULL, annotate = TRUE,
    ...)


pollutionRose(mydata,
    pollutant = "nox",
    key.footer = pollutant,
    breaks = 6, paddle = FALSE,
    key.position = "right",
    ...)


}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{A data frame containing fields \code{ws} and \code{wd}}

  \item{ws.int}{The Wind speed interval. Default is 2 m/s but for low met
  masts with low mean wind speeds a value of 1 or 0.5 m/s may be
  better. Note, this argument is superseded in \code{pollutionRose}. See
  \code{breaks} below.}

  \item{breaks}{The number of break points produced for wind speed in
  \code{windRose} or pollutant in \code{pollutionRose}. For
  \code{windRose} and the \code{ws.int} default of 2 m/s, the default,
  4, generates the break points 2, 4, 6, 8 m/s. For
  \code{pollutionRose}, the default, 6, attempts to breaks the supplied
  data at approximately 6 sensible break points. For example, the
  argument \code{breaks = c(1, 10, 100)} breaks the data into segments
  <1, 1-10, 10-100, >100.}

  \item{angle}{Default angle of "spokes" is 30. Other potentially useful
  angles are 45 and 10. Note that the width of the wind speed interval
  may need adjusting using \code{width}.}

\item{type}{\code{type} determines how the data are split
    i.e. conditioned, and then plotted. The default is will produce a
    single plot using the entire data. Type can be one of the built-in
    types as detailed in \code{cutData} e.g. "season", "year", "weekday"
    and so on. For example, \code{type = "season"} will produce four
    plots --- one for each season.

    It is also possible to choose \code{type} as another variable in the
    data frame. If that variable is numeric, then the data will be split
    into four quantiles (if possible) and labelled accordingly. If type
    is an existing character or factor variable, then those
    categories/levels will be used directly. This offers great
    flexibility for understanding the variation of different variables
    and how they depend on one another.

    Type can be up length two e.g. \code{type = c("season", "weekday")}
    will produce a 2x2 plot split by season and day of the week. Note,
    when two types are provided the first forms the columns and the
    second the rows.}


  \item{cols}{Colours to be used for plotting. Options include
  \code{default}, \code{increment}, \code{heat}, \code{jet}, \code{hue}
  and user defined. For user defined the user can supply a list of
  colour names recognised by R (type \code{colours()} to see the full
  list). An example would be \code{cols = c("yellow", "green", "blue",
  "black")}.}

  \item{main}{Title of plot.}

  \item{grid.line}{Grid line interval to use.}

  \item{paddle}{Either \code{TRUE} (default) or \code{FALSE}. If
  \code{TRUE} plots rose using `paddle' style spokes. If \code{FALSE}
  plots rose using `wedge' style spokes.}

  \item{width}{For \code{paddle = TRUE}, the adjustment factor for width
  of wind speed intervals. For example, \code{width = 1.5} will make the
  paddle width 1.5 times wider.}

  \item{offset}{The size of the 'hole' in the middle of the plot,
   expressed as a percentage of the polar axis scale, default 10.}

  \item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If
  \code{TRUE} titles and axis labels will automatically try and format
  pollutant names and units properly e.g.  by subscripting the `2' in
  NO2.}

  \item{key.header, key.footer}{Adds additional text/labels above and/or
  below the scale key, respectively. For example, passing
  \code{windRose(mydata, key.header = "ws")} adds the addition text as
  a scale header. Note: This argument is passed to
  \code{drawOpenKey} via \code{quickText}, applying the
  \code{auto.text} argument, to handle formatting.}

  \item{key.position}{Location where the scale key is to plotted.
  Allowed arguments currently include \code{"top"}, \code{"right"},
  \code{"bottom"} and \code{"left"}.}

  \item{key}{Fine control of the scale key via \code{drawOpenKey}. See
  \code{drawOpenKey} for further details.}

  \item{dig.lab}{The number of signficant figures at which scientific
  number formatting is used in break point and key labelling. Default
  5.}

  \item{statistic}{The \code{statistic} to be applied to each data bin
  in the plot. Options currently include \code{"prop.count"} and
  \code{"prop.mean"}. The default \code{"prop.count"} sizes bins according
  to the proportion of the frequency of measurements, in each bin. Similarly,
  \code{prop.mean} sizes bins according to their relative contribution to
  the mean. In both cases, results are expressed as percentages of the
  'whole-of-panel' \code{statistic} measurement. The overall value in
  each panel is shown at the bottom-right.}

  \item{pollutant}{Alternative data series to be sampled
  instead of wind speed. The \code{windRose} default NULL is equivalent
  to \code{pollutant = "ws"}.}

 \item{annotate}{If \code{TRUE} then the percentage calm and mean values
 are printed in each panel.}

  \item{...}{For \code{pollutionRose} other parameters that are passed
  on to \code{windRose}. For \code{windRose} other parameters that are
  passed on to \code{drawOpenKey}, \code{lattice:xyplot} and \code{cutData}. }

}

\value{
  As well as generating the plot itself, \code{windRose} and \code{pollutionRose}
  also return an object of class ``openair''. The object includes three main
  components: \code{call}, the command used to generate the plot; \code{data},
  the data frame of summarised information used to make the
  plot; and \code{plot}, the plot itself. If retained, e.g. using
  \code{output <- windRose(mydata)}, this output can be used to recover the data, reproduce
  or rework the original plot or undertake further analysis.

  An openair output can be manipulated using a number of generic operations, including
  \code{print}, \code{plot} and \code{summarise}. See \code{\link{openair.generics}}
  for further details.

  Summarised proportions can also be extracted directly using the \code{$data} operator, e.g.
  \code{object$data} for \code{output <- windRose(mydata)}. This returns a data frame with
  three set columns: \code{cond}, conditioning based on \code{type}; \code{wd}, the wind
  direction; and \code{calm}, the \code{statistic} for the proportion of data unattributed to
  any specific wind direction because it was collected under calm conditions; and then several
  (one for each range binned for the plot) columns giving proportions of measurements
  associated with each \code{ws} or \code{pollutant} range plotted as a discrete panel.

}


\details{For \code{windRose} data are summarised by direction,
  typically by 45 or 30 (or 10) degrees and by different wind speed
  categories. Typically, wind speeds are represented by different
  width "paddles". The plots show the proportion (here represented
  as a percentage) of time that the wind is from a certain angle and
  wind speed range.

  By default \code{windRose} will plot a windRose in using
  "paddle" style segments and placing the scale key below the plot.

  The argument \code{pollutant} uses the same plotting structure but
  substitutes another data series, defined by \code{pollutant}, for
  wind speed.

  The option \code{statistic = "prop.mean"} provides a measure of the
  relative contribution of each bin to the panel mean, and is intended
  for use with \code{pollutionRose}.

  \code{pollutionRose} is a \code{windRose} wrapper which brings
  \code{pollutant} forward in the argument list, and attempts to
  sensibly rescale break points based on the \code{pollutant} data
  range by by-passing \code{ws.int}.

  By default, \code{pollutionRose} will plot a pollution rose of
  \code{nox} using "wedge" style segments and placing the scale
  key to the right of the plot.

}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw (with some additional contributions by Karl
Ropkins)}

\note{
 \code{windRose} and \code{pollutionRose} both use
  \link{drawOpenKey} to produce scale keys.
}

\section{Warnings}{Note that \code{windRose} and \code{pollutionRose}
  are designed to work with UK Met Office files. Wind directions of 0
  degrees are assumed to be calms. The function will assume any data
  with wind speed = 0 or wind direction = 0 will be calm.

  Note that any \code{mydata} rows that are not classified as calm and
  contain \code{NAs} for \code{wd}, \code{ws} or \code{pollutant} (if
  set) cannot be assigned to the windRose structure and are removed
  before calculating the \code{statistic} or percent contributions.

}

\seealso{See \code{\link{drawOpenKey}} for fine control of the scale key.

See \code{\link{polarFreq}} for a more flexible version that
considers other statistics and pollutant concentrations.}

\examples{
# load example data from package data(mydata)

# basic plot
windRose(mydata)

# one windRose for each year
windRose(mydata,type = "year")

# windRose in 10 degree intervals with gridlines and width adjusted
windRose(mydata, angle = 10, width = 0.2, grid.line = 1)

# pollutionRose of nox
pollutionRose(mydata, pollutant = "nox")

## source apportionment plot - contribution to mean
\dontrun{
pollutionRose(mydata, pollutant = "pm10", type = "year", statistic = "prop.mean")
}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{methods}% __ONLY ONE__ keyword per line
