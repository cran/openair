\name{polarFreq}
\alias{polarFreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to plot wind speed/direction frequencies and other statistics}

\description{ \code{polarFreq} primarily plots wind speed-direction
frequencies in \sQuote{bins}. Each bin is colour-coded depending on the
frequency of measurements. Bins can also be used to show the
concentration of pollutants using a range of commonly used statistics.
}

\usage{
polarFreq(mydata,
    pollutant = "",
    statistic = "frequency",
    ws.int = 1,
grid.line = 5,
    breaks = seq(0, 5000, 500),
    cols = "default",
    trans = TRUE,
    type = "default",
    min.bin = 1,
    border.col = "transparent",
    main = "",
    key.header = statistic,
    key.footer = pollutant,
    key.position = "right",
    key = NULL,
    auto.text = TRUE, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mydata}{A data frame minimally containing \code{ws}, \code{wd} and
\code{date}.}

  \item{pollutant}{Mandatory. A pollutant name corresponding to a
  variable in a data frame should be supplied e.g. \code{pollutant =
  "nox"} }

  \item{statistic}{The statistic that should be applied to each wind
speed/direction bin. Can be "frequency", "mean", "median", "max"
(maximum), "stdev" (standard deviation) or "weighted.mean". The option
"frequency" (the default) is the simplest and plots the frequency of
wind speed/direction in different bins. The scale therefore shows the
counts in each bin. The option "mean" will plot the mean concentration
of a pollutant (see next point) in wind speed/direction bins, and so
on. Finally, "weighted.mean" will plot the concentration of a pollutant
weighted by wind speed/direction. Each segment therefore provides the
percentage overall contribution to the total concentration.  More
information is given in the examples. Note that for options other than
"frequency", it is necessary to also provide the name of a
pollutant. See function \code{cutData} for further details.}

  \item{ws.int}{Wind speed interval assumed. In some cases e.g. a low
    met mast, an interval of 0.5 may be more appropriate.}

   \item{grid.line}{Radial spacing of grid lines.}

  \item{breaks}{The user can provide their own scale. \code{breaks}
  expects a sequence of numbers that define the range of the scale. The
  sequence could represent one with equal spacing e.g. \code{breaks =
  seq(0, 100, 10)} - a scale from 0-10 in intervals of 10, or a more
  flexible sequence e.g. \code{breaks = c(0, 1, 5, 7, 10)}, which may be
  useful for some situations.}

  \item{cols}{Colours to be used for plotting. Options include
  "default", "increment", "heat", "jet" and user defined. For user
  defined the user can supply a list of colour names recognised by R
  (type \code{colours()} to see the full list). An example would be
  \code{cols = c("yellow", "green", "blue")}}

  \item{trans}{Should a transformation be applied? Sometimes when
producing plots of this kind they can be dominated by a few high
points. The default therefore is \code{TRUE} and a square-root transform
is applied. This results in a non-linear scale and (usually) a better
representation of the distribution. If set to \code{FALSE} a linear
scale is used.}

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

  

  \item{min.bin}{The minimum number of points allowed in a wind
  speed/wind direction bin.  The default is 1. A value of two requires
  at least 2 valid records in each bin an so on; bins with less than 2
  valid records are set to NA. Care should be taken when using a value >
  1 because of the risk of removing real data points. It is recommended
  to consider your data with care. Also, the \code{polarPlot} function
  can be of use in such circumstances.}

  \item{border.col}{The colour of the boundary of each wind
speed/direction bin. The default is transparent. Another useful choice
sometimes is "white".}

  \item{main}{Title of plot.}

  \item{key.header, key.footer}{Adds additional text/labels to the scale
  key. For example, passing options \code{key.header =
  "header", key.footer = "footer"} adds addition text
  above and below the scale key. These arguments are passed to
  \code{drawOpenKey} via \code{quickText}, applying the \code{auto.text}
  argument, to handle formatting. }

  \item{key.position}{Location where the scale key is to plotted. 
  Allowed arguments currently include \code{"top"}, \code{"right"}, 
  \code{"bottom"} and \code{"left"}.}

  \item{key}{Fine control of the scale key via \code{drawOpenKey}. See 
  \code{drawOpenKey} for further details.}

  \item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If
  \code{TRUE} titles and axis labels will automatically try and format
  pollutant names and units properly e.g.  by subscripting the
  \sQuote{2} in NO2.}

\item{\dots}{Other graphical parameters passed onto
    \code{lattice:xyplot} and \code{cutData}. For example, in the case
    of \code{cutData} the option \code{hemisphere = "southern"}.}  }

\value{
  As well as generating the plot itself, \code{polarFreq} also returns an object of class 
  ``openair''. The object includes three main components: \code{call}, the command used to 
  generate the plot; \code{data}, the data frame of summarised information used to make the 
  plot; and \code{plot}, the plot itself. If retained, e.g. using 
  \code{output <- polarFreq(mydata, "nox")}, this output can be used to recover the data, reproduce 
  or rework the original plot or undertake further analysis.  

  An openair output can be manipulated using a number of generic operations, including 
  \code{print}, \code{plot} and \code{summary}. See \code{\link{openair.generics}} 
  for further details.   
  
}


\details{
\code{polarFreq} is its default use provides details of wind speed and
direction frequencies. In this respect it is similar to
\code{\link{windRose}}, but considers wind direction intervals of 10
degrees and a user-specified wind speed interval. The frequency of wind
speeds/directions formed by these \sQuote{bins} is represented on a
colour scale.

The \code{polarFreq} function is more flexible than either
\code{\link{windRose}} or \code{\link{polarPlot}}. It can, for
example, also consider pollutant concentrations (see examples
below). Instead of the number of data points in each bin, the
concentration can be shown. Further, a range of statistics can be used
to describe each bin - see \code{statistic} above. Plotting mean
concentrations is useful for source identification and is the same as
\code{\link{polarPlot}} but without smoothing, which may be preferable
for some data. Plotting with \code{statistic = "weighted.mean"} is
particularly useful for understanding the relative importance of
different source contributions. For example, high mean concentrations
may be observed for high wind speed conditions, but the weighted mean
concentration may well show that the contribution to overall
concentrations is very low.

\code{polarFreq} also offers great flexibility with the scale used and
the user has fine control over both the range, interval and colour.  }

\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
%\note{ ~~further notes~~

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{See Also as \code{\link{windRose}}, \code{\link{polarPlot}}}
\examples{

# basic wind frequency plot
polarFreq(mydata)

# wind frequencies by year
polarFreq(mydata, type = "year")


# mean SO2 by year, showing only bins with at least 2 points
\dontrun{polarFreq(mydata, pollutant = "so2", type = "year", statistic = "mean", min.bin = 2)}

# weighted mean SO2 by year, showing only bins with at least 2 points
\dontrun{polarFreq(mydata, pollutant = "so2", type = "year", statistic = "weighted.mean", min.bin = 2)}

#windRose for just 2000 and 2003 with different colours
\dontrun{polarFreq(subset(mydata, format(date, "\%Y") \%in\% c(2000, 2003)), type = "year", cols = "jet")}

# user defined breaks from 0-700 in intervals of 100 (note linear scale)
\dontrun{polarFreq(mydata, breaks = seq(0, 700, 100))}

# more complicated user-defined breaks - useful for highlighting bins with a certain number of data points
\dontrun{polarFreq(mydata, breaks = c(0, 10, 50, 100, 250, 500, 700))}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
