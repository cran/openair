\name{windRose}
\alias{windRose}
\alias{pollutionRose}

\title{Traditional windRose plot and pollutionRose variation}
\description{
The traditional windRose plot that plots wind speed  and wind direction by
different intervals. The pollutionRose applies the same plot 
structure but substitutes other measurements, most commonly a pollutant 
time series, for wind speed.
}

\usage{
windRose(polar,
    ws.int = 2, angle = 30, type = "default", 
    cols = "default", main = "", grid.line = 5,
    width = 1, auto.text = TRUE, breaks = 4, paddle = TRUE,
    key.header = NULL, key.footer = "(m/s)",
    key.position = "bottom", key = NULL, dig.lab = 5,
    pollutant = NULL, output = "graph",
    ...)

ans <- windRose(...)

pollutionRose(polar, 
    pollutant = "nox",
    key.footer = pollutant,
    breaks = 6, paddle = FALSE,
    key.position = "right",
    ...)

ans <- pollutionRose(...)

}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polar}{A data frame containing fields \code{ws} and \code{wd}}

  \item{ws.int}{The Wind speed interval. Default is 2 m/s but for low met
  masts with low mean wind speeds a value of 1 or 0.5 m/s may be
  better. Note, this argument is superceded in \code{pollutionRose}. See 
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

  \item{type}{Default is \code{default}, which will plot all data on one
  windRose. Other useful options are \code{year}, \code{month} and \code{hour} for
  one windRose for each year or each month respectfully. Many more
  types are shown in the function \code{cutData}. }

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

  \item{pollutant}{Alternative data series to be sampled 
  instead of wind speed. The \code{windRose} default NULL is equivalent 
  to \code{pollutant = "ws"}.}  

  \item{output}{The type of output returned by call. Default, 
  \code{graph}, returns the rose plot itself. Alternative, 
  \code{data}, plots the rose but returns the plot data frame.}

  \item{...}{For \code{pollutionRose} other parameters that are 
  passed on to \code{windRose}. For \code{windRose} other parameters 
  that are passed on to \code{drawOpenKey} and 
  \code{lattice:xyplot}. }

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
  substiutes another data series, defined by \code{pollutant}, for 
  wind speed. 

  \code{pollutionRose} is a \code{windRose} wrapper which brings 
  \code{pollutant} forward in the argument list, and attempts to 
  sensibly rescale break points based on the \code{pollutant} data 
  range by by-passing \code{ws.int}.
 
  By default, \code{pollutionRose} will plot a pollutionRose of 
  \code{nox} using "wedge" style segments and placing the scale 
  key to the right of the plot.

}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw (with some additional contributions by Karl 
Ropkins)}

%\note{ ~~further notes~~

\section{Warnings }{Note that \code{windRose} and \code{pollutionRose} 
  are designed to work with UK Met Office files. Wind directions of 0 degrees 
  are assumed to be calms. The function will assume any data with 
  wind speed = 0 or wind direction = 0 will be calm.

  Note that \code{windRose} and \code{pollutionRose} both use 
  \link{drawOpenKey} to produce scale keys.} 
 
%}
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
windRose(mydata, angle = 10, width = 0.3, grid.line = 1)

# pollutionRose of nox
pollutionRose(mydata, pollutant = "nox")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{methods}% __ONLY ONE__ keyword per line
