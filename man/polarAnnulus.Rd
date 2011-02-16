\name{polarAnnulus}
\alias{polarAnnulus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bivariate polarAnnulus plot}
\description{
  Typically plots the concentration of a pollutant by wind direction and as a function of time as an annulus. The function is good for visualising how concentrations of pollutants vary by wind direction and a time period e.g. by month, day of week.
}
\usage{
polarAnnulus(polar,
    pollutant = "nox", resolution = "fine",
    local.time = FALSE, period = "hour", type = "default",
    limits = c(0, 100), cols = "default", width = "normal",
    exclude.missing = TRUE, date.pad = FALSE, 
    force.positive = TRUE, k = 15, normalise = FALSE, main = "", 
    key.header = "", key.footer = pollutant, 
    key.position = "right", key = NULL, 
    auto.text = TRUE, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{polar}{A data frame minimally containing \code{ws}, \code{wd} and
  a pollutant. Can also contain \code{date} if plots by time period are
  required.}

\item{pollutant}{Mandatory. A pollutant name corresponding to a
    variable in a data frame should be supplied e.g. \code{pollutant =
    "nox"}. There can also be more than one pollutant specified
    e.g. \code{pollutant = c("nox", "no2")}. The main use of using two
    or more pollutants is for model evaluation where two species would
    be expected to have similar concentrations. This saves the user
    stacking the data and it is possible to work with columns of data
    directly. A typical use would be \code{pollutant = c("obs", "mod")}
    to compare two columns "obs" (the observations) and "mod" (modelled
    values).}

\item{resolution}{Two plot resolutions can be set: \code{"normal"} and
  \code{"fine"} (the default).}

\item{local.time}{Should the results be calculated in local time? The
  default is \code{TRUE}. Emissions activity tends to occur at local
  time e.g. rush hour is at 8 am every day. When the clocks go forward
  in spring, the emissions are effectively released into the
  atmosphere at BST - 1 hour during the summer. When plotting
  diurnal profiles, this has the effect of "smearing-out" the
  concentrations. A better approach is to express time as local time,
  which here is defined as BST (British Summer Time). This correction
  tends to produce better-defined diurnal profiles of concentration
  (or other variables) and allows a better comparison to be made with
  emissions/activity data. If set to \code{FALSE} then GMT is used.}

\item{period}{This determines the temporal period to consider. Options
  are "trend" (the default), "season" to plot variation throughout the
  year, "weekday" to plot day of the week variation and "hour" to plot
  the diurnal variation.}

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

\item{limits}{The function does its best to choose sensible limits
  automatically. However, there are circumstances when the user will
  wish to set different ones. An example would be a series of plots
  showing each year of data separately. The limits are set in the form
  \code{c(lower, upper)}, so \code{limits = c(0, 100)} would force the
  plot limits to span 0-100.}

\item{cols}{Colours to be used for plotting. Options include
  "default", "increment", "heat", "jet" and user defined. For user
  defined the user can supply a list of colour names recognised by R
  (type \code{colours()} to see the full list). An example would be
  \code{cols = c("yellow", "green", "blue")}}

\item{width}{The width of the annulus; can be "normal" (the default),
  "thin" or "fat".}

\item{exclude.missing}{Setting this option to \code{TRUE} (the
  default) removes points from the plot that are too far from the
  original data. The smoothing routines will produce predictions at
  points where no data exist i.e. they predict. By removing the points
  too far from the original data produces a plot where it is clear where
  the original data lie. If set to \code{FALSE} missing data will be
  interpolated.}

\item{date.pad}{For \code{type = "trend"} (default), \code{date.pad =
    TRUE} will pad-out missing data to the beginning of the first year and
  the end of the last year. The purpose is to ensure that the trend plot
  begins and ends at the beginning or end of year.}

\item{force.positive}{The default is \code{TRUE}. Sometimes if
  smoothing data with steep gradients it is possible for predicted
  values to be negative. \code{force.positive = TRUE} ensures that
  predictions remain postive. This is useful for several reasons. First,
  with lots of missing data more interpolation is needed and this can
  result in artifacts because the predictions are too far from the
  original data. Second, if it is known beforehand that the data are all
  postive, then this option carries that assumption through to the
  prediction. The only likely time where setting \code{force.positive =
    FALSE} would be if background concentrations were first subtracted
  resulting in data that is legitimately negative. For the vast majority
  of situations it is expected that the user will not need to alter the
  default option.}

\item{k}{This is the smoothing parameter that is set if auto.smooth is
  set to \code{FALSE}. Typically, value of around 15 (the default) seems
  to be suitable and will resolve more features in the plot. For
  \code{type = "trend"} \code{k = 20} is used to reveal more
  detail. Increasing the value of \code{k} will decrease the amount of
  smoothing, the calculations will take longer. Increasing \code{k} for
  \code{type} other than trend will have little effect. However it can
  be useful in trends to show more detail, hence a higher default value
  of \code{k}. In some cases e.g. a trend plot with less than 1-year of
  data the smoothing with the default values may become too noisy and
  affected more by outliers. Choosing a lower value of \code{k} (say 10)
  may help produce a better plot.}

 \item{normalise}{If \code{TRUE} concentrations are normalised by
  dividing by their mean value. This is done \emph{after} fitting the
  smooth surface. This option is particularly useful if one is
  interested in the patterns of concentrations for several pollutants on
  different scales e.g. NOx and CO. Often useful if more than one
  \code{pollutant} is chosen. }

\item{main}{Title of plot.}

 \item{key.header, key.footer}{Adds additional text/labels to the scale
  key. For example, passing the options \code{key.header =
  "header", key.footer = "footer1"} adds addition text
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
    of \code{cutData} the option \code{hemisphere = "southern"}.}
}

\value{
  As well as generating the plot itself, \code{polarAnnulus} also returns an object of class 
  ``openair''. The object includes three main components: \code{call}, the command used to 
  generate the plot; \code{data}, the data frame of summarised information used to make the 
  plot; and \code{plot}, the plot itself. If retained, e.g. using 
  \code{output <- polarAnnulus(mydata, "nox")}, this output can be used to recover the data, reproduce 
  or rework the original plot or undertake further analysis.  

  An openair output can be manipulated using a number of generic operations, including 
  \code{print}, \code{plot} and \code{summary}. See \code{\link{openair.generics}} 
  for further details.   
  
}

\details{The \code{polarAnnulus} function shares many of the properties
of the \code{polarPlot}. However, \code{polarAnnulus} is focussed on
displaying information on how concentrations of a pollutant (values of
another variable) vary with wind direction and time. Plotting as an
annulus helps to reduce compression of information towards the centre of
the plot. The circular plot is easy to interpret because wind direction
is most easily understood in polar rather than Cartesian coordinates.

  The inner part of the annulus represents the earliest time and the
  outer part of the annulus the latest time. The time dimension can be
  shown in many ways including "trend" (default), "hour" (hour or day),
  "season" (month of the year) and "weekday" (day of the week). Taking
  hour as an example, the plot will show how concentrations vary by hour
  of the day and wind direction. Such plots can be very useful for
  understanding how different source influences affect a location.

  For \code{type = "trend"} the amount of smoothing does not vary
linearly with the length of the time series i.e. a certain amount of
smoothing per unit interval in time. This is a deliberate choice because
should one be interested in a subset (in time) of data, more detail will
be provided for the subset compared with the full data set. This allows
users to investigate specific periods in more detail. Full flexibility
is given through the smoothing parameter \code{k}.  }

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
%\note{ ~~further notes~~

\section{Warning }{The function is written mostly for more than 1-year
of hourly data. If less than 1-year of data are used and \code{type =
"trend"} then \code{type = "season"} will be used, which should have the
desired effect. Setting \code{k} too high may result in an error if
there are insufficient data to justify such detailed
smoothing. Calculations will take longer as \code{k} increases.}

\seealso{\code{\link{polarPlot}}, \code{\link{polarFreq}}}
\examples{

# load example data from package
data(mydata)

# trend plot for PM10 at Marylebone Rd
polarAnnulus(mydata, poll="pm10", main = "trend in pm10 at Marylebone Road")

# seasonal plot for PM10 at Marylebone Rd
\dontrun{polarAnnulus(mydata, poll="pm10", period = "season")}

# trend in coarse particles (PMc = PM10 - PM2.5), calculate PMc first

mydata$pmc <- mydata$pm10 - mydata$pm25
\dontrun{polarAnnulus(mydata, poll="pmc", period = "trend", main = "trend in pmc at Marylebone Road")}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{bivariate}% __ONLY ONE__ keyword per line
