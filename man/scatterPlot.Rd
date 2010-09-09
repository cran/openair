\name{scatterPlot}
\alias{scatterPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Flexible scatterPlots}
\description{
  Scatter plots with conditioning and three main approaches:
  conventional scatterPlot, hexagonal binning and kernel density
  estimates. The former also has options for fitting smooth fits and
  linear models with uncertainties shown.
}
\usage{
scatterPlot(mydata,
x = "nox",
y = "no2",
method = "scatter",
group = FALSE,
avg.time = "default",
data.thresh = 0,
statistic = "mean",
percentile = 95,
type = "default",
layout = c(1, 1),
smooth = TRUE,
linear = FALSE,
ci = TRUE,
cols = "hue",
main = "",
ylab = y,
xlab = x,
pch = 1,
lwd = 1,
key = TRUE,
key.title = type,
key.columns = 1,
strip = TRUE,
log.x = FALSE,
log.y = FALSE,
nbin = 256,
continuous = FALSE,
auto.text = TRUE,
...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mydata}{A data frame containing at least two numeric variables to plot.}

  \item{x}{Name of the x-variable to plot.}

  \item{y}{Name of the y-variable to plot.}

  \item{method}{Methods include \code{"scatter"} (conventional scatter
  plot), \code{"hexbin"} (hexagonal binning using the \code{hexbin}
  package) and \code{"density"} (2D kernel density estimates). }

  \item{group}{If more than one pollutant is chosen, should they all be
    plotted on the same graph together? The default is \code{FALSE}, which
    means they are plotted in separate panels with their own scaled. If
    \code{TRUE} then they are plotted on the same plot with the same
    scale.}

 \item{avg.time}{This defines the time period to average to. Can be "sec",
  "min", "hour", "day", "DSTday", "week", "month", "quarter" or
  "year". For much increased flexibility a number can precede these
  options followed by a space. For example, a timeAverage of 2 months
  would be \code{period = "2 month"}. See function \code{timeAverage} for
  further details on this. This option ise useful as one method by which
  the number of points plotted is reduced i.e. by choosing a longer
  averging time.}

\item{data.thresh}{The data capture threshold to use (\%) when
  aggregating the data using \code{avg.time}. A value of zero means that
  all available data will be used in a particular period regardless if
  of the number of values available. Conversely, a value of 100 will
  mean that all data will need to be present for the average to be
  calculated, else it is recorded as \code{NA}. Not used if
  \code{avg.time = "default"}.}

\item{statistic}{The statistic to apply when aggregating the data;
  default is the mean. Can be one of "mean", "max", "min", "median",
  "frequency", "sd", "percentile". Note that "sd" is the standard
  deviation and "frequency" is the number (frequency) of valid
  records in the period. "percentile" is the percentile level (\%)
  between 0-100, which can be set using the "percentile" option -
  see below. Not used if \code{avg.time = "default"}.}

\item{percentile}{The percentile level in \% used when \code{statistic =
    "percentile"} and when aggregating the data with
    \code{avg.time}. The default is 95. Not used if \code{avg.time =
      "default"}.}

 \item{type}{The type of analysis to be done. The default is will
    produce a single plot using the entire data. Other types include
    "hour" (for hour of the day), "weekday" (for day of the week) and
    "month" (for month of the year), "year" for a polarPlot for each
    year. It is also possible to choose \code{type} as another variable
    in the data frame. For example, \code{type = "o3"} will plot four
    windRoses for different levels of ozone, split into four quantiles
    (approximately equal numbers of counts in each of the four
    splits). This offers great flexibility for understanding the
    variation of different variables dependent on another. See function
    \code{cutData} for further details. Note there is also an option
    "site", which is used for situations where there is a column "site"
    and multiple sites are present..}


  \item{layout}{Determines how the panels are laid out. By default,
    plots will be shown in one column with the number of rows equal to the
    number of pollutants, for example. If the user requires 2 columns and
    two rows, layout should be set to \code{layout = c(2, 2)}. In general,
    layout is expressed as number of columns times number of rows.}

 \item{smooth}{A smooth line is fitted to the data if \code{TRUE};
   optionally with 95\% confidence intervals shown.}

  \item{linear}{A linear model is fitted to the data if \code{TRUE};
 optionally with 95\% confidence intervals shown. The equation of the
 line and R2 value is also shown.}

 \item{ci}{Should the confidence intervals for the smooth/linear fit be shown?}

  \item{cols}{Colours to be used for plotting. Options include "default",
    "increment", "heat", "spectral", "hue", "brewer1"  and user
    defined (see manual for more details). The same line colour can be
    set for all pollutant e.g. \code{cols = "black"}.}

  \item{main}{The plot title; default is no title.}

  \item{ylab}{Name of y-axis variable. By default will use the name of
    \code{y}.}

  \item{xlab}{Name of x-axis variable. By default will use the name of
    \code{x}.}

  \item{pch}{The symbol type used for plotting. Default is to provide
    different symbol types for different pollutant. If one requires a
    single symbol for all pollutants, the set \code{pch = 1}, for
    example.}

  \item{lwd}{Not used yet.}

  \item{key}{Should a key be drawn? The default is \code{TRUE}.}

  \item{key.title}{The title of the key (if used).}

   \item{key.columns}{Number of columns to be used in the key. With many
  pollutants a single column can make to key too wide. The user can thus
  choose to use several columns by setting \code{columns} to be less
  than the number of pollutants.}

  \item{strip}{Should a strip be drawn? The default is \code{TRUE}.}

   \item{log.x}{Should the x-axis appear on a log scale? The default is
   \code{FALSE}. If \code{TRUE} a well-formatted log10 scale is
   used. This can be useful for checking linearity once logged.}

  \item{log.y}{Should the y-axis appear on a log scale? The default is
   \code{FALSE}. If \code{TRUE} a well-formatted log10 scale is
   used. This can be useful for checking linearity once logged.}

 \item{nbin}{Number of bins used for kernel density output using method
   \code{"density"}.}

 \item{continuous}{When this option is \code{TRUE} a plot of x vs. y
 will be made, colour-coded by levels of \code{type}. A continuous
 separate colour scale is shown. If \code{continuous = FALSE} and
 \code{type} is numeric then the plot of x vs. y is shown by different
 \emph{quantiles} of \code{type}. Note that all data will be used, even
 if several sites are present. Sometimes it may be useful to subset for
 an individual site. See example below.}

  \item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE}
    titles and axis labels will automatically try and format pollutant
    names and units properly e.g.  by subscripting the `2' in NO2.}

  \item{\dots}{Other graphical parameters.}
}

\details{ The \code{scatterPlot} is the basic function for plotting
  scatterPlots in flexible ways in \code{openair}. It is flexible
  enough to consider lots of conditioning variables and takes care of
  fitting smooth or linearRelationships to the data.

  There are three main ways of plotting the relationship between two
  variables, which are set using the \code{method} option. The default
  \code{"scatter"} will plot a conventional scatterPlot. In cases
  where there are lots of data and over-plotting becomes a problem, then
  \code{method = "hexbin"} or  \code{method = "density"} can be
  useful. The former requires the \code{hexbin} package to be installed.

  By default a smooth fit is shown as this can help show the overall
  form of the data e.g. whether the relationship appears to be linear or
  not. Also, a linear fit can be shown using \code{linear = TRUE} as an option.

  The user has fine control over the choice of colours and symbol type
  used.

  Another way of reducing the number of points used in the plots which
  can sometimes be useful is to aggregate the data. For example, hourly
  data can be aggregated to daily data. See \code{timePlot} for
  examples here.

  By default plots are shown with a colour key at the bottom and in the
  case of conditioning, strips on the top of each
  plot. Sometimes this may be overkill and the user can opt to remove
  the key and/or the strip by setting \code{key} and/or \code{strip} to
  \code{FALSE}. One reason to do this is to maximise the plotting area
  and therefore the information shown.
}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}

\seealso{\code{\link{linearRelation}}, \code{\link{timePlot}} and
  \code{\link{timeAverage}} for details on selecting averaging times
  and other statistics in a flexible way}

\examples{
# load openair data if not loaded already
data(mydata)

# basic use, single pollutant
scatterPlot(mydata, x = "nox", y = "no2")

# scatterPlot by year
scatterPlot(mydata, x = "nox", y = "no2", type = "year")

# scatterPlot by day of the week, removing key at bottom
scatterPlot(mydata, x = "nox", y = "no2", type = "weekday", key =
FALSE)

# example of the use of continuous where colour is used to show
# different levels of a third (numeric) variable
# plot daily averages and choose a filled plot symbol (pch = 16)
# select only 2004
\dontrun{dat2004 <- selectByDate(mydata, year = 2004)
scatterPlot(dat2004, x = "nox", y = "no2", type = "co", continuous =
 TRUE, avg.time = "day", pch = 16)}

# show linear fit, by year
\dontrun{scatterPlot(mydata, x = "nox", y = "no2", type = "year", smooth =
FALSE, linear = TRUE)}

# do the same, but for daily means...
\dontrun{scatterPlot(mydata, x = "nox", y = "no2", type = "year", smooth =
FALSE, linear = TRUE, avg.time = "day")}

# log scales
\dontrun{scatterPlot(mydata, x = "nox", y = "no2", type = "year", smooth =
FALSE, linear = TRUE, avg.time = "day", log.x = TRUE, log.y = TRUE)}

# also works with the x-axis in date format (alternative to timePlot)
\dontrun{scatterPlot(mydata, x = "date", y = "no2", avg.time = "month", key = FALSE)}

# use hexagonal binning
\dontrun{
library(hexbin)
# basic use, single pollutant
scatterPlot(mydata, x = "nox", y = "no2", method = "hexbin")

# scatterPlot by year
scatterPlot(mydata, x = "nox", y = "no2", type = "year", method = "hexbin")
}


}
\keyword{methods}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
