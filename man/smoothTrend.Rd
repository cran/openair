\name{smoothTrend}
\alias{smoothTrend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate smoothTrends}
\description{
Use non-parametric methods to calculate time series trends
}
\usage{
smoothTrend(mydata,
pollutant = "nox",
deseason = FALSE,
type = "default",
statistic = "mean",
percentile = NA,
data.thresh = 0,
simulate = FALSE,
n = 200,
autocor = FALSE,
cols = "brewer1",
ylab = pollutant,
xlab = "year",
lty = 1,
lwd = 1,
pch = 1,
cex = 0.6,
key.columns = length(percentile),
main = "",
ci = TRUE,
alpha = 0.2,
date.breaks = 7,
auto.text = TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{A data frame containing the field \code{date} and at least
    one other parameter for which a trend test is required; typically (but
    not necessarily) a pollutant.}

  \item{pollutant}{The parameter for which a trend test is
    required. Mandatory.}

  \item{deseason}{Should the data be de-deasonalized first? If
    \code{TRUE} the function \code{stl} is used (seasonal trend
    decomposition using loess). Note that if \code{TRUE} missing data are
    first linearly interpolated because \code{stl} cannot handle missing
    data.}

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

   \item{statistic}{Statistic used for calculating monthly
   values. Default is \code{"mean"}, but can also be
   \code{"percentile"}. See \code{timeAverage} for more details.}

 \item{percentile}{Percentile value(s) to use if \code{statistic =
 "percentile"} is chosen. Can be a vector of numbers
 e.g. \code{percentile = c(5, 50, 95)} will plot the 5th, 50th and 95th
 percentile values together on the same plot.}

\item{data.thresh}{The data capture threshold to use (\%) when
  aggregating the data using \code{avg.time}. A value of zero means that
  all available data will be used in a particular period regardless if
  of the number of values available. Conversely, a value of 100 will
  mean that all data will need to be present for the average to be
  calculated, else it is recorded as \code{NA}. Not used if
  \code{avg.time = "default"}.}


  \item{simulate}{Should simulations be carried out to determine the
    Mann-Kendall tau and p-value. The default is \code{FALSE}. If
    \code{TRUE}, bootstrap simulations are undertaken, which also account
    for autocorrelation.}

  \item{n}{Number of bootstrap simulations if \code{simulate = TRUE}.}

  \item{autocor}{Should autocorrelation be considered in the trend
    uncertainty estimates? The default is \code{FALSE}. Generally,
    accounting for autocorrelation increases the uncertainty of the trend
    estimate sometimes by a large amount.}

   \item{cols}{Colours to use. Can be a vector of colours
   e.g. \code{cols = c("black", "green")} or pre-defined openair colours
   --- see \code{openColours} for more details.}

 \item{ylab}{y-axis label.}

  \item{xlab}{x-axis label.}

  \item{lty}{Line type to use, can be a vector of types if the option
    \code{statistic = "percentile"} is used e.g. \code{lty = c(5, 1, 5)}.}

  \item{lwd}{Line width to use, can be a vector of widths if the option
    \code{statistic = "percentile"} is used e.g. \code{lwd = c(1, 2,
      1)}.}

   \item{pch}{Plot symbol to use, can be a vector of symbols if the option
    \code{statistic = "percentile"} is used e.g. \code{pch = c(1, 2,
      1)}. To remove symbols altogether use \code{pch = NA}.}

   \item{cex}{Plot symbol size, can be a vector of sizes if the option
    \code{statistic = "percentile"} is used e.g. \code{cex = c(1, 2,
    4)}.}


   \item{key.columns}{Number of columns used if a key is drawn when using
    the option \code{statistic = "percentile"}.}

  \item{main}{Title of plot, if required.}

  \item{ci}{Should confidence intervals be plotted? The default is
    \code{FALSE}.}

  \item{alpha}{The alpha transparency of shaded confidence intervals -
    if plotted. A value of 0 is fully transparent and 1 is fully
    opaque.}

   \item{date.breaks}{Number of major x-axis intervals to use. The
    function will try and choose a sensible number of dates/times as
    well as formatting the date/time appropriately to the range being
    considered. This does not always work as desired automatically. The user can
    therefore increase or decrease the number of intervals by adjusting
    the value of \code{date.breaks} up or down. }

  \item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If
    \code{TRUE} titles and axis labels will automatically try and format
    pollutant names and units properly e.g.  by subscripting the
    \sQuote{2} in NO2.}

  \item{\dots}{Other graphical parameters e.g. pch = 16 for filled
    circles and to \code{cutData}.  For example, in the case
    of \code{cutData} the option \code{hemisphere = "southern"} }
}

\value{
  As well as generating the plot itself, \code{smoothTrend} also returns an object of class 
  ``openair''. The object includes three main components: \code{call}, the command used to 
  generate the plot; \code{data}, the data frame of summarised information used to make the 
  plot; and \code{plot}, the plot itself. If retained, e.g. using 
  \code{output <- smoothTrend(mydata, "nox")}, this output can be used to recover the data, reproduce 
  or rework the original plot or undertake further analysis.  

  An openair output can be manipulated using a number of generic operations, including 
  \code{print}, \code{plot} and \code{summarise}. See \code{\link{openair.generics}} 
  for further details.   
  
}


\details{
The \code{smoothTrend} function provides a flexible way of estimating
the trend in the concentration of a pollutant or other variable. Monthly
mean values are calculated from an hourly (or higher resolution) or
daily time series. There is the option to deseasonalise the data if
there is evidence of a seasonal cycle.

\code{smoothTrend} uses a Generalized Additive Model (GAM) from the
\code{\link{mgcv}} package to find the most appropriate level of
smoothing. The function is particularly suited to situations where trends
are not monotonic (see discussion with \code{\link{MannKendall}} for
more details on this). The \code{smoothTrend} function is particularly
useful as an exploratory technique e.g. to check how linear or
non-linear trends are.

95\% confidence intervals are shown by shading. Bootstrap estimates of
the confidence intervals are also available through the \code{simulate}
option. Residual resampling is used.

Trends can be considered in a very wide range of ways, controlled by
setting \code{type} - see examples below.
}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
%\note{ ~~further notes~~

%  ~Make other sections like Warning with
\section{Warning }{If the option \code{deseason = TRUE} is set and there
are missing data after aggregation to monthly values, missing months
will be linearly interpolated.}
%}

\seealso{\code{\link{MannKendall}} for an alternative method of
  calculating trends.}
\examples{
# load example data from package
data(mydata)

# trend plot for nox
smoothTrend(mydata, pollutant = "nox")

# trend plot by each of 8 wind sectors
\dontrun{smoothTrend(mydata, pollutant = "o3", type = "wd", ylab = "o3 (ppb)")}

# several pollutants, no plotting symbol
\dontrun{smoothTrend(mydata, pollutant = c("no2", "o3", "pm10", "pm25"), pch = NA)}

# percentiles
\dontrun{smoothTrend(mydata, pollutant = "o3", statistic = "percentile",
percentile = 95)}

# several percentiles with control over lines used
\dontrun{smoothTrend(mydata, pollutant = "o3", statistic = "percentile",
percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5))}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
