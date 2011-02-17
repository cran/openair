\name{MannKendall}
\alias{MannKendall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mann-Kendall trend tests}

\description{
Mann-Kendall tests for trend with Sen-Theil slope estimates.
}

\usage{
MannKendall(mydata,
pollutant = "nox",
deseason = FALSE,
type = "default",
avg.time = "month",
statistic = "mean",
percentile = NA,
data.thresh = 0,
simulate = FALSE,
alpha = 0.05,
dec.place = 2,
ylab = pollutant,
xlab = "year",
main = "",
auto.text = TRUE,
autocor = FALSE,
slope.percent = FALSE,
date.breaks = 7, ...)
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
  
  \item{avg.time}{Either "month" (the default), or "year". Determines
whether monthly mean or annual mean trends are plotted. Note that for
"annual", six or more years are required.}

 \item{statistic}{Statistic used for calculating monthly
   values. Default is \code{"mean"}, but can also be
   \code{"percentile"}. See \code{time.average} for more details.}

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

  \item{alpha}{For the confidence interval calculations of the
slope. The default is 0.05. To show 99\% confidence intervals for the
value of the trend, choose alpha = 0.01 etc.}

  \item{dec.place}{The number of decimal places to display the trend
  estimate at. The default is 2.}

\item{ylab}{y-axis label.}

 \item{xlab}{x-axis label.}

  \item{main}{Title of plot, if required.}

  \item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If
  \code{TRUE} titles and axis labels will automatically try and format
  pollutant names and units properly e.g.  by subscripting the
  \sQuote{2} in NO2.}

  \item{autocor}{Should autocorrelation be considered in the trend
uncertainty estimates? The default is \code{FALSE}. Generally,
accounting for autocorrelation increases the uncertainty of the trend
estimate - sometimes by a large amount.}

\item{slope.percent}{Should the slope and the slope uncertainties be
  expressed as a percentage change per year? The default is \code{FALSE}
  and the slope is expressed as an average units/year change
  e.g. ppb. Percentage changes can often be confusing and should be
  clearly defined. Here the percentage change is expressed as 100 *
  (C.end/C.start - 1) / (end.year - start.year). Where C.start is the
  concentration at the start date and C.end is the concentration at the
  end date.

  For \code{period = "annual"} (end.year - start.year) will be the total
  number of years - 1. For example, given a concentration in year 1 of
  100 units and a percentage reduction of 5\%/yr, after 5 years there
  will be 75 units but the actual time span will be 6 years i.e. year 1
  is used as a reference year. Things are slightly different for monthly
  values e.g. \code{period = "month"}, which will use the total number
  of months as a basis of the time span and is therefore able to deal
  with partial years. There can be slight differences in the \%/yr trend
  estimate therefore, depending on whether monthly or annual values are
  considered.}

 \item{date.breaks}{Number of major x-axis intervals to use. The
    function will try and choose a sensible number of dates/times as
    well as formatting the date/time appropriately to the range being
    considered. This does not always work as desired automatically. The user can
    therefore increase or decrease the number of intervals by adjusting
    the value of \code{date.breaks} up or down. }

  \item{\dots}{Other graphical parameters passed onto
    \code{lattice:xyplot} and \code{cutData}. For example, in the case
    of \code{cutData} the option \code{hemisphere = "southern"}.}
}

\value{
  As well as generating the plot itself, \code{MannKendall} also returns an object of class 
  ``openair''. The object includes three main components: \code{call}, the command used to 
  generate the plot; \code{data}, the data frame of summarised information used to make the 
  plot; and \code{plot}, the plot itself. If retained, e.g. using 
  \code{output <- MannKendall(mydata, "nox")}, this output can be used to recover the data, reproduce 
  or rework the original plot or undertake further analysis.  

  An openair output can be manipulated using a number of generic operations, including 
  \code{print}, \code{plot} and \code{summary}. See \code{\link{openair.generics}} 
  for further details.

  The \code{data} component of the \code{MannKendall} output includes to subsets: 
  \code{main.data}, the monthly data \code{res2} the trend
  statistics. For \code{output <- MannKendall(mydata, "nox")}, these can be extracted as 
  \code{object$data$main.data} and \code{object$data$res2}, respectively. 

  Note: In the case of the intercept, it is assumed the
  y-axis crosses the x-axis on 1/1/1970.
  
}

\details{The \code{Mann-Kendall} function provides a collection of
functions to analyse trends in air pollution data. The Mann-Kendall test
is a commonly used test in environmental sciences to detect the presence
of a trend. It is often used with the Sen-Theil (or just Sen) estimate
of slope. See references.  The \code{Mann-Kendall} function is flexible
in the sense that it can be applied to data in many ways e.g. by day of
the week, hour of day and wind direction. This flexibility makes it much
easier to draw inferences from data e.g. why is there a strong downward
trend in concentration from one wind sector and not another, or why
trends on one day of the week or a certain time of day are unexpected.

The Mann-Kendall test from trend is for data that are \emph{monotonic} -
see \url{http://en.wikipedia.org/wiki/Monotonic_function}. The most
appropriate use for this function is for data that are
\sQuote{well-behaved} i.e. tend to be steadily increasing or decreasing
or steady. For data that are strongly seasonal, perhaps from a
background site, or a pollutant such as ozone, it will be important to
deseasonalise the data (using the option \code{deseason =
TRUE}.Similarly, for data that increase, then decrease, or show sharp
changes it may be better to use \code{\link{smoothTrend}}.

Some of the code used in \code{MannKendall} is based on that from Rand
Wilxox \url{http://www-rcf.usc.edu/~rwilcox/}. This mostly relates to
the Sen-Theil slope estimates and uncertainties.  Further modifications
have been made to take account of correlated data based on Kunsch (1989).

The slope estimate and confidence intervals in the slope are plotted and
numerical information presented.

The basic function have been adapted to take account of auto-correlated
data using block bootstrap simulations (Kunsch, 1989). The principal
reason for doing so is to gain a better estimate of trend uncertainty.
}


\references{

Helsel, D., Hirsch, R., 2002. Statistical methods in water resources. US
Geological Survey.  \url{http://pubs.usgs.gov/twri/twri4a3/}. Note that
this is a very good resource statistics as applied to environmental
data.

Hirsch, R. M., Slack, J. R., Smith, R. A., 1982. Techniques of trend analysis for monthly
water-quality data. Water Resources Research 18 (1), 107-121.

Kunsch, H. R., 1989. The jackknife and the bootstrap for general stationary observations.
Annals of Statistics 17 (3), 1217-1241.

\dots see also several of the Air Quality Expert Group (AQEG) reports
for the use of similar tests applied to UK/European air quality data,
see \url{http://www.defra.gov.uk/ENVIRONMENT/airquality/panels/aqeg/}.

}
\author{David Carslaw with trend code from Rand Wilcox}
%\note{ ~~further notes~~


\section{Warning }{If \code{deseason = TRUE}, missing data will be
linearly interpolated.}

\seealso{See \code{\link{smoothTrend}} for a flexible approach to
estimating trends using nonparametric regression. The
\code{smoothTrend} function is suitable for cases where trends are not
monotonic and is probably better for exploring the shape of trends.}
\examples{

# load example data from package
data(mydata)

# trend plot for nox
MannKendall(mydata, pollutant = "nox")

# trend plot for ozone with p=0.01 i.e. uncertainty in slope shown at
# 99 % confidence interval

\dontrun{MannKendall(mydata, pollutant = "o3", ylab = "o3 (ppb)", alpha = 0.01)}

# trend plot by each of 8 wind sectors
\dontrun{MannKendall(mydata, pollutant = "o3", type = "wd", ylab = "o3 (ppb)")}

# and for a subset of data (from year 2000 onwards)
\dontrun{MannKendall(select.by.date(mydata, year = 2000:2005), pollutant = "o3", ylab = "o3 (ppb)")}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{trend}% __ONLY ONE__ keyword per line
