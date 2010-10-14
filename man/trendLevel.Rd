\name{trendLevel}

\alias{trendLevel}
\alias{trendLevelHour}
\alias{trendLevelWd}

\title{ trendLevel }

\description{
  The trendLevel function provides a way of rapidly showing a large amount of 
  data in a condensed form. In one plot, the variation in the concentration of 
  one pollutant can to shown as a function of three other properties. The default 
  version of the plot uses y = time of day, x = month of year and type = year to 
  provide information on trends, seasonal effects and diurnal variations. However, x, 
  y and type and summarising statistics can all be modified to provide a range 
  of other similar plots. 
}

\usage{
trendLevel(mydata,
    pollutant = "nox", 
    x = "month", y = "hour", type = "year", 
    xlab = x, ylab = y, typelab = NULL, 
    limits = c(0, 100), cols = "default", auto.text = TRUE, 
    key.header = "use.stat.name", key.footer = pollutant, 
    key.position = "right", key = NULL,
    statistic = c("mean", "max", "frequency"), stat.args = NULL, 
    stat.safe.mode = TRUE, drop.unused.types = TRUE,
    ...)		
}


\arguments{

  \item{mydata}{ The openair data frame to use to generate the \code{trendLevel} plot. }

  \item{pollutant}{ The name of the data series in \code{mydata} to sample to produce the 
  \code{trendLevel} plot. }

  \item{x}{ The name of the data series to use as the \code{trendLevel} x-axis. This is used with 
  the \code{y} and \code{type} options to bin the data before applying \code{statistic} (see 
  below). Allowed options currently include \code{"hour"}, \code{"month"}, \code{"year"}, and 
  \code{"wd"}. Other data series in \code{mydata} can also be used. (Note: \code{trendLevel} 
  does not allow duplication in \code{x}, \code{y} and \code{type} options 
  within a call.) }

  \item{y, type}{ The names of the data series to use as the \code{trendLevel} y-axis and for additional conditioning, 
  respectively. As \code{x} above. }

  \item{xlab, ylab, typelab}{ The x-axis, y-axis and type conditioning labels. All labels are passed 
  via \code{quickText} to handle formatting if enabled (\code{auto.text = TRUE}). By default \code{trendLevel} 
  uses x and y names as xlab and ylab, respectively, but does not attach a label to type cases.}

  \item{limits}{ The colour scale range to use when generating the \code{trendLevel} plot. }

  \item{cols}{ The colour set to use to colour the \code{trendLevel} surface. \code{cols} is passed 
  to \code{openColours} for evaluation. See \code{?openColours} for more details. }

  \item{auto.text}{ Automatic routine text formatting. \code{auto.text = TRUE} allows labels 
  (\code{xlab}, \code{ylab}, \code{main}, etc.) to be passed to the plot via \code{quickText}. 
  \code{auto.text = FALSE} turns this option off and passes labels to the plot without modification. }

  \item{key.header, key.footer}{Adds additional text labels above and/or 
  below the scale key, respectively. For example, passing the options
  \code{key.header = "", key.footer = c("mean","nox")} adds 
  the addition text as a scale footer. If enabled (\code{auto.text = TRUE}), 
  these arguments are passed to the scale key (\code{drawOpenKey}) via \code{quickText} 
  to handle formatting. The term \code{"get.stat.name"}, used as the default 
  \code{key.header} setting, is reserved and automatically adds statistic function 
  names or defaults to \code{"level"} when unnamed functions are requested via 
  \code{statistic}. }

  \item{key.position}{Location where the scale key should be plotted. 
  Allowed arguments currently include \code{"top"}, \code{"right"}, 
  \code{"bottom"} and \code{"left"}.}

  \item{key}{Fine control of the scale key via \code{drawOpenKey}. See 
  \code{?drawOpenKey} for further details.}

  \item{statistic}{ The statistic method to be use to summarise locally binned \code{pollutant} 
  measurements with. Three options are currently encoded: \code{"mean"} (default), \code{"max"} 
  and \code{"frequency"}. (Note: Functions can also be sent directly via \code{statistic}. 
  However, this option is still in development and should be used with caution. See Details below.)  }

  \item{stat.args}{ Additional options to be used with \code{statistic} if this is a function. The 
  extra options should be supplied as a list of named parameters. (see Details below.) }

  \item{stat.safe.mode}{ An addition protection applied when using functions direclty with 
  \code{statistic} that most users can ignore. This option returns \code{NA} instead of running 
  \code{statistic} on binned subsamples that are empty. Many common functions terminate 
  with an error meassage when applied to an empty dataset. So, this options provides a mechanism to work 
  with such functions. For a very few cases, e.g. for a function that counted missing entries, it might 
  need to be set to \code{FALSE} (see Details below.) }

  \item{drop.unused.types}{ Hide unused/empty \code{type} conditioning cases. Some conditioning options may 
  generate empty cases for some data sets, e.g. a hour of the day when no measurements were taken. Empty  
  \code{x} and \code{y} cases generate 'holes' in individual plots. However, empty \code{type} cases 
  would produce blank panels if plotted. Therefore, the default, \code{TRUE}, excludes these empty panels 
  from the plot. The alternative \code{FALSE} plots all \code{type} panels. }

  \item{...}{ Addition options passed to \code{levelplot} in \code{lattice} for evaluation. 
  Note: \code{trendLevel} sets several \code{levelplot} options, and, therefore, checks and 
  modifies some options before passing them on. For example, 
  \code{main} and \code{strip} are both locally adjusted. Like other labelling, \code{main} is 
  passed via quickText to handle formatting if it is set and local formatting is enabled 
  (\code{auto.text = TRUE}). \code{strip} provides fine control of \code{typelab} and the associated 
  plot strip. If called, \code{strip} options override \code{typelab} and \code{trendLevel} 
  control of the plot strip.}

}

\value{
  As well as generating the plot itself, \code{trendLevel} also returns an object (a list) with 
  two parts: \code{data}, a data frame of the summarised information used to make the plot, and 
  \code{plot}, the plot itself. 
}

\details{
  \code{trendLevel} allows the use of third party summarising functions via the 
  \code{statistic} option. Any additional function arguments not included within 
  a function called using \code{statistic} should be supplied as a list of named 
  parameters and sent using \code{stat.args}. For example, the encoded option 
  \code{statistic = "mean"} is equivalent to \code{statistic = mean, 
  stat.args = list(na.rm = TRUE)} or the R command \code{mean(x, na.rm= TRUE)}. 
  Many R functions and user`s own code could be 
  applied in a similar fashion, subject to the following restrictions: the first 
  argument sent to the function must be the data series to be analysed; the name `x' 
  cannot be used for any of the extra options supplied in \code{stat.args}; and the 
  function should return the required answer as a numeric or NA. Note: 
  If the supplied function returns more than one answer, currently only the first of these 
  is retained and used by \code{trendLevel}. All other returned information will be 
  ignored without warning. If the function terminates with an error when it is sent an empty 
  data series, the option \code{stat.safe.mode} should not be set to \code{FALSE} or 
  \code{trendLevel} may fail. Note: The \code{stat.safe.mode = TRUE} option returns an NA 
  without warning for empty data series. 
      
}

\author{ David Carslaw and Karl Ropkins }

\examples{

#basic use
#default statistic = "mean"
trendLevel(mydata, pollutant = "nox")

#applying same as 'own' statistic
my.mean <- function(x) mean(x, na.rm = TRUE)
trendLevel(mydata, pollutant = "nox", statistic = my.mean)

#alternative for 'third party' statistic  
#trendLevel(mydata, pollutant = "nox", statistic = mean, 
#           stat.args = list(na.rm = TRUE))

}

\keyword{methods}
