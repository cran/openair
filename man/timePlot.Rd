\name{timePlot}
\alias{timePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot time series}
\description{
  Plot time series quickly, perhaps for multiple pollutants, grouped or
  in separate panels.
}
\usage{
timePlot(mydata,
pollutant = "nox",
group = FALSE,
stack = FALSE,
normalise = FALSE,
avg.time = "default",
data.thresh = 0,
statistic = "mean",
percentile = NA,
date.pad = FALSE,
type = "default",
layout = c(1, 1),
cols = "brewer1",
main = "",
ylab = pollutant,
lty = 1:length(pollutant),
lwd = 1,
pch = NA,
key = TRUE,
strip = TRUE,
log = FALSE,
smooth = FALSE,
ci = TRUE,
key.columns = 1,
name.pol = pollutant,
date.breaks = 7,
auto.text = TRUE, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mydata}{A data frame of time series. Must include a \code{date}
field and at least one variable to plot.}

  \item{pollutant}{Name of variable to plot. Two or more pollutants can be
    plotted, in which case a form like \code{pollutant = c("nox", "co")}
    should be used.}

  \item{group}{If more than one pollutant is chosen, should they all be
    plotted on the same graph together? The default is \code{FALSE}, which
    means they are plotted in separate panels with their own scaled. If
    \code{TRUE} then they are plotted on the same plot with the same
    scale.}

  \item{stack}{If \code{TRUE} the time series will be stacked by
    year. This option can be useful if there are several years worth of
    data making it difficult to see much detail when plotted on a single
    plot.}

  \item{normalise}{Should variables be normalised? The default is
    \code{FALSE}. If \code{TRUE} then the variable(s) are divided by
    their mean values. This helps to compare the shape of the diurnal
    trends for variables on very different scales.}

 \item{avg.time}{This defines the time period to average to. Can be "sec",
  "min", "hour", "day", "DSTday", "week", "month", "quarter" or
  "year". For much increased flexibility a number can precede these
  options followed by a space. For example, a timeAverage of 2 months
  would be \code{period = "2 month"}. See function \code{timeAverage} for
  further details on this.}

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
  deviation and "frequency" is the number (frequency) of valid records
  in the period. "percentile" is the percentile level (\%) between
  0-100, which can be set using the "percentile" option - see
  below. Not used if
  \code{avg.time = "default"}.}

\item{percentile}{The percentile level in \% used when \code{statistic =
    "percentile"} and when aggregating the data with
    \code{avg.time}. More than one percentile level is allowed for
    \code{type = "default"} e.g. \code{percentile = c(50, 95)}. Not used
    if \code{avg.time = "default"}.}

\item{date.pad}{Should missing data be padded-out? This is useful where
  a data frame consists of two or more "chunks" of data with time gaps
  between them. By setting \code{date.pad = TRUE} the time gaps between
  the chunks are shown properly, rather than with a line connecting each
  chunk. For irregular data, set to \code{FALSE}. Note, this should not
  be set for \code{type} other than \code{default}.}

  \item{type}{The type of analysis to be done. The default is will
produce a single plot using the entire data. There is an option "site",
which requires a slightly different format. If "site" is used a data
frame with headings \code{date}, a pollutant and \code{site} is
required. It is also possible to choose \code{type} as another variable
in the data frame. For example, \code{type = "o3"} will plot four
quantiles of levels of ozone. This offers great flexibility for
understanding the variation of different variables dependent on
another. See function \code{cutData} for more details on the flexibility
here.}

  \item{layout}{Determines how the panels are laid out. By default,
    plots will be shown in one column with the number of rows equal to the
    number of pollutants, for example. If the user requires 2 columns and
    two rows, layout should be set to \code{layout = c(2, 2)}. In general,
    layout is expressed as number of columns times number of rows.}

  \item{cols}{Colours to be used for plotting. Options include "default",
    "increment", "heat", "spectral", "hue", "brewer1" (default) and user
    defined (see manual for more details). The same line colour can be
    set for all pollutant e.g. \code{cols = "black"}.}

  \item{main}{The plot title; default is no title.}

  \item{ylab}{Name of y-axis variable. By default will use the name of
    \code{pollutant}(s).}

  \item{lty}{The line type used for plotting. Default is to provide
    different line types for different pollutant. If one requires a
    continuous line for all pollutants, the set \code{lty = 1}, for
    example. See \code{lty} option for standard plotting.}

  \item{lwd}{The line width used; default is 1. To set a wider line for
    all pollutant the choose, for example, \code{lwd = 2}. Alternatively,
    varying line widths can be chosen depending on the pollutant. For
    example, if \code{pollutant = c("nox", "no2", "o3")}, and a wider line
    was wanted for ozone, then \code{lwd} can be expressed as \code{lwd =
      c(1, 1, 2)}.}

  \item{pch}{The type of symbol to be plotted. The default is not to
  plot a symbol. It can be useful to do so in cases where there are not
  consecutive points in time and a line cannot be drawn between two
  points. Symbols can be plotted as a vector of types e.g. for unfilled
  and filled symbols use \code{pch = c(1, 16)}.}

  \item{key}{Should a key be drawn? The default is \code{TRUE}.}

  \item{strip}{Should a strip be drawn? The default is \code{TRUE}.}

   \item{log}{Should the y-axis appear on a log scale? The default is
   \code{FALSE}. If \code{TRUE} a well-formatted log10 scale is
   used. This can be useful for plotting data for several different
   pollutants that exist on very different scales. It is therefore
   useful to use \code{log = TRUE} together with \code{group = TRUE}.}

 \item{smooth}{Should a smooth line be applied to the data? The default
   is \code{FALSE}.}

 \item{ci}{If a smooth fit line is applied, then \code{ci} determines
 whether the 95\% confidence intervals aer shown.}

  \item{key.columns}{Number of columns to be used in the key. With many
  pollutants a single column can make to key too wide. The user can thus
  choose to use several columns by setting \code{columns} to be less
  than the number of pollutants.}

\item{name.pol}{This option can be used to give alternative names for
  the variables plotted. Instead of taking the column headings as names,
  the user can supply replacements. For example, if a column had the
  name "nox" and the user wanted a different description, then setting
  \code{pol.name = "nox before change"} can be used. If more than one
  pollutant is plotted then use \code{c} e.g. \code{pol.name = c("nox
    here", "o3 there")}.}

    \item{date.breaks}{Number of major x-axis intervals to use. The
    function will try and choose a sensible number of dates/times as
    well as formatting the date/time appropriately to the range being
    considered. This does not always work as desired automatically. The user can
    therefore increase or decrease the number of intervals by adjusting
    the value of \code{date.breaks} up or down. }

  \item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE}
    titles and axis labels will automatically try and format pollutant
    names and units properly e.g.  by subscripting the `2' in NO2.}

  \item{\dots}{Other graphical parameters.}
}

\value{
  As well as generating the plot itself, \code{timePlot} also returns an object of class 
  ``openair''. The object includes three main components: \code{call}, the command used to 
  generate the plot; \code{data}, the data frame of summarised information used to make the 
  plot; and \code{plot}, the plot itself. If retained, e.g. using 
  \code{output <- timePlot(mydata, "nox")}, this output can be used to recover the data, reproduce 
  or rework the original plot or undertake further analysis.  

  An openair output can be manipulated using a number of generic operations, including 
  \code{print}, \code{plot} and \code{summarise}. See \code{\link{openair.generics}} 
  for further details.   
  
}



\details{
  The \code{timePlot} is the basic time series plotting function in
  \code{openair}. Its purpose is to make it quick and easy to plot time
  series for pollutants and other variables. The other purpose is to plot
  potentially many variables together in as compact a way as possible.

  The function is flexible enough to plot more than one variable at
  once. If more than one variable is chosen plots it can either show all
  variables on the same plot (with different line types) \emph{on the
    same scale}, or (if \code{group = FALSE}) each variable in its own
  panels with its own scale.

  The general preference is not to plot two variables on the same graph
  with two different y-scales. It can be misleading to do so and
  difficult with more than two variables. If there is in interest in
  plotting several variables together that have very different scales,
  then it can be useful to normalise the data first, which can be down be
  setting \code{normalise = TRUE}. This option ensures that each variable
  is divided by its mean and makes it easy to plot two or more variables
  on the same plot - generally with \code{group = TRUE}.

  The user has fine control over the choice of colours, line width and
  line types used. This is useful for example, to emphasise a particular
  variable with a specific line type/colour/width.

  \code{timePlot} works very well with \code{\link{selectByDate}},
  which is used for selecting particular date ranges quickly and
  easily. See examples below.

  By default plots are shown with a colour key at the bottom and in teh
  case of multiple pollutants or sites, strips on teh left of each
  plot. Sometimes this may be overkill and the user can opt to remove
  the key and/or the strip by setting \code{key} and/or \code{strip} to
  \code{FALSE}. One reason to do this is to maximise the plotting area
  and therefore the information shown.
}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}

\seealso{\code{\link{MannKendall}}, \code{\link{smoothTrend}},
  \code{\link{linearRelation}}, \code{\link{selectByDate}} and
  \code{\link{timeAverage}} for details on selecting averaging times
  and other statistics in a flexible way}

\examples{

# basic use, single pollutant
timePlot(mydata, pollutant = "nox")

# two pollutants in separate panels
timePlot(mydata, pollutant = c("nox", "no2"))

# two pollutants in the same panel with the same scale
timePlot(mydata, pollutant = c("nox", "no2"), group = TRUE)

# alternative by normalising concentrations and plotting on the same
  scale
timePlot(mydata, pollutant = c("nox", "no2"), group = TRUE, normalise =
  TRUE)

# examples of selecting by date

# plot for nox in 1999
timePlot(selectByDate(mydata, year = 1999), pollutant = "nox")

# select specific date range for two pollutants
timePlot(selectByDate(mydata, start = "6/8/2003", end = "13/8/2003"),
pollutant = c("no2", "o3"))

# choose different line styles etc
timePlot(mydata, pollutant = c("nox", "no2"), lty = 1)

# choose different line styles etc
timePlot(selectByDate(mydata, year = 2004, month = 6), pollutant =
c("nox", "no2"), lwd = c(1, 2), col = "black")

# different averaging times

#daily mean O3
timePlot(mydata, pollutant = "o3", avg.time = "day")

# daily mean O3 ensuring each day has data capture of at least 75\%
timePlot(mydata, pollutant = "o3", avg.time = "day", data.thresh = 75)

# 2-week average of O3 concentrations
timePlot(mydata, pollutant = "o3", avg.time = "2 week")
}
\keyword{methods}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
