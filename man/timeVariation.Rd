\name{timeVariation}
\alias{timeVariation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diurnal and day of week variation}
\description{
  Plots the diurnal and day of the week variation for different variables,
  typically pollutant concentrations. Three separate plots are produced.
}
\usage{
timeVariation(mydata,
pollutant = "nox",
local.time = FALSE,
normalise = FALSE,
ylab = pollutant,
xlab = NULL,
name.pol = pollutant,
type = "default",
ci = TRUE,
cols = "hue",
main = "",
key = NULL,
key.columns = 1,
auto.text = TRUE,
alpha = 0.4,...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{A data frame of hourly (or higher temporal resolution
    data). Must include a \code{date} field and at least one variable to
    plot.}

  \item{pollutant}{Name of variable to plot. Two or more pollutants can be
    plotted, in which case a form like \code{pollutant = c("nox", "co")}
    should be used.}

  \item{local.time}{Should the results be calculated in local time? The
    default is \code{FALSE}. Emissions activity tends to occur at local
    time e.g. rush hour is at 8 am every day. When the clocks go forward
    in spring, the emissions are effectively released into the
    atmosphere at BST - 1 hour during the summer. When plotting
    diurnal profiles, this has the effect of "smearing-out" the
    concentrations. Sometimes, a better approach is to express time as
    local time, which here is defined as BST (British Summer Time). This
    correction tends to produce better-defined diurnal profiles of
    concentration (or other variables) and allows a better comparison to
    be made with emissions/activity data. If set to \code{FALSE} then
    GMT is used.}

  \item{normalise}{Should variables be normalised? The default is
    \code{FALSE}. If \code{TRUE} then the variable(s) are divided by
    their mean values. This helps to compare the shape of the diurnal
    trends for variables on very different scales.}

  \item{ylab}{Name of y-axis variable. By default will use the name of
    \code{pollutant}.}

   \item{xlab}{x-axis label.}

  \item{name.pol}{Names to be given to the pollutant(s). This is useful
    if you want to give a fuller description of the variables, maybe
    also including subscripts etc.}

  \item{type}{Can either be \code{default} or \code{site}. If
    \code{site} is chosen then the data frame must contain a field named
    \code{site}. The \code{openair} manual shows how the \code{site}
    field can be used in a very flexible way. It is also possible to
    choose \code{type} as another variable in the data frame. For
    example, \code{type = "o3"} will plot four windRoses for different
    levels of ozone, split into four quantiles (approximately equal
    numbers of counts in each of the four splits). This offers great
    flexibility for understanding the variation of different variables
    dependent on another. See function \code{cutData} for further
    details.}

  \item{ci}{Should confidence intervals be shown? The default is
    \code{TRUE}. Setting this to \code{FALSE} can be useful if multiple
    pollutants are chosen where over-lapping confidence intervals can
    over complicate plots.}

  \item{cols}{Colours to be used for plotting. Options include
"default", "increment", "heat", "spectral", "hue" (default) and user
defined (see manual for more details).}

  \item{main}{The plot title; default is no title.}

  \item{key}{By default \code{timeVariation} produces four plots on
  one page. While it is useful to see these plots together, it is
  sometimes necessary just to use one for a report. If \code{key} is
  \code{TRUE}, a key is added to all plots allowing the extraction of a
  single plot \emph{with} key. See below for an example.}

 \item{key.columns}{Number of columns to be used in the key. With many
  pollutants a single column can make to key too wide. The user can thus
  choose to use several columns by setting \code{columns} to be less
  thna the number of pollutants.}

  \item{auto.text}{Either \code{TRUE} (default) or \code{FALSE}. If \code{TRUE}
    titles and axis labels will automatically try and format pollutant
    names and units properly e.g.  by subscripting the `2' in NO2.}

  \item{alpha}{The alpha transparency used for plotting confidence
    intervals. 0 is fully transparent and 1 is opaque. The default is 0.4}

  \item{\dots}{Other graphical parameters.}
}

\details{
  The variation of pollutant concentrations by hour of the day and day of
  the week can reveal many interesting features that relate to source
  types. For traffic sources, there are often important differences in the
  way vehicles vary by vehicles type e.g. less heavy vehicles at weekends.

  The \code{timeVariation} function makes it easy to see how
  concentrations (and many other variable types) vary by hour of the day
  and day of the week.

  The plots also show the 95\% confidence intervals in the mean, which is
  particularly useful for comparing two different pollutants whose
  concentrations have been normalised.

  Note also that the \code{timeVariation} function works well on a subset
  of data and in conjunction with other plots. For example, a
  \code{\link{polarPlot}} may highlight an interesting feature for a
  particular wind speed/direction range. By filtering for those conditions
  \code{timeVariation} can help determine whether the temporal variation
  of that feature differs from other features --- and help with source
  identification.

  In addition, \code{timeVariation} will work well with other variables if
  available. Examples include meteorological and traffic flow data.

  The option \code{type = "site"} provides great flexibility. For
  example, looking a variations across multiple sites, by different time
  periods. The latter is particularly useful to see if diurnal
  variations etc. vary over time. See the full documentation for details
  of use.

  Results are returned as a list - both the numerical and individual
  plots. See examples below for details.
}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}

\seealso{\code{\link{polarPlot}}, \code{\link{linearRelation}}}

  \examples{

# basic use
timeVariation(mydata, pollutant = "nox")

# for a subset of conditions
timeVariation(subset(mydata, ws > 3 & wd > 100 & wd < 270),
pollutant = "pm10", ylab = "pm10 (ug/m3)")

# multiple pollutants with concentrations normalised
timeVariation(mydata, pollutant = c("nox", "co"), normalise = TRUE)

# show BST/GMT variation (see ?cutData for more details)
# the NOx plot shows the profiles are very similar when expressed in
# local time, showing that the profile is dominated by a local source
# that varies by local time and not by GMT i.e. road vehicle emissions

timeVariation(mydata, pollutant = "nox", type = "gmtbst")

# By contrast, a variable such as wind speed shows a clear shift when
#  expressed in local time. These two plots can help show whether the
#  variation is dominated by man-made influences or natural processes

timeVariation(mydata, pollutant = "ws", type = "gmtbst")

## sub plots can be extracted, in which case set key = TRUE
## plots are retruned in a list e.g. to get the diurnal plot of NO2:
results <- timeVariation(mydata, pollutant = "no2", key = TRUE)
results[[6]] ## plots diurnal plot on its own

## results[[5]] is the weekday and hour plot
## results[[7]] is the weekday plot
## results[[8]] is the monthly plot

## numerical results (mean, lower/upper uncertainties)
## results[[1]] are the weekday and hour results
## results[[2]] are the diurnal results
## results[[3]] are the weekday results
## results[[4]] are the monthly results

}
\keyword{methods}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
