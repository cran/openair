\name{conditionalQuantile}
\alias{conditionalQuantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conditional quantile estimates for model evaluation}

\description{
Function to calculate conditional quantiles with flexible
conditioning. The function is for use in model evaluation and more
generally to help better understand forecast predictions and how well
they agree with observations.
}

\usage{
conditionalQuantile(mydata, obs = "obs", mod = "mod",
type = "default",
bins = 31,
min.bin = c(10, 20),
xlab = "predicted value",
ylab = "observed value",
col = brewer.pal(5, "YlOrRd"),
key.columns = 2,
key.position = "bottom",
auto.text = TRUE, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mydata}{A data frame containing the field \code{obs} and
  \code{mod} representing observed and modelled values.}

  \item{obs}{The name of the observations in \code{mydata}.}

  \item{mod}{The name of the predictions (modelled values) in \code{mydata}.}

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
  
  \item{bins}{Number of bins to be used in calculating the different
  quantile levels.}

 \item{min.bin}{The minimum number of points required for the estimates
 of the 25/75th and 10/90th percentiles.}

 \item{xlab}{label for the x-axis.}

\item{ylab}{label for the y-axis.}

  \item{col}{Colours to be used for plotting the uncertainty bands and
  median line. Must be of length 5 or more.}

  \item{key.columns}{Number of columns to be used in the key.}

  \item{key.position}{Location of the key e.g. "top", "bottom", "right",
  "left". See \code{lattice} \code{xyplot} for more details.}


 \item{auto.text}{ Either \code{TRUE} (default) or \code{FALSE}. If
          \code{TRUE} titles and axis labels etc. will automatically try
          and format pollutant names and units properly e.g.  by
          subscripting the `2' in NO2. }

  \item{\dots}{Other graphical parameters passed onto
    \code{lattice:xyplot} and \code{cutData}. For example, in the case
    of \code{cutData} the option \code{hemisphere = "southern"}.}
}

%\value{}

\details{The \code{conditionalQuantile} function is a useful approach
  for comparing continuous observations and predictions i.e. forecasts.

  The function requires a data frame consisting of a column of
  observations and a column of predictions. The observations are split up
  into \code{bins} according to values of the predictions. The median
  prediction line together with the 25/75th and 10/90th quantile values
  are plotted together with a line showing a "perfect" model. Also shown
  is a histogram of predicted values.

  Far more insight can be gained into model performance through
  conditioning using \code{type}. For example, \code{type = "season"}
  will plot conditional quantiles by each season. \code{type} can also
  be a factor or character field e.g. representing different models
  used.

  See Wilks (2005) for more details and the examples below.
}


\references{
Wilks, D. S., 2005. Statistical Methods in the Atmospheric Sciences,
Volume 91, Second Edition (International Geophysics), 2nd Edition.
Academic Press.


}
\author{David Carslaw}
%\note{ ~~further notes~~


%\section{Warning }{}

\seealso{See \code{\link{modStats}} for model evaluation statistics and
  the package \code{verification} for comprehensive functions for
  forecast verification.}
\examples{

# load example data from package
data(mydata)

## make some dummy prediction data based on 'nox'
mydata$mod <- mydata$nox*1.1 + mydata$nox * runif(1:nrow(mydata))

# basic conditional quantile plot
## A "perfect" model is shown by the blue line
## predictions tend to be increasingly positively biased at high nox,
## shown by departure of median line from the blue one.
## The widening uncertainty bands with increasing NOx shows that
## hourly predictions are worse for higher NOx concentrations.
## Also, the red (median) line extends beyond the data (blue line),
## which shows in this case some predictions are much higher than
## the corresponding measurements. Note that the uncertainty bands
## do not extend as far as the median line because there is insufficient
# to calculate them
conditionalQuantile(mydata, obs = "nox", mod = "mod")

## can split by season to show seasonal performance (not very
## enlightening in this case - try some real data and it will be!)

\dontrun{conditionalQuantile(mydata, obs = "nox", mod = "mod", type = "season")}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{trend}% __ONLY ONE__ keyword per line
