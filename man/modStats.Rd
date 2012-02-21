\name{modStats}
\alias{modStats}
\title{Calculate common model evaluation statistics}
\usage{
  modStats(mydata, mod = "mod", obs = "obs",
    type = "default", rank.name = NULL, ...)
}
\arguments{
  \item{mydata}{A data frame.}

  \item{mod}{Name of a variable in \code{mydata} that
  respresents modelled values.}

  \item{obs}{Name of a variable in \code{mydata} that
  respresents measured values.}

  \item{type}{\code{type} determines how the data are split
  i.e. conditioned, and then plotted. The default is will
  produce statistics using the entire data. \code{type} can
  be one of the built-in types as detailed in
  \code{cutData} e.g. "season", "year", "weekday" and so
  on. For example, \code{type = "season"} will produce four
  sets of statistics --- one for each season.

  It is also possible to choose \code{type} as another
  variable in the data frame. If that variable is numeric,
  then the data will be split into four quantiles (if
  possible) and labelled accordingly. If type is an
  existing character or factor variable, then those
  categories/levels will be used directly. This offers
  great flexibility for understanding the variation of
  different variables and how they depend on one another.

  More than one type can be considered e.g. \code{type =
  c("season", "weekday")} will produce statistics split by
  season and day of the week.}

  \item{rank.name}{Simple model ranking can be carried out
  if \code{rank.name} is supplied. \code{rank.name} will
  generally refer to a column representing a model name,
  which is to ranked. The ranking is based the Index of
  Agreement performance, as that indicator is arguably the
  best single model performance indicator available.}

  \item{...}{Other aruments to be passed to \code{cutData}
  e.g.  \code{hemisphere = "southern"}}
}
\value{
  Returns a data frame with model evaluation statistics.
}
\description{
  Function to calculate common numerical model evaluation
  statistics with flexible conditioning
}
\details{
  This function is under development and currently provides
  some common model evaluation statistics. These include
  (to be mathematically defined later):

  \itemize{

  \item \eqn{n}, the number of complete pairs of data.

  \item \eqn{FAC2}, fraction of predictions within a factor
  of two.

  \item \eqn{MB}, the mean bias.

  \item \eqn{MGE}, the mean gross error.

  \item \eqn{NMB}, the normalised mean bias.

  \item \eqn{NMGE}, the normalised mean gross error.

  \item \eqn{RMSE}, the root mean squared error.

  \item \eqn{r}, the Pearson correlation coefficient.

  \item \eqn{IOA}, the Index of Agreement based on Willmott
  et al. (2011), which spans between -1 and +1 with values
  approaching +1 representing better model performance.

  An IOA of 0.5, for example, indicates that the sum of the
  error-magnitudes is one half of the sum of the
  observed-deviation magnitudes.  When IOA = 0.0, it
  signifies that the sum of the magnitudes of the errors
  and the sum of the observed-deviation magnitudes are
  equivalent. When IOA = -0.5, it indicates that the sum of
  the error-magnitudes is twice the sum of the perfect
  model-deviation and observed-deviation magnitudes. Values
  of IOA near -1.0 can mean that the model-estimated
  deviations about O are poor estimates of the observed
  deviations; but, they also can mean that there simply is
  little observed variability - so some caution is needed
  when the IOA approaches -1.

  }

  All statistics are based on complete pairs of \code{mod}
  and \code{obs}.

  Conditioning is possible through setting \code{type}.
}
\examples{
## the example below is somewhat artificial --- assuming the observed
## values are given by NOx and the predicted values by NO2.

modStats(mydata, mod = "no2", obs = "nox")

## evaluation stats by season

modStats(mydata, mod = "no2", obs = "nox", type = "season")
}
\author{
  David Carslaw
}
\references{
  Willmott, C.J., Robeson, S.M., Matsuura, K., 2011. A
  refined index of model performance. International Journal
  of Climatology.
}
\keyword{methods}

