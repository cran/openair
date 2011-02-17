\name{modStats}
\alias{modStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate common model evaluation statistics}

\description{
  Function to calculate common numerical model evaluation statistics
  with flexible conditioning
}
\usage{
results <- modStats(mydata,  mod = "mod", obs = "obs", type = "default",
  ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{mydata}{A data frame.}

\item{mod}{Name of a variable in \code{mydata} that respresents
  modelled values.}

\item{obs}{Name of a variable in \code{mydata} that respresents
  measured values.}

\item{type}{\code{type} determines how the data are split
    i.e. conditioned, and then plotted. The default is will produce
    statistics using the entire data. \code{type} can be one of the built-in
    types as detailed in \code{cutData} e.g. "season", "year", "weekday"
    and so on. For example, \code{type = "season"} will produce four
    sets of statistics --- one for each season.

    It is also possible to choose \code{type} as another variable in the
    data frame. If that variable is numeric, then the data will be split
    into four quantiles (if possible) and labelled accordingly. If type
    is an existing character or factor variable, then those
    categories/levels will be used directly. This offers great
    flexibility for understanding the variation of different variables
    and how they depend on one another.

    More than one type can be considered e.g. \code{type = c("season",
    "weekday")} will produce statistics split by season and day of the
  week.}

\item{...}{Other aruments to be passed to \code{cutData}
  e.g. \code{hemisphere = "southern"}}

}

\details{This function is under development and currently provides some
  common model evaluation statistics. These include (to be
  mathematically defined later):

  \itemize{

    \item \eqn{n}, the number of complete pairs of data.

    \item \eqn{FAC2}, fraction of predictions within a factor of two.

    \item \eqn{MB}, the mean bias.

    \item \eqn{MGE}, the mean gross error.

    \item \eqn{NMB}, the normalised mean bias.

    \item \eqn{NMGE}, the normalised mean gross error.

    \item \eqn{RMSE}, the root mean squared error.

    \item \eqn{r}, the Pearson correlation coefficient.
    
  }
  

  All statistics are based on complete pairs of \code{mod} and
  \code{obs}.

  Conditioning is possible through setting \code{type}.

  }

\value{Returns a data frame with model evaluation statistics.}

%\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}
%\note{ ~~further notes~~


\examples{
## the example below is somewhat artificial --- assuming the observed
## values are given by NOx and the predicted values by NO2.

modStats(mydata, mod = "no2", obs = "nox")

## evaluation stats by season

modStats(mydata, mod = "no2", obs = "nox", type = "season")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{}% __ONLY ONE__ keyword per line
