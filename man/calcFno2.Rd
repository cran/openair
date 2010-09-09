\name{calcFno2}
\alias{calcFno2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate NO2/NOX emission ratios from monitoring data}
\description{
Given hourly NOX and NO2 from a roadside site and hourly NOX,
NO2 and O3 from a background site the function will estimate the
emissions ratio of NO2/NOX --- the level of primary NO2
}

\usage{
calcFno2(input,
tau = 60,
plot = TRUE,
user.fno2,
main = "",
xlab = "year",
...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{A data frame with the following fields. \code{nox}
    and\code{no2} (roadside NOX and NO2 concentrations),
    \code{backnox}, \code{backno2} and \code{backo3} (hourly
    background concentrations of each pollutant). In addition
    \code{temp} (temperature in degrees Celsius) and \code{cl} (cloud
    cover in Oktas). Note that if \code{temp} and \code{cl} are not
    available, typical means values of 11 deg. C and cloud = 3.5 will be
    used.}

  \item{tau}{Mixing time scale. It is unlikely the user will need to
    adjust this. See details below.}

  \item{plot}{Should a plot be produced? The default is \code{TRUE} and
    will plot the trend in f-NO2 as a monthly time series.}

  \item{user.fno2}{User-supplied f-NO2 fraction e.g. 0.1 is a
    NO2/NOX ratio of 10\% by volume. \code{user.no2} will be
    applied to the whole time series and is useful for testing "what if"
    questions.}

  \item{main}{Title of plot if required.}


  \item{xlab}{x-axis label.}

  \item{\dots}{Other graphical parameters.}
}

\details{ The principal purpose of this function is to estimate the
 level of primary (or direct) NO2 from road vehicles. When hourly data
 of NOX, NO2 and O3 are available, the total oxidant method of Clapp and
 Jenkin (2001) can be used. If roadside O3 measurements are available
 see \code{\link{linearRelation}} for details of how to estimate the
 primary NO2 fraction.

 In the absence of roadside O3 measurements, it is rather more
 problematic to calculate the fraction of primary NO2. Carslaw and
 Beevers (2005c) developed an approach based on
 \code{\link{linearRelation}} the analysis of roadside
 and background measurements. The increment in roadside NO2
 concentrations is primarily determined by direct emissions of NO2 and
 the availability of One to react with NO to form NO2. The method aims
 to quantify the amount of NO2 formed through these two processes by
 seeking the optimum level of primary NO2 that gives the least error.

Test data is provided at \url{http://www.openair-project.org}.
}
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }

\references{
Clapp, L.J., Jenkin, M.E., 2001. Analysis of the relationship
between ambient levels of O3, NO2 and NO as a function of
NOX in the UK. Atmospheric Environment 35 (36),
6391-6405.

  Carslaw, D.C. and N Carslaw (2007).  Detecting and characterising small
changes in urban nitrogen dioxide concentrations.  Atmospheric
Environment.  Vol. 41, 4723-4733.

Carslaw, D.C., Beevers, S.D. and M.C. Bell (2007). Risks of exceeding
the hourly EU limit value for nitrogen dioxide resulting from increased
road transport emissions of primary nitrogen dioxide. Atmospheric
Environment 41 2073-2082.

Carslaw, D.C. (2005a). Evidence of an increasing NO2/NOX emissions ratio
from road traffic emissions. Atmospheric Environment, 39(26) 4793-4802.

Carslaw, D.C. and Beevers, S.D. (2005b). Development of an urban
inventory for road transport emissions of NO2 and comparison with
estimates derived from ambient measurements. Atmospheric Environment,
(39): 2049-2059.

Carslaw, D.C. and Beevers, S.D. (2005c). Estimations of road vehicle
primary NO2 exhaust emission fractions using monitoring data in
London. Atmospheric Environment, 39(1): 167-177.

Carslaw, D. C. and S. D. Beevers (2004). Investigating the Potential
Importance of Primary NO2 Emissions in a Street Canyon. Atmospheric
Environment 38(22): 3585-3594.

Carslaw, D. C. and S. D. Beevers (2004). New Directions: Should road
vehicle emissions legislation consider primary NO2? Atmospheric
Environment 38(8): 1233-1234.


}
\author{David Carslaw}


\section{Warning }{\code{calcFno2} works best when the roadside signal
  is strong i.e. when roadside concentrations are in clear excess of
  background concentrations. When there is little difference between
  roadside and background concentrations it is difficult for the method
  to distinguish between the NO2 from direct emissions and that formed
  through the reaction with NO and O3. Similarly, during summertime
  conditions when concentrations are generally lower, it can be
  difficult to estimate the level of primary NO2. A safer approach would
  be to calculate f-NO2 during "winter" condtions. See
  \code{\link{selectByDate}} for information on selecting specific
  time periods.

Sometimes it can be useful to filter by wind direction (and maybe wind
speed) to ensure that the conditions where the contribution from the
road is prominent are selected. The \code{\link{polarPlot}} function
can be useful for filtering the data in this way.}

\seealso{ \code{\link{linearRelation}} if you have roadside ozone measurements. }
\examples{
## Users should see the full openair manual for examples of how
## to use this function.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{ ~kwd2 }% ONLY ONE keyword per line
