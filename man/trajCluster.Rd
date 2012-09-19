\name{trajCluster}
\alias{trajCluster}
\title{Calculate clusters for back tracectories}
\usage{
  trajCluster(traj, method = "Euclid", n.cluster = 5,
    plot = TRUE, type = "default", cols = "Set1",
    split.after = FALSE, ...)
}
\arguments{
  \item{traj}{An openair trajectory data frame resulting
  from the use of \code{importTraj}.}

  \item{method}{Method used to calculate the distance
  matrix for the back trajectories. There are two methods
  available: \dQuote{Euclid} and \dQuote{Angle}.}

  \item{n.cluster}{Number of clusters to calculate.}

  \item{plot}{Should a plot be produced?}

  \item{type}{\code{type} determines how the data are split
  i.e. conditioned, and then plotted. The default is will
  produce a single plot using the entire data. Type can be
  one of the built-in types as detailed in \code{cutData}
  e.g. \dQuote{season}, \dQuote{year}, \dQuote{weekday} and
  so on. For example, \code{type = "season"} will produce
  four plots --- one for each season. Note that the cluster
  calculations are separately made of each level of
  "type".}

  \item{cols}{Colours to be used for plotting. Options
  include \dQuote{default}, \dQuote{increment},
  \dQuote{heat}, \dQuote{jet} and \code{RColorBrewer}
  colours --- see the \code{openair} \code{openColours}
  function for more details. For user defined the user can
  supply a list of colour names recognised by R (type
  \code{colours()} to see the full list). An example would
  be \code{cols = c("yellow", "green", "blue")}}

  \item{split.after}{For \code{type} other than
  \dQuote{default} e.g. \dQuote{season}, the trajectories
  can either be calculated for each level of \code{type}
  independently or extracted after the cluster calculations
  have been applied to the whole data set.}

  \item{...}{Other graphical parameters passed onto
  \code{lattice:levelplot} and \code{cutData}. Similarly,
  common axis and title labelling options (such as
  \code{xlab}, \code{ylab}, \code{main}) are passed to
  \code{levelplot} via \code{quickText} to handle routine
  formatting.}
}
\value{
  Returns original data frame with a new (factor) variable
  \code{cluster} giving the calculated cluster.
}
\description{
  This function carries out cluster analysis of HYSPLIT
  back trajectories. The function is specifically designed
  to work with the trajectories imported using the
  \code{openair} \code{importTraj} function, which provides
  pre-calculated back trajectories at specific receptor
  locations.
}
\details{
  Two main methods are available to cluster the back
  trajectories using two different calculations of the
  distance matrix. The default is to use the standard
  Euclidian distance between each pair of trajectories.
  Also available is an angle-based distance matrix based on
  Sirois and Bottenheim (1995). The latter method is useful
  when the interest is the direction of the trajectories in
  clustering.

  The distance matrix calculations are made in C++ for
  speed. For data sets of up to 1 year both methods should
  be relatively fast, although the \code{method = "Angle"}
  does tend to take much longer to calculate. Further
  details of these methods are given in the openair manual.
}
\examples{
\dontrun{
## import trajectories
traj <- importTraj(site = "london", year = 2009)
## calculate clusters
traj <- trajCluster(traj, n.clusters = 5)
head(traj) ## note new variable 'cluster'
## use different distance matrix calculation, and calculate by season
traj <- trajCluster(traj, method = "Angle", type = "season", n.clusters = 4)
}
}
\author{
  David Carslaw
}
\references{
  Sirois, A. and Bottenheim, J.W., 1995. Use of backward
  trajectories to interpret the 5-year record of PAN and O3
  ambient air concentrations at Kejimkujik National Park,
  Nova Scotia. Journal of Geophysical Research, 100:
  2867-2881.
}
\seealso{
  \code{\link{importTraj}}, \code{\link{trajPlot}},
  \code{\link{trajLevel}}
}
\keyword{methods}

