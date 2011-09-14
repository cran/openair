\name{importTraj}
\alias{importTraj}
\title{Import pre-calculated HYSPLIT 96-hour back trajectories}
\usage{
  importTraj(site = "london", year = 2009, local = NA)
}
\arguments{
  \item{site}{Site code of the network site to import e.g.
  "london". Only one site can be imported at a time. The
  following sites are available from 1988-2010:
  \tabular{llrr}{ SITE CODE \tab SITE NAME \tab LATITUDE
  \tab LONGITUDE\cr birm \tab Birmigham Centre \tab
  52.47972 \tab -1.908078\cr ed \tab Edinburgh \tab
  55.95197 \tab -3.195775\cr london \tab Central London
  \tab 51.5 \tab -0.1 \cr lh \tab Lullington Heath \tab
  50.7937 \tab 0.18125 \cr mh \tab Mace Head \tab 53.33
  \tab -9.9 \cr sv \tab Strath Vaich \tab 57.73446 \tab
  -4.776583\cr yw \tab Yarner Wood \tab 50.5976 \tab
  -3.71651 }}

  \item{year}{Year or years to import. To import a sequence
  of years from 1990 to 2000 use \code{year = 1990:2000}.
  To import several specfic years use \code{year = c(1990,
  1995, 2000)} for example.}

  \item{local}{Used for testing purposes on a local file
  system.}
}
\value{
  Returns a data frame with pre-calculated back
  trajectories.
}
\description{
  Function to import pre-calculated back trajectories using
  the NOAA HYSPLIT model. The trajectories have been
  calculated for a select range of locations which will
  expand in time. They cover the last 20 years or so and
  can be used together with other \code{openair} functions.
}
\details{
  This function imports pre-calculated back trajectories
  using the HYSPLIT trajectory model (Hybrid Single
  Particle Lagrangian Integrated Trajectory Model
  \url{http://ready.arl.noaa.gov/HYSPLIT.php}). Back
  trajectories provide some very useful information for air
  quality data analysis. However, while they are commonly
  calculated by researchers it is generally difficult for
  them to be calculated on a routine basis and used easily.
  In addition, the availability of back trajectories over
  several years can be very useful, but again difficult to
  calculate.

  Trajectories are run at 3-hour intervals and stored in
  yearly files (see below). The trajectories are started at
  ground-level (10m) and propagated backwards in time.

  These trajectories have been calculated using the Global
  NOAA-NCEP/NCAR reanalysis data archives. The global data
  are on a latitude-longitude grid (2.5 degree). Note that
  there are many different meteorological data sets that
  can be used to run HYSPLIT e.g. including ECMWF data.
  However, in order to make it practicable to run and store
  trajectories for many years and sites, the NOAA-NCEP/NCAR
  reanalysis data is most useful. In addition, these
  archives are available for use widely, which is not the
  case for many other data sets e.g. ECMWF. HYSPLIT
  calculated trajectories based on archive data may be
  distributed without permission (see
  \url{http://ready.arl.noaa.gov/HYSPLIT_agreement.php}).
  For those wanting, for example, to consider higher
  resolution meteorological data sets it may be better to
  run the trajectories separately.

  We are extremely grateful to NOAA for making HYSPLIT
  available to produce back trajectories in an open way. We
  ask that you cite HYSPLIT if used in published work.

  The files consist of the following information:

  \describe{ \item{date}{This is the arrival point time and
  is repeated the number of times equal to the length of
  the back trajectory --- typically 96 hours (except early
  on in the file). It is this field that should be used to
  link with air quality data. See example below.}
  \item{receptor}{Receptor number, currently only 1.}
  \item{year}{The year} \item{month}{Month 1-12}
  \item{day}{Day of the month 1-31} \item{hour}{Hour of the
  day 0-23 GMT} \item{hour.inc}{Number of hours back in
  time 0 to -96.} \item{lat}{Latitude in decimal format.}
  \item{lon}{Longitude in decimal format.}
  \item{height}{Height of trajectory (m).}
  \item{pressure}{Pressure of trajectory (kPa).} }
}
\note{
  The trajectories were run using the February 2011 HYSPLIT
  model. The function is primarily written to investigate a
  single site at a time for a single year. The trajectory
  files are quite large and care should be exercised when
  importing several years and/or sites.
}
\examples{
## import trajectory data for London in 2009
\dontrun{mytraj <- importTraj(site = "london", year = 2009)}

## combine with measurements
\dontrun{theData <- importAURN(site = "kc1", year = 2009)
mytraj <- merge(mytraj, theData, by = "date")}
}
\author{
  David Carslaw
}
\seealso{
  \code{\link{trajPlot}}, \code{\link{importAURN}},
  \code{\link{importKCL}},\code{\link{importADMS}},
  \code{\link{importSAQN}}
}
\keyword{methods}

