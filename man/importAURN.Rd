\name{importAURN}
\alias{importAURN}
\title{AURN data import for openair}
\usage{
  importAURN(site = "my1", year = 2009, pollutant = "all",
  hc = FALSE)
}
\arguments{
  \item{site}{Site code of the AURN site to import e.g.
  "my1" is Marylebone Road. Several sites can be imported
  with \code{site = c("my1", "nott")} --- to import
  Marylebone Road and Nottingham for example.}

  \item{year}{Year or years to import. To import a sequence
  of years from 1990 to 2000 use \code{year = 1990:2000}.
  To import several specfic years use \code{year = c(1990,
  1995, 2000)} for example.}

  \item{pollutant}{Pollutants to import. If omitted will
  import all pollutants ffrom a site. To import only NOx
  and NO2 for example use \code{pollutant = c("nox",
  "no2")}.}

  \item{hc}{A few sites have hydrocarbon measurements
  available and setting \code{hc = TRUE} will ensure
  hydrocarbon data are imported. The default is however not
  to as most users will not be interested in using
  hydrocarbon data and the resulting data frames are
  considerably larger.}
}
\value{
  Returns a data frame of hourly mean values with date in
  POSIXct class and time zone GMT.
}
\description{
  Function for importing hourly mean UK Automatic Urban and
  Rural Network (AURN) air quality archive data files for
  use with the \code{openair} package. Files are imported
  from a remote server operated by AEA that provides air
  quality data files as R data objects.
}
\details{
  The \code{importAURN} function has been written to make
  it easy to import data from the UK AURN. AEA have
  provided .RData files (R workspaces) of all individual
  sites and years for the AURN. These files are updated on
  a daily basis. This approach requires a link to the
  Internet to work.

  There are several advantages over the web portal approach
  where .csv files are downloaded. First, it is quick to
  select a range of sites, pollutants and periods (see
  examples below). Second, storing the data as .RData
  objects is very efficient as they are about four times
  smaller than .csv files --- which means the data
  downloads quickly and saves bandwidth. Third, the
  function completely avoids any need for data manipulation
  or setting time formats, time zones etc. Finally, it is
  easy to import many years of data beyond the current
  limit of about 64,000 lines. The final point makes it
  possible to download several long time series in one go.
  The function also has the advantage that the proper site
  name is imported and used in \code{openair} functions.

  The site codes and pollutant names can be upper or lower
  case. The function will issue a warning when data less
  than six months old is downloaded, which may not be
  ratified.

  The data are imported by stacking sites on top of one
  another and will have field names \code{site},
  \code{code} (the site code) and \code{pollutant}.
  Sometimes it is useful to have columns of site data. This
  can be done using the \code{reshape} function --- see
  examples below.

  All units are expressed in mass terms for gaseous species
  (ug/m3 for NO, NO2, NOx (as NO2), SO2 and hydrocarbons;
  and mg/m3 for CO). PM10 concentrations are provided in
  gravimetric units of ug/m3 or scaled to be comparable
  with these units. Over the years a variety of instruments
  have been used to measure particulate matter and the
  technical issues of measuring PM10 are complex. In recent
  years the measurements rely on FDMS (Filter Dynamics
  Measurement System), which is able to measure the
  volatile component of PM. In cases where the FDMS system
  is in use there will be a separate volatile component
  recorded as 'v10', which is already included in the
  absolute PM10 measurement. Prior to the use of FDMS the
  measurements used TEOM (Tapered Element Oscillating.
  Microbalance) and these concentrations have been
  multiplied by 1.3 to provide an estimate of the total
  mass including the volatile fraction.

  The few BAM (Beta-Attenuation Monitor) instruments that
  have been incorporated into the network throughout its
  history have been scaled by 1.3 if they have a heated
  inlet (to account for loss of volatile particles) and
  0.83 if they do not have a heated inlet. The few TEOM
  instruments in the network after 2008 have been scaled
  using VCM (Volatile Correction Model) values to account
  for the loss of volatile particles. The object of all
  these scaling processes is to provide a reasonable degree
  of comparison between data sets and with the reference
  method and to produce a consistent data record over the
  operational period of the network, however there may be
  some discontinuity in the time series associated with
  instrument changes.

  No corrections have been made to teh PM2.5 data. The
  volatile component of FDMS PM2.5 (where available) is
  shown in the 'v2.5' column.

  While the function is being developed, the following site
  codes should help with selection. \itemize{ \item A3
  London A3 Roadside \item ABD Aberdeen \item ABD7 Aberdeen
  Union Street Roadside \item ACTH Auchencorth Moss \item
  AH Aston Hill \item BAR2 Barnsley 12 \item BAR3 Barnsley
  Gawber \item BARN Barnsley \item BATH Bath Roadside \item
  BEL Belfast East \item BEL2 Belfast Centre \item BEL4
  Belfast Clara St \item BEX London Bexley \item BHAM
  Birmingham \item BIL Billingham \item BIR Bircotes \item
  BIR1 Birmingham Tyburn \item BIR2 Birmingham East \item
  BIRM Birmingham Centre \item BIRT Birmingham Tyburn
  Roadside \item BLAC Blackpool \item BLC2 Blackpool Marton
  \item BLCB Blackburn Darwen Roadside \item BOLT Bolton
  \item BORN Bournemouth \item BOT Bottesford \item BRAD
  Bradford Centre \item BREN London Brent \item BRI London
  Bridge Place \item BRIS Bristol Centre \item BRIT
  Brighton Roadside \item BRN Brentford Roadside \item BRS2
  Bristol Old Market \item BRS8 Bristol St Paul's \item
  BRT3 Brighton Preston Park \item BURY Bury Roadside \item
  BUSH Bush Estate \item BY1 Bromley Roadside \item BY2
  London Bromley \item CA1 Camden \item CAM Cambridge
  Roadside \item CAMB Cambridge \item CAN London Canvey
  \item CANT Canterbury \item CAR Cardiff \item CARD
  Cardiff Centre \item CARL Carlisle Roadside \item CHIL
  Chilworth \item CHP Chepstow A48 \item CHS6 Chesterfield
  \item CHS7 Chesterfield Roadside \item CLL Central London
  \item CLL2 London Bloomsbury \item COV2 Coventry Centre
  \item COV3 Coventry Memorial Park \item CRD London
  Cromwell Road \item CRD2 London Cromwell Road 2 \item
  CWMB Cwmbran \item DERY Derry \item DUMF Dumfries \item
  EAGL Stockton-on-Tees Eaglescliffe \item ECCL Salford
  Eccles \item ED Edinburgh Centre \item ED3 Edinburgh St
  Leonards \item EX Exeter Roadside \item FEA Featherstone
  \item FW Fort William \item GDF Great Dun Fell \item GLA
  Glasgow City Chambers \item GLA3 Glasgow Centre \item
  GLA4 Glasgow \item GLAS Glasgow Hope St \item GLAZ
  Glazebury \item GRA2 Grangemouth Moray \item GRAN
  Grangemouth \item HAR Harwell \item HARR London Harrow
  \item HG1 Haringey Roadside \item HG2 London Haringey
  \item HIL London Hillingdon \item HM High Muffles \item
  HOPE Stanford-le-Hope Roadside \item HORE Horley \item
  HORS London Westminster \item HOVE Hove Roadside \item
  HR3 London Harrow Stanmore \item HRL London Harlington
  \item HS1 Hounslow Roadside \item HUL2 Hull Freetown
  \item HULL Hull Centre \item INV2 Inverness \item ISL
  London Islington \item LB Ladybower \item LEAM Leamington
  Spa \item LED6 Leeds Headingley \item LEED Leeds Centre
  \item LEIC Leicester Centre \item LEOM Leominster \item
  LERW Lerwick \item LH Lullington Heath \item LINC Lincoln
  Roadside \item LIVR Liverpool Centre \item LN Lough Navar
  \item LON6 London Eltham \item LV6 Liverpool Queen's
  Drive Roadside \item LVP Liverpool Speke \item LW1 London
  Lewisham \item MAN Manchester Town Hall \item MAN3
  Manchester Piccadilly \item MAN4 Manchester South \item
  MH Mace Head \item MID Middlesbroug \item MY1 London
  Marylebone Road \item NCA3 Newcastle Cradlewell Roadside
  \item NEWC Newcastle Centre \item NO10 Norwich Forum
  Roadside \item NOR1 Norwich Roadside \item NOR2 Norwich
  Centre \item NOTT Nottingham Centre \item NPT3 Newport
  \item NTON Northampton \item OLDB Sandwell Oldbury \item
  OSY St Osyth \item OX Oxford Centre Roadside \item OX3
  Oxford St Ebbes \item PEMB Narberth \item PLYM Plymouth
  Centre \item PMTH Portsmouth \item PRES Preston \item PT
  Port Talbot \item PT4 Port Talbot Margam \item REA1
  Reading New Town \item READ Reading \item REDC Redcar
  \item ROCH Rochester Stoke \item ROTH Rotherham Centre
  \item RUGE Rugeley \item SALT Saltash Roadside \item SCN2
  Scunthorpe Town \item SCUN Scunthorpe \item SDY Sandy
  Roadside \item SEND Southend-on-Sea \item SHE Sheffield
  Tinsley \item SHE2 Sheffield Centre \item SIB Sibton
  \item SOM Somerton \item SOUT Southampton Centre \item
  STE Stevenage \item STEW Stewartby \item STOC Stockport
  \item SUN2 Sunderland Silksworth \item SUND Sunderland
  \item SUT1 Sutton Roadside \item SUT3 London Sutton \item
  SV Strath Vaich \item SWA1 Swansea Roadside \item SWAN
  Swansea \item TED London Teddington \item TH2 Tower
  Hamlets Roadside \item THUR Thurrock \item TRAN Wirral
  Tranmere \item WA2 London Wandsworth \item WAL Walsall
  Alumwell \item WAL2 Walsall Willenhall \item WAR
  Warrington \item WBRO Sandwell West Bromwich \item WC
  Wharleycroft \item WEYB Weybourne \item WFEN Wicken Fen
  \item WIG3 Wigan Leigh \item WIG5 Wigan Centre \item WL
  West London \item WOLV Wolverhampton Centre \item WRAY
  Wray \item WREX Wrexham \item YARM Stockton-on-Tees Yarm
  \item YW Yarner Wood }
}
\examples{
## import all pollutants from Marylebone Rd from 1990:2009
\dontrun{mary <- importAURN(site = "my1", year = 2000:2009)}

## import nox, no2, o3 from Marylebone Road and Nottingham Centre for 2000
\dontrun{thedata <- importAURN(site = c("my1", "nott"), year = 2000,
pollutant = c("nox", "no2", "o3"))}

## import over 20 years of Mace Head O3 data!
\dontrun{o3 <- importAURN(site = "mh", year = 1987:2009)}

## import hydrocarbon (and other) data from Marylebone Road
\dontrun{mary <- importAURN(site = "my1", year =1998, hc = TRUE)}

## reshape the data so that each column represents a pollutant/site
\dontrun{thedata <- importAURN(site = c("nott", "kc1"), year = 2008,
pollutant = "o3")
thedata <- melt(thedata, measure.vars = "o3")
thedata <- dcast(thedata, ... ~ variable + site + code)
## thedata now has columns  o3_Nottingham Centre_NOTT o3_London N. Kensington_KC1
## now can export as a csv file:
write.csv(thedata, file = "~/temp/thedata.csv")

}
}
\author{
  David Carslaw
}
\seealso{
  \code{\link{importKCL}}, \code{\link{importADMS}},
  \code{\link{importSAQN}}
}
\keyword{methods}

