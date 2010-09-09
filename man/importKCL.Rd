\name{importKCL}
\alias{importKCL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Import data from King's College London networks}

\description{Function for importing hourly mean data from King's College
  London networks. Files are imported from a remote server operated by
  King's College London that provides air quality data files as R data
  objects. }

\usage{thedata <- importKCL(site = "my1", year = 2009, pollutant =
"all", met = FALSE, units = "mass")}

%- maybe also 'usages' for other objects documented here.
\arguments{
\item{site}{Site code of the network site to import e.g. "my1" is
Marylebone Road. Several sites can be imported with \code{site =
c("my1", "kc1")} --- to import Marylebone Road and North Kensignton for
example.}

  \item{year}{Year or years to import. To import a sequence of years
  from 1990 to 2000 use \code{year = 1990:2000}. To import several
  specfic years use \code{year = c(1990, 1995, 2000)} for example.}

  \item{pollutant}{Pollutants to import. If omitted will import all
  pollutants from a site. To import only NOx and NO2 for example use
  \code{pollutant = c("nox", "no2")}.}

\item{met}{Should meteorological data be added to the import data? The
  default is \code{FALSE}. If \code{TRUE} wind speed (m/s), wind
  direction (degrees) and
  temperature (deg. C) are added from the Heathrow site. This is
  currently for testing and we intend to use representative London data
  from sites where there are no commercial issues related to data
  usage.}
\item{units}{By default the returned data frame expresses the units in
  mass terms (ug/m3 for NOx, NO2, O3, SO2; mg/m3 for CO). Use
  \code{units = "volume"} to use ppb etc. PM10_raw TEOM data are multiplied
  by 1.3 and PM2.5 have no correction applied. See details below
  concerning PM10 concentrations.}

}

\details{The \code{importKCL} function has been written to make it easy
to import data from the King's College London air pollution
networks. KCL have provided .RData files (R workspaces) of all
individual sites and years for the KCL networks. These files are updated on a
weekly basis. This approach requires a link to the Internet to work.

There are several advantages over the web portal approach where .csv
files are downloaded. First, it is quick to select a range of sites,
pollutants and periods (see examples below). Second, storing the data as
.RData objects is very efficient as they are about four times smaller
than .csv files --- which means the data downloads quickly and saves
bandwidth. Third, the function completely avoids any need for data
manipulation or setting time formats, time zones etc. Finally, it is
easy to import many years of data beyond the current limit of about
64,000 lines. The final point makes it possible to download several long
time series in one go. The function also has the advantage that the
proper site name is imported and used in \code{openair} functions.

The site codes and pollutant names can be upper or lower case. The
function will issue a warning when data less than six months old is
downloaded, which may not be ratified.

The data are imported by stacking sites on top of one another and will
have field names \code{date}, \code{site}, \code{code} (the site code) and
pollutant(s). Sometimes it is useful to have columns of site
data. This can be done using the \code{reshape} function --- see
examples below.

The function imports two measures of PM10 where
available. \code{PM10_raw} are TEOM measurements with a 1.3 factor
applied to take account of volatile losses. The \code{PM10} data is a
current best estimate of a gravimetric equivalent measure as described
below.

For the assessment of the EU Limit Values, PM10 needs to be measured
using the reference method or one shown to be equivalent to the
reference method. Defra carried out extensive trials between 2004 and
2006 to establish which types of particulate analysers in use in the UK
were equivalent. These trials found that measurements made using
Partisol, FDMS, BAM and SM200 instruments were shown to be equivalent to
the PM10 reference method. However, correction factors need to be
applied to measurements from the SM200 and BAM instruments. Importantly,
the TEOM was demonstrated as not being equivalent to the reference
method due to the loss of volatile PM, even when the 1.3 correction
factor was applied.  The Volatile Correction Model (VCM) was developed
for Defra at King's to allow measurements of PM10 from TEOM instruments
to be converted to reference equivalent; it uses the measurements of
volatile PM made using nearby FDMS instruments to correct the
measurements made by the TEOM. It passed the equivalence testing using
the same methodology used in the Defra trials and is now the recommended
method for correcting TEOM measurements (Defra, 2009). VCM correction of
TEOM measurements can only be applied after 1st January 2004, when
sufficiently widespread measurements of volatile PM became
available. The 1.3 correction factor is now considered redundant for
measurements of PM10 made after 1st January 2004.  Further information
on the VCM can be found at
\url{http://www.volatile-correction-model.info/}.

All PM10 statistics on the LondonAir web site, including the bulletins
and statistical tools (and in the RData objects downloaded using
\code{importKCL}), now report PM10 results as reference equivalent. For
PM10 measurements made by BAM and SM200 analysers the applicable
correction factors have been applied. For measurements from TEOM
analysers the 1.3 factor has been applied up to 1st January 2004, then
the VCM method has been used to convert to reference equivalent.

While the function is being developed, the following site codes should
help with selection. We will also make available other meta data such as
site type and location to make it easier to select sites based on other
information.

\itemize{
  \item A30 A3 - AURN
\item AR1 Chichester - Lodsworth
\item AR2 Wealden - Isfield
\item AS1 Bath
\item BA1 Basildon
\item BB1 Broxbourne Roadside
\item BE0 Belfast - Carbon
\item BE1 Belfast - CPC
\item BE8 Belfast - Nitrate
\item BE9 Belfast - Partisol SO4
\item BF1 Bedford Rural
\item BF3 Bedford - Kempston
\item BF4 Bedford  - Prebend Street
\item BG1 Barking and Dagenham - Rush Green
\item BG2 Barking and Dagenham - Scrattons Farm
\item BG3 Barking and Dagenham - North Street
\item BG5 Waterside Close
\item BG6 Becontree Avenue
\item BG7 Lindsey Road
\item BH1 Brighton Roadside
\item BH2 Brighton and Hove - Hove Town Hall
\item BH3 Brighton and Hove - Foredown Tower
\item BH5 Brighton Mobile (Preston Fire Station)
\item BH6 Brighton Mobile (Lewes Road)
\item BH7 Brighton Mobile (Gloucester Road)
\item BI1 Birmingham - CPC
\item BL0 Camden - Bloomsbury
\item BL1 Bloomsbury - SMPS
\item BM1 Ballymena - Ballykeel
\item BM2 Ballymena - North Road
\item BN1 Barnet - Tally Ho Corner
\item BN2 Barnet - Finchley
\item BN3 Barnet 3 - Strawberry Vale
\item BO1 Ballymoney 1
\item BO2 Bexley - Bexley High Street
\item BO3 Bexley - Bourne Road
\item BP0 Bridge Place - AURN
\item BT1 Brent - Kingsbury
\item BT2 Brent 2 -  Ikea
\item BT3 Brent 3 -  Harlesden
\item BT4 Brent - Ikea
\item BT5 Brent - Neasden Lane
\item BT6 Brent - John Keble Primary School
\item BT7 Brent - St Marys Primary School
\item BTE Environment Agency Neasden Lane
\item BW1 Brentwood - Brentwood Town Hall
\item BX0 Bexley - Belvedere FDMS
\item BX1 Bexley - Slade Green
\item BX2 Bexley - Belvedere
\item BX3 Bexley - Thamesmead
\item BX4 Bexley - Erith
\item BX5 Bexley 5 - Bedonwell
\item BX6 Bexley - Thames Road North FDMS
\item BX7 Bexley - Thames Road North
\item BX8 Bexley - Thames Road South
\item BX9 Bexley - Slade Green FDMS
\item BY1 Bromley Rent office
\item BY4 Bromley 4 - Town Centre Tweedy Rd
\item BY5 Bromley - Biggin Hill
\item BY7 Bromley - Harwood Avenue
\item BY8 Bromley 8 - TECO NOX
\item CA1 Crawley Background
\item CA2 Crawley - Gatwick Airport
\item CD1 Camden - Swiss Cottage
\item CD3 Camden - Shaftesbury Avenue
\item CD4 Camden - St Martins College (NOX 1)
\item CD5 Camden - St Martins College (NOX 2)
\item CD6 Camley Street
\item CD7 Camden 1 Partisol
\item CF1 Cardiff
\item CI1 Chichester - A27 Chichester Bypass
\item CI4 Chichester - Orchard Street
\item CK1 Cookstown
\item CO1 CONCAWE - Hyde Park
\item CP1 Castle Point - Canvey Island
\item CR2 Croydon - Purley Way
\item CR3 Croydon - Thornton Heath
\item CR4 Croydon - George Street
\item CR5 Croydon - Norbury
\item CR6 Croydon 6 - Euston Road
\item CT1 City of London - Senator House
\item CT2 City of London 2 - Farringdon Street
\item CT3 City of London - Sir John Cass School
\item CT4 City of London 4 - Beech Street
\item CT5 City of London Black Smoke (LONDON CITY 16)
\item CT6 City of London - Walbrook Wharf
\item CT7 City of London 7 - NOx paint study site
\item CT8 City of London - Upper Thames Street
\item CY1 Crystal Palace - Crystal Palace Parade
\item DC1 Dacorum Background (Hemel H)
\item DC2 Dacorum Background (re-opened)
\item DC3 High Street Northchurch
\item DE1 Derry City - Brandywell
\item DE2 Derry City - Dales Corner
\item DM1 Dunmurry
\item EA0 Ealing - Acton Town Hall FDMS
\item EA1 Ealing - Ealing Town Hall
\item EA2 Ealing - Acton Town Hall
\item EA4 Ealing Mobile - Hamilton Road
\item EA5 Ealing Mobile - Southall
\item EA6 Ealing - Hanger Lane Gyratory
\item EA7 Ealing - Southall
\item EA8 Ealing - Horn Lane
\item EA9 Court Way, Acton
\item EB1 Eastbourne - Devonshire Park
\item EB3 Eastbourne - Holly Place
\item EC1 Entec - Blackwall Lane
\item ED0 Edinburgh St Leonard's
\item EG2 Ealing Black Smoke (EALING 7)
\item EH1 E. Herts Rural (Throcking)
\item EH2 East Herts - Throcking
\item EH3 East Herts - Sawbridgeworth
\item EH4 East Herts - Ware
\item EH5 East Herts - Bishops Stortford
\item EI0 Ealing 10
\item EL1 Elmbridge - Bell Farm Hersham
\item EL2 Elmbridge - Esher High Street
\item EN1 Enfield 1 - Bushhill Park
\item EN2 Enfield 2 - Church Street
\item EN3 Enfield 3 - Salisbury School Ponders End
\item EN4 Enfield - Derby Road
\item EN5 Enfield - Bowes Primary School
\item EN6 Enfield Black Smoke (ENFIELD 14)
\item FB1 Rushmoor - Medway Drive
\item GB0 Greenwich and Bexley - Falconwood FDMS
\item GB6 Greenwich and Bexley - Falconwood
\item GL1 Glasgow Centre
\item GL4 Glasgow Centre
\item GN0 Greenwich - A206 Burrage Grove
\item GN2 Greenwich - Millennium Village
\item GN3 Greenwich - Plumstead High Street
\item GN4 Greenwich 9
\item GR2 Greenwich Black Smoke (GREENWICH 9)
\item GR3 Woolwich
\item GR4 Greenwich - Eltham
\item GR5 Greenwich - Trafalgar Road
\item GR7 Greenwich - Blackheath
\item GR8 Greenwich - Woolwich Flyover
\item GR9 Greenwich - Westhorne Avenue
\item GW1 Gatwick 1
\item HA0 Harwell - Carbon
\item HA1 Harwell Rural AURN
\item HA4 Harwell - SMPS
\item HA7 Harwell Rural AURN
\item HA8 Harwell - Nitrate
\item HA9 Harwell - Partisol SO4
\item HF1 A3 - AURN
\item HF2 Hammersmith & Fulham 2 - Brook Green
\item HF3 Hammersmith & Fulham 3 - Scrubs Lane
\item HG1 Haringey - Haringey Town Hall
\item HG2 Haringey - Priory Park
\item HG3 Haringey 3 - Bounds Green
\item HI0 Hillingdon - Sipson Road
\item HI1 Hillingdon - South Ruislip
\item HI2 Hillingdon - Hillingdon Hospital
\item HI3 Hillingdon - Oxford Avenue
\item HK4 Hackney - Clapton
\item HK6 Hackney - Old Street
\item HL1 Halifax
\item HM1 A3 - AURN
\item HM4 Hertsmere - Borehamwood
\item HO1 Horsham Background
\item HO2 Horsham - Park Way
\item HO4 Horsham - Storrington
\item HR1 Harrow - Stanmore
\item HR2 Harrow - Pinner Road
\item HS1 Hounslow 1 - Brentford
\item HS2 Hounslow - Cranford
\item HS3 Hounslow 3 - Brentford
\item HS4 Hounslow - Chiswick High Road
\item HS5 Hounslow - Brentford
\item HS6 Hounslow - Heston Road
\item HS7 Hounslow - Hatton Cross
\item HS9 Hounslow - Feltham
\item HT1 Hastings - Bulverhythe
\item HT2 Hastings - Fresh Fields
\item HV1 Havering - Rainham
\item HV2 Havering 2 - Harold Hill
\item HV3 Havering - Romford
\item HX0 Birmingham Tyburn
\item HX9 Birmingham Tyburn - CPC
\item IC6 City of London - Walbrook Wharf Indoor
\item IG4 Greenwich - Eltham Ecology Centre Indoor
\item IS1 Islington 1 - Upper Street
\item IS2 Islington - Holloway Road
\item IS3 Islington Black Smoke (ISLINGTON 9)
\item IS4 Islington 4 - Foxham Gardens
\item IS5 Islington 5 - Duncan Terrace
\item IS6 Islington - Arsenal
\item IT2 Tower Hamlets 2 indoor CO
\item KB1 South Kirkby
\item KC0 North Kensington - Carbon
\item KC1 Kensington and Chelsea - North Ken
\item KC2 Kensington and Chelsea - Cromwell Road
\item KC3 Kensington and Chelsea - Knightsbridge
\item KC4 Kensington and Chelsea - Kings Road
\item KC5 Kensington and Chelsea - Earls Court Rd
\item KC7 Kensington and Chelsea - North Ken FDMS
\item KC8 North Kensington - Nitrate
\item KC9 North Kensington - Partisol SO4
\item KT1 Kingston 1 - Chessington
\item KT2 Kingston 2 - Town Centre
\item KX1 Kings Cross 1 - Bingfield Street
\item KX2 Kings Cross 2 - Copenhagen Street
\item KX3 Kings Cross 3 - Bingfield Park
\item KX4 Kings Cross 4 - Coopers Lane
\item LA1 Luton Airport
\item LB0 Lambeth 5 spare
\item LB1 Lambeth - Christchurch Road
\item LB2 Lambeth 2 - Vauxhall Cross
\item LB3 Lambeth - Loughborough Junct
\item LB4 Lambeth - Brixton Road
\item LB5 Lambeth - Bondway Interchange
\item LB6 Lambeth - Streatham Green
\item LBG London Mean Background
\item LH0 Hillingdon - Harlington
\item LH1 Heathrow Airport - UKMO
\item LH2 Heathrow Airport
\item LL1 Lullington Heath Rural AURN
\item LN1 Luton - Challney Community College
\item LRS London Mean Road Side
\item LS1 Lewes - Telscombe Cliffs
\item LS2 Lewes - Commercial Square
\item LW1 Lewisham - Catford
\item LW2 Lewisham - New Cross
\item LW3 Lewisham - Mercury Way
\item MA1 Manchester - CPC
\item MA2 Manchester Piccadilly
\item MD1 Biggleswade (Roadside)
\item MD2 Mid Beds Rural (Silsoe)
\item MD3 Central Beds - Sandy
\item MD4 Central Beds - Marston Vale
\item ME1 Merton - Morden Civic Centre
\item MO2 Met Office - Mumbles
\item MP1 Marchwood Power - Marchwood
\item MP2 Marchwood Power - Millbrook Rd, Soton
\item MP3 Marchwood Power - Marchwood (SM200)
\item MR1 Marylebone Road - CPC
\item MR2 Marylebone Road - SMPS
\item MR3 Marylebone Rd - Baker St
\item MV1 Mole Valley 1 - Leatherhead
\item MV2 Mole Valley 2 - Lower Ashtead
\item MV3 Mole Valley - Dorking
\item MW1 Windsor and Maidenhead - Frascati Way
\item MW2 Windsor and Maidenhead - Clarence Road
\item MW3 Windsor and Maidenhead - Ascot
\item MY0 Marylebone Road - Carbon
\item MY1 Westminster - Marylebone Road
\item MY2 Marylebone Rd - Baker St
\item MY3 Marylebone Rd - Baker St
\item MY7 Westminster - Marylebone Road FDMS
\item MY8 Marylebone Rd - Nitrate
\item NA5 Newtownabbey- Mallusk
\item NA6 Newtownabbey- Shore Road
\item NB0 Narbeth AURN
\item NE2 Port Talbot - CPC
\item NF1 New Forest - Holbury
\item NF2 New Forest - Fawley
\item NF3 New Forest - Ringwood
\item NF4 New Forest - Totton
\item NF5 New Forest - Lyndhurst
\item NH1 N. Herts Roadside 1  (Baldock)
\item NH2 N. Herts Roadside 2  (Baldock)
\item NH3 N. Herts Background (Baldock)
\item NH4 North Herts - Breechwood Green
\item NH5 North Herts - Baldock Roadside
\item NH6 North Herts - Hitchin Library
\item NK1 North Kensington - CPC
\item NK3 North Kensington
\item NK4 North Kensington - Defra OP
\item NM1 Newham 1 - Tant Avenue
\item NM2 Newham - Cam Road
\item NM3 Newham - Wren Close
\item NT0 Nottingham Centre
\item NW1 Norwich
\item OX0 Oxford Centre Roadside AURN
\item OX1 South Oxfordshire - Henley
\item OX2 South Oxfordshire - Wallingford
\item OX3 South Oxfordshire - Watlington
\item OX4 Oxford St Ebbes AURN
\item PL0 Plymouth Centre
\item PL1 Plymouth Saltash Kerbside
\item PO1 Portsmouth Background AURN
\item PT1 Port Talbot
\item PT4 Port Talbot Margam
\item PT6 Port Talbot Dyffryn School
\item PT8 Port Talbot - Env Ag Taibach
\item PT9 Port Talbot - Env Ag Corus
\item RB1 Redbridge - Perth Terrace
\item RB2 Redbridge 2 - Ilford Broadway
\item RB3 Redbridge - Fullwell Cross
\item RB4 Redbridge - Gardner Close
\item RB5 Redbridge - South Woodford
\item RB6 Ilford Black Smoke (ILFORD 6)
\item RD0 Reading AURN - New Town
\item RD1 Reading - Caversham Road
\item RD2 Reading - Kings Road
\item RD3 Reading - Oxford Road
\item RG1 Reigate and Banstead - Horley
\item RG2 Reigate and Banstead - Horley South
\item RG3 Reigate and Banstead - Poles Lane
\item RG4 Reigate and Banstead - Reigate High St
\item RGY Additional O3 at RG3
\item RI1 Richmond - Castelnau
\item RI2 Richmond - Barnes Wetlands
\item RI5 Richmond Mobile
\item RI6 Richmond Mobile
\item RI7 Richmond Mobile - Richmond Park
\item RI8 Richmond Mobile - Richmond Park
\item RIA Richmond Mobile - George Street
\item RIB Richmond Mobile - George Street
\item RIC Richmond Mobile - Kew Road
\item RID Richmond Mobile - Kew Road
\item RIE Richmond Mobile - Richmond Road, Twickenham
\item RIF Richmond Mobile - Richmond Road, Twickenham
\item RIG Richmond Mobile - Upper Teddington Road
\item RIH Richmond Mobile - Upper Teddington Road
\item RII Richmond Mobile - Somerset Road, Teddington
\item RIJ Richmond Mobile - Somerset Road, Teddington
\item RIK Richmond Mobile - St. Margarets Grove
\item RIL Richmond Mobile - St. Margarets Grove
\item RIM Richmond Mobile - Petersham Road, Ham
\item RIN Richmond Mobile - Petersham Road, Ham
\item RIO Richmond Mobile - Stanley Road, Twickenham
\item RIP Richmond Mobile - Stanley Road, Twickenham
\item RIQ Richmond Mobile - Richmond Road, Twickenham
\item RIR Richmond Mobile - Richmond Road, Twickenham
\item RIS Richmond Mobile - Lincoln Avenue, Twickenham
\item RIU Richmond Mobile - Mortlake Road, Kew
\item RIW Richmond - Upper Teddington Road
\item RIY Richmond - Hampton Court Road
\item RM1 Runnymede - RHUL
\item RY1 Rother - Rye Harbour
\item RY2 Rother - De La Warr Road
\item SA1 St Albans - Fleetville
\item SB1 South Beds - Dunstable
\item SC1 Sevenoaks 1
\item SD1 Southend Background AURN
\item SE1 Stevenage - Lytton Way
\item SH1 Shukri research
\item SH2 Shukri research
\item SK1 Southwark 1 - Elephant & Castle
\item SK2 Southwark 2 - Old Kent Road
\item SL1 Sunderland
\item SO0 Southampton Background AURN FDMS
\item SO1 Southampton Background AURN
\item SR0 Stoke on Trent
\item SS1 Slough Black Smoke
\item ST1 Sutton 1
\item ST2 Sutton 2
\item ST3 Sutton - Carshalton
\item ST4 Sutton - Wallington
\item ST5 Sutton - Beddington Lane
\item ST6 Sutton - Worcester Park
\item ST7 Sutton - Therapia Lane
\item SU1 Sussex Mobile
\item SU2 Sussex Mobile
\item SU3 Sussex Mobile
\item SU4 Sussex Mobile
\item SU5 Tinsley Close, Crawley
\item SU6 Sussex mobile, Chichester County Hall
\item SU7 East Sussex - Warnham
\item SX1 Sussex Mobile (location 1)
\item SX2 Sussex Mobile (location 2)
\item SX3 Sussex Mobile
\item SX4 Sussex Mobile A259 (location 4)
\item SX5 Sussex Mobile Fresh Fields Road (location 5)
\item SX6 Sussex mobile site
\item SX7 Sussex mobile site
\item SX8 Sussex mobile site Arundel
\item SX9 Sussex mobile site Newhaven
\item TB1 Thurrock Black Smoke
\item TD0 Richmond - National Physical Laboratory
\item TE0 Tendring St Osyth - AURN
\item TH1 Tower Hamlets - Poplar
\item TH2 Tower Hamlets - Mile End Road
\item TH3 Tower Hamlets - Bethnal Green
\item TH4 Tower Hamlets - Blackwall
\item TK1 Thurrock - London Road (Grays)
\item TK2 Thurrock - Purfleet
\item TK3 Thurrock - Stanford-le-Hope
\item TK6 Thurrock - roadside OSIRIS
\item TK8 Thurrock - London Road (Purfleet)
\item TL1 GR4 Biomass
\item TL2 CD1 chem speciation PM2.5
\item TL3 BX2 Biomass
\item TR1 Three Rivers - Rickmansworth
\item UN1 Unison 1 - East
\item UN2 Unison 2 - West
\item VS1 Victoria Street - TRL
\item WA2 Wandsworth - Town Hall
\item WA3 Wandsworth 3 - Roehampton
\item WA4 Wandsworth 4 - High Street
\item WA6 Wandsworth 6 - Tooting
\item WA7 Wandsworth - Putney High Street
\item WE0 West London - AURN
\item WF1 Watford Roadside
\item WF2 Watford - Watford Town Hall
\item WH1 Welwyn Hatfield  - Council Offices
\item WL1 Waltham Forest - Dawlish Road
\item WL2 Waltham Forest 2 -  Mobile
\item WL3 Waltham Forest 3 - Chingford
\item WL4 Waltham Forest - Crooked Billet
\item WL5 Waltham Forest - Leyton
\item WM0 Westminster - Horseferry Road
\item WM2 Westminster Black Smoke (WESTMINSTER 17)
\item WM3 Westminster - Hyde Park Partisol
\item WM4 Westminster - Charing Cross Library
\item WM5 Westminster - Covent Garden
\item WR1 Bradford
\item XB1 Bletchley
\item XP1 Biomass Partisol - Greenwich(Eltham) - was TL1
\item XP2 Biomass Partisol -Islington (Arsenal) - was TL2
\item XP3 Biomass Partisol - Bexley(Belvedere)
\item XP4 Biomass Partisol - Horseferry Road
\item XP5 Biomass Partisol - North Ken
\item XP6 Biomass Partisol - Ealing (Southall)
\item YB1 Bentley Hall Farm
\item YC1 Carr Lane
\item YD1 Downes Ground
\item YG1 Gainsborough Cemetery
\item YH1 Harrogate Roadside
\item YH2 Hemingbrough Landing
\item YL1 Lowlands Farm
\item YN1 New House Farm
\item YP1 Park Farm
\item YS1 Stile Cop Cemetery
\item YS2 Smeathalls Farm
\item YSB A3 - AURN
\item YW1 West Bank
\item ZA1 Ashford Rural - Pluckley
\item ZA2 Ashford Roadside
\item ZC1 Chatham Roadside - A2
\item ZD1 Dover Roadside - Town Hall
\item ZD2 Dover Roadside - Townwall Street
\item ZD3 Dover Background - Langdon Cliff
\item ZD4 Dover Background - East Cliff
\item ZD5 Dover Coast Guard Met
\item ZD6 Dover Docks
\item ZF1 Folkestone Suburban - Cheriton
\item ZG1 Gravesham Backgrnd - Northfleet
\item ZG2 Gravesham Roadside - A2
\item ZG3 Gravesham Ind Bgd - Northfleet
\item ZH1 Thanet Rural - Minster
\item ZH2 Thanet Background
\item ZH3 Thanet Airport - Manston
\item ZH4 Thanet Roadside - Ramsgate
\item ZL1 Luton Background
\item ZM1 Maidstone Meteorological
\item ZM2 Maidstone Roadside - Fairmeadow
\item ZM3 Maidstone Rural - Detling
\item ZR1 Dartford Roadside - St Clements
\item ZR2 Dartford Roadside 2 - Town Centre
\item ZR3 Dartford Roadside 3 - Bean Interchange
\item ZS1 Stoke Rural
\item ZT1 Tonbridge Roadside - Town Centre
\item ZT2 T. Wells Background - Town Hall
\item ZT3 T. Wells Rural - Southborough
\item ZT4 T. Wells - Roadside
\item ZV1 Sevenoaks - Greatness Park
\item ZV2 Sevenoaks - Bat and Ball
\item ZW1 Swale Roadside
\item ZW2 Swale Background
\item ZW3 Swale Roadside 2
\item ZY1 Canterbury Backgrnd - Chaucer TS
\item ZY2 Canterbury Roadside - St. Dunstans
}
}

\value{Returns a data frame of hourly mean values with date in POSIXct
  class and time zone GMT. }

\author{David Carslaw}

\section{Warning }{The \code{importKCL} function is currently released as a
  test version. NOTE - Information is required on the site codes for
  easy use.}

\seealso{\code{\link{importAURN}}, \code{\link{importADMS}}}

\examples{

## import all pollutants from Marylebone Rd from 1990:2009
\dontrun{mary <- importKCL(site = "my1", year = 2000:2009)}

## import nox, no2, o3 from Marylebone Road and North Kensignton for 2000
\dontrun{thedata <- importKCL(site = c("my1", "kc1"), year = 2000,
pollutant = c("nox", "no2", "o3"))}

## import met data too...
\dontrun{my1 <- importKCL(site = "my1", year = 2008, met = TRUE)}

## reshape the data so that each column represents a pollutant/site
\dontrun{thedata <- importKCL(site = c("my1", "kc1"), year = 2008,
pollutant = "o3")
thedata <- reshape(thedata, idvar = "date", timevar = "code", direction
= "wide")
## thedata now has columns o3.MY1 and o3.KC1
## can also get rid of columns, in this case site:
thedata <- reshape(thedata, idvar = "date", timevar = "code", direction
= "wide", drop = "site")
## now can export as a csv file:
write.csv(thedata, file = "~/temp/thedata.csv")

}


}
\keyword{methods}


