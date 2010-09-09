\name{import}
\alias{import}

\title{ Generic data import for openair }
\description{Generic (workhorse) function for importing and formatting data
for use with the openair package. The function uses \code{read.table} (in 
\code{utils}).  }

\usage{ 
mydata <- import(file = file.choose(), file.type="csv", 
		header.at = 1, data.at = 2, 
		eof.report = NULL, na.strings = c("", "NA"), quote="\"", 
		date.name = "date", date.break = "/", date.order = "dmy", 
		time.name = "date", time.break = ":", time.order = "hm", 
		time.format ="GMT", 
		is.ws = NULL, is.wd = NULL, is.site = NULL,
		misc.info = NULL, 
		bad.24 = FALSE, 
		correct.time = NULL, 
		output = "final")
}

\arguments{
\item{file}{ The name of the file to be imported. Default, \code{file =
file.choose()}, opens browser. Alternatively, the use of \code{read.table} (in
\code{utils}) also allows this to be a character vector of a file
path, connection or url (although use as url currently not fully
tested). }

\item{file.type}{ The file format, defaults to common "csv"
  (comma delimited) format, but also allows "txt" (tab delimited). }

\item{header.at}{ The file row holding header information. This is used
  to set names for the resulting imported data frame. }

\item{data.at}{The file row to start reading data from. When generating the data frame,
the function will ignore all information before this row, and attempt to
include all data from this row onwards unless eof.report enabled. }

\item{eof.report}{ End of file marker. When genearating the data frame,
the function will ignore all information after eof.report is
encountered. The default setting (NULL) turns this argument off. }

\item{na.strings}{ Strings of any terms that are to be interpreted as NA
  values within the file. }

\item{quote}{ String of characters (or character equivalents) the imported file 
may use to represent a character field. }

\item{date.name}{ Header name of column (or columns)
holding date information. Combined with time information as single date
column in the generated data frame. }

\item{date.break}{ The break
character separating days, months and years in date information. For
example, "-" in "01-01-2009". }

\item{date.order}{ The order of date
information, using d for days, j for Julian date, m for months and y for years. 
So, "dmy" or "mdy" for common UK or US logger date stamp formats. Allows any
logical combination ("y", "ymd", etc). Can also handle more complex date 
structures by calling POSIX* directly. For example, "posix %d-%b-%Y" would 
apply the POSIX* format structure "%d-%b-%Y" and allow the import of date enteries 
like "01-JAN-2010". See associated help, ?format.POSIXct, for full list of format 
options. (Note: direct POSIX calls supercede other date settings.)}

\item{time.name}{ Header name of column (or columns) holding time 
information. Combined with date information as
single date column in the generated data frame. }

\item{time.break}{ The
break character separating hours, minutes and seconds in time
information. For example, ":" in "12:00:00". }

\item{time.order}{ The
order of time information, using h for hours, m for minutes and s for
seconds. The argument allows any logical combination ("hm", "hms",
etc). Like date.order, can also handle more complex date 
structures by calling POSIX* directly. For example, "posix %I:%M %p" would 
apply the POSIX* format structure "%I:%M %p" and allow the import of time enteries 
like "00:01 PM". See associated help, ?format.POSIXct, for full list of format 
options. (Note: direct POSIX calls supercede other time settings.)}

\item{time.format}{ The time format the imported data was logged in. 
Allows most common formats, e.g.: "GMT" (default), "UTC", etc. 
See \code{as.POSIX*} functions for further information. }

\item{is.ws}{ Wind speed information identifier. Default
NULL turns this option off. When set to valid header/data column name,
used to select wind speed data. Note: data renamed "ws" as part of this
operation. }

\item{is.wd}{ Wind direction information
identifier. Default NULL turns this option off. When set to valid
header/data column name, used to select wind direction data. Note: data
renamed "wd" as part of this operation. }

\item{is.site}{ Site
information identifier. Default NULL turns this option off. When set,
the standard (import) method uses this information to generate a "site" 
data column.}

\item{misc.info}{ Row number(s) of
any additional information that may be required from the original
file. Each line retained as a character vector in the generated data
frame comment. }

\item{bad.24}{ Time stamp reset. Some time series are
logged as 00:00:01 to 24:00:00 as opposed to the more conventional
00:00:00 to 23:59:59. bad.24 = TRUE resets the time stamp for the
latter, which is not allowed by some R time series classes and
functions. }

\item{correct.time}{ Numerical correction (in seconds) for
imported date. Default NULL turns this option off. When enabled, used to
offset "date" entries. }

\item{output}{ Type of data object to be
generated. Default "final" returns a standard data set for use in
openair. Alternative "working" returns a list of file components without testing 
file structure. This Option is intended to be used with wrapper functions. }

}

\details{ The \code{import()} function was
developed to import and format data for direct use
with the openair package. The main intention was to simplify initial data
handling for those unfamilar with R, and, in particular, associated time series
formatting requirements. Using default settings, \code{import()} imports files
configured like example file "example data long.csv" (supplied with
openair or available from the openair website). 

Other similar file structures can be readily imported by modifying the function
arguments. 

More complex data importing and formatting can be achieved
using an import wrapper.  For example, the \code{importAURNCsv} is an import
wrapper that uses \code{import()} with modified arguments to import data 
previously downloaded from the UK AURN database. This enforces unique handling of "is.site" and
employs two additional arguments, "data.order" and "simplify.names" and 
\code{rbind} (in \code{reshape}) to complete additional
reformatting. }

\value{ Using the default \code{output = "final"}
setting, the function returns a data frame for use in openair. By
comparison to the original file, the resulting data frame is modified as
follows: Time and date information will combined in a single column
"date", formatted as a conventional timeseries (as.POSIX*). Time
adjustments may also be made, subject to bad.24 and correct.time
argument settings. Columns identified as wind speed and wind direction 
information using "is.ws" and "is.wd", respectively, will be renamed "ws" and "wd",
respectively.  An additional "site" column will be generated if enabled by "is.site". 
Any additional information (as defined in "misc.info") and data adjustments
(as set in''bad.24' and 'correct.time') will be retained in the data
frame comment. 

Using the alternative \code{output = "working"} setting, 
the function returns a list containing separate data frames for the different elements of the 
data frame (data, names, date, misc.info, etc.).   
}

\author{ Karl Ropkins }


\seealso{Dedicated import functions available for selected file types, e.g. :
\code{\link{importAURN}}, \code{\link{importAURNCsv}}, \code{\link{importKCL}}, 
\code{\link{importADMS}}, etc. }

\examples{

##########
# example 1
##########
# data obtained from http://www.openair-project.org

#import data as mydata
## mydata <- import("example data long.csv")

#use openair function
## polar.plot(mydata, pollutant="nox")


}
\keyword{methods}


