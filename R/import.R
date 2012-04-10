##' Generic data import for openair
##'
##' This function is mostly used to simplify the importing of csv and
##' text file in \code{openair}. In particular it helps to get the
##' date or date/time into the correct format. The file can contain
##' either a date or date/time in a single column or a date in one
##' column and time in another.
##'
##' The function uses \code{\link{strptime}} to parse dates and
##' times. Users should consider the examples for use of these
##' formats.
##'
##' \code{import} will also ensure wind speed and wind direction are
##' correctly labelled (i.e. "ws", "wd") if \code{ws} or \code{wd} are
##' given.
##'
##' @param file The name of the file to be imported. Default, \code{file =
##'   file.choose()}, opens browser. Alternatively, the use of
##'   \code{read.table} (in \code{utils}) also allows this to be a character
##'   vector of a file path, connection or url.
##' @param file.type The file format, defaults to common "csv" (comma
##'   delimited) format, but also allows "txt" (tab delimited).
##' @param sep Allows user to specify a delimiter if not "," (csv) or
##' TAB (txt). For example ";" is sometimes used to delineate separate
##' columns.
##' @param header.at The file row holding header information or \code{NULL} if
##'   no header to be used.
##' @param data.at The file row to start reading data from. When generating the
##'   data frame, the function will ignore all information before this row, and
##'   attempt to include all data from this row onwards.
##' @param date Name of the field containing the date. This can be a
##' date e.g. 10/12/2012 or a date-time format e.g. 10/12/2012 01:00.
##' @param date.format The format of the date. This is given in 'R'
##' format according to \code{strptime}. For example, a date format
##' such as 1/11/2000 12:00 (day/month/year hour:minutes) is given the
##' format "\%d/\%m/\%Y \%H:\%M". See examples below and \code{strptime}
##' for more details.
##' @param time The name of the column containing a time --- if there
##' is one. This is used when a time is given in a separate column and
##' \code{date} contains no information about time.
##' @param time.format If there is a column for \code{time} then the
##' time format must be supplied. Common examples include "\%H:\%M"
##' (like 07:00) or an integer giving the hour, in which case the
##' format is "\%H". Again, see examples below.
##' @param tz.in The time zone of the data being read. Most of the
##' time this field can be ignored. However, one situation where it is
##' useful to supply \code{tz.in} is if the original data considered
##' daylight saving time i.e. there is an hour missing in spring and
##' duplicated in autumn. An example for UK data would be \code{tz.in
##' = "Europe/London"}.
##' @param tz.out The time zone of the output to be used by
##' \code{openair} functions.
##' @param na.strings Strings of any terms that are to be interpreted
##' as missing (\code{NA}). For example, this might be "-999", or
##' "n/a" and can be of several items.
##' @param quote String of characters (or character equivalents) the imported
##'   file may use to represent a character field.
##' @param ws Name of wind speed field if present if different from
##' "ws" e.g. \code{ws = "WSPD"}.
##' @param wd Name of wind direction field if present if different
##' from "wd" e.g. \code{wd = "WDIR"}.
##' @param correct.time Numerical correction (in seconds) for imported
##' date.  Default \code{NULL} turns this option off. This can be useful if
##' the hour is represented as 1 to 24 (rather than 0 to 23 assumed by
##' R). In which case \code{correct.time = -3600} will correct the
##' hour.
##' @param ... Other arguments passed to \code{read.table}.
##' @return A data frame formatted for openair use.
##' @author David Carslaw
##' @export
##' @seealso Dedicated import functions available for selected file types, e.g.
##'   : \code{\link{importAURN}}, \code{\link{importAURNCsv}},
##'   \code{\link{importKCL}}, \code{\link{importADMS}}, etc.
##' @keywords methods
##' @examples
##'
##' ## Note that more examples are given in the openair manual
##'
##' \dontrun{
##' ## import a file with date in format 1/12/2000 10:00 (default format dd/mm/YYYY HH:MM) called "DATE"
##' thedata <- import("~/data/testdata.csv", date = "DATE")
##'
##' ## import a file with date in format 12/1/2000 10:00 (USA format mm/dd/YYYY HH:MM) called "DATE"
##' thedata <- import("~/data/testdata.csv", date = "DATE", date.format = "%m/%d/%Y %H:%M")
##'
##' ## import a file where date and time are in separate columns with
##' ## names "date" (called "Date" in format dd/mm/YYYY) and "time"
##' ## (called "Time" in format "HH:MM")
##' thedata <- import("~/data/testdata.csv", date = "Date", date.format = "%d/%m/%Y",
##' time = "Time", time.format = "%H:%M")
##' }
##'
import <- function (file = file.choose(), file.type = "csv", sep = ",", header.at = 1,
                    data.at = 2,  date = "date", date.format = "%d/%m/%Y %H:%M",
                    time = NULL,  time.format = NULL,
                    tz.in = "GMT", tz.out = "GMT", na.strings = c("", "NA"),
                    quote = "\"", ws = NULL, wd = NULL,
                    correct.time = NULL, ...)
{

    if (file.type == "csv") sep <- "," else sep <- ""

    if (!missing(sep)) sep <- sep

    ## read header
    if (header.at > 0 ) {
        Names <- read.table(file, nrows = 1, skip = (header.at - 1), sep = sep,
                            colClasses = "character", na.strings = "")

        ## deal with header columns that are left blank
        if (any(is.na(Names))) {
            id <- which(is.na(Names))
            Names[id] <- colnames(Names)[id]

        }
    }

    ## read data
    thedata <- read.table(file, skip = (data.at - 1), sep = sep, na.strings = na.strings,
                          quote = quote, stringsAsFactors = FALSE, ...)

    names(thedata) <- Names

    ## rename date field
    if (!date %in% Names) stop (paste("Can't find variable", date))
    names(thedata)[which(Names == date)] <- "date"

    if (!is.null(ws)) {
        if (!ws %in% Names) stop (paste("Can't find variable", ws))
        names(thedata)[which(Names == ws)] <- "ws"
    }

    if (!is.null(wd)) {
        if (!wd %in% Names) stop (paste("Can't find variable", wd))
        names(thedata)[which(Names == wd)] <- "wd"
    }


    ## set date format - if no time column use date format directly
    if (is.null(time)) {

        thedata$date <- as.POSIXct(strptime(thedata$date, format = date.format, tz = tz.in))

        ## if all dates are NA, there is a problem...
        if (all(is.na(thedata$date))) stop ("Date conversion problems, check that date.format is correct")

    } else {

        ## time is in a separate column
        thedata$date <- as.POSIXct(strptime(paste(thedata$date, thedata[, time]),
                                            format = paste(date.format, time.format),
                                            tz = tz.in))

        ## if all dates are NA, there is a problem...
        if (all(is.na(thedata$date))) stop ("Date conversion problems, check that date.format and/or time.format is correct")
    }

    if (!is.null(correct.time)) thedata$date <- thedata$date + correct.time

    attr(thedata$date, "tzone") <- tz.out

    ## deal with missing dates
    ids <- which(is.na(thedata$date))
    if (length(ids) > 0) {

            thedata <- thedata[-ids, ]
            warning(paste("Missing dates detected, removing",
                length(ids), "lines"), call. = FALSE)
        }

    ## print data types - helps with debugging
    print(unlist(sapply(thedata, class)))

    thedata
}

