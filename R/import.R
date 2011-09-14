##' Generic data import for openair
##'
##' Generic (workhorse) function for importing and formatting data for use with
##' the openair package. The function uses \code{read.table} (in \code{utils}).
##'
##' The \code{import()} function was developed to import and format data for
##' direct use with the openair package. The main intention was to simplify
##' initial data handling for those unfamilar with R, and, in particular,
##' associated time series formatting requirements. Using default settings,
##' \code{import()} imports files configured like example file "example data
##' long.csv" (supplied with openair or available from the openair website).
##'
##' Other similar file structures can be readily imported by modifying the
##' function arguments.
##'
##' More complex data importing and formatting can be achieved using an import
##' wrapper.  For example, the \code{importAURNCsv} is an import wrapper that
##' uses \code{import()} with modified arguments to import data previously
##' downloaded from the UK AURN database. This enforces unique handling of
##' "is.site" and employs two additional arguments, "data.order" and
##' "simplify.names" and \code{rbind} (in \code{reshape}) to complete
##' additional reformatting.
##'
##' @param file The name of the file to be imported. Default, \code{file =
##'   file.choose()}, opens browser. Alternatively, the use of
##'   \code{read.table} (in \code{utils}) also allows this to be a character
##'   vector of a file path, connection or url (although use as url currently
##'   not fully tested).
##' @param file.type The file format, defaults to common "csv" (comma
##'   delimited) format, but also allows "txt" (tab delimited).
##' @param sep R-style file separator, e.g \code{sep = ","} for comma
##'   delimited; see \code{read.table} for further details of file type. If
##'   set, supersedes \code{file.type}.
##' @param header.at The file row holding header information or \code{NULL} if
##'   no header to be used. If valid numeric, this is used to set names for the
##'   resulting imported data frame. If \code{NULL} default column names are
##'   generated that can then be modified using R function \code{names}. Note:
##'   If NULL no header is identified and associated column identifiers
##'   (\code{date.name}, \code{field.name}, \code{is.ws}, \code{is.wd} and
##'   \code{is.site}) must be numeric.
##' @param data.at The file row to start reading data from. When generating the
##'   data frame, the function will ignore all information before this row, and
##'   attempt to include all data from this row onwards unless eof.report
##'   enabled.
##' @param eof.report End of file marker. When genearating the data frame, the
##'   function will ignore all information after eof.report is encountered. The
##'   default setting (NULL) turns this argument off.
##' @param na.strings Strings of any terms that are to be interpreted as NA
##'   values within the file.
##' @param quote String of characters (or character equivalents) the imported
##'   file may use to represent a character field.
##' @param date.name Header name or number of column (or columns) holding date
##'   information. Combined with time information as single date column in the
##'   generated data frame.
##' @param date.break The break character separating days, months and years in
##'   date information. For example, "-" in "01-01-2009".
##' @param date.order The order of date information, using d for days, j for
##'   Julian date, m for months and y for years. So, "dmy" or "mdy" for common
##'   UK or US logger date stamp formats. Allows any logical combination ("y",
##'   "ymd", etc). Can also handle more complex date structures by calling
##'   POSIX* directly. For example, "posix %d-%b-%Y" would apply the POSIX*
##'   format structure "%d-%b-%Y" and allow the import of date enteries like
##'   "01-JAN-2010". See associated help, ?format.POSIXct, for full list of
##'   format options. (Note: direct POSIX calls supercede other date settings.)
##' @param time.name Header name or number of column (or columns) holding time
##'   information. Combined with date information as single date column in the
##'   generated data frame.
##' @param time.break The break character separating hours, minutes and seconds
##'   in time information. For example, ":" in "12:00:00".
##' @param time.order The order of time information, using h for hours, m for
##'   minutes and s for seconds. The argument allows any logical combination
##'   ("hm", "hms", etc). Like date.order, can also handle more complex date
##'   structures by calling POSIX* directly. For example, "posix %I:%M %p"
##'   would apply the POSIX* format structure "%I:%M %p" and allow the import
##'   of time enteries like "00:01 PM". See associated help, ?format.POSIXct,
##'   for full list of format options. (Note: direct POSIX calls supercede
##'   other time settings.)
##' @param time.format The time format the imported data was logged in. Allows
##'   most common formats, e.g.: "GMT" (default), "UTC", etc. See
##'   \code{as.POSIX*} functions for further information.
##' @param cipher Alternative date and time handling method, currently in
##'   development. If supplied the key date/time Saturday 1st February 2003
##'   16:05:06 (or nearest case) in the format used within the supplied file,
##'   \code{cipher} attempts to decode this and format supplied date/time
##'   information using the associated method. \code{cipher} is more flexible
##'   than \code{date.order} and \code{time.order}, allowing the import of both
##'   numeric and non-numeric month descriptors (02, Feb, February), YY and
##'   YYYY year formats (03, 2003), days of week (Sat, Saturday), and 12 and 24
##'   hour time stamps (04:05 PM, 16:05). Like \code{date.order} and
##'   \code{time.order}, \code{cipher} can also handle more complex formats by
##'   calling POSIX* directly. Note: This option is currently in development
##'   and should be used with care.
##' @param is.ws Wind speed information identifier. Default NULL turns this
##'   option off. When set to valid header name or data column number, used to
##'   select wind speed data. Note: data renamed "ws" as part of this
##'   operation.
##' @param is.wd Wind direction information identifier. Default NULL turns this
##'   option off. When set to valid header name or data column number, used to
##'   select wind direction data. Note: data renamed "wd" as part of this
##'   operation.
##' @param is.site Site information identifier. Default NULL turns this option
##'   off. When set to valid header name or data column number, the standard
##'   (import) method uses this information to generate a "site" data column.
##' @param misc.info Row number(s) of any additional information that may be
##'   required from the original file. Each line retained as a character vector
##'   in the generated data frame comment.
##' @param bad.24 Time stamp reset. Some time series are logged as 00:00:01 to
##'   24:00:00 as opposed to the more conventional 00:00:00 to 23:59:59. bad.24
##'   = TRUE resets the time stamp for the latter, which is not allowed by some
##'   R time series classes and functions.
##' @param correct.time Numerical correction (in seconds) for imported date.
##'   Default NULL turns this option off. When enabled, used to offset "date"
##'   entries.
##' @param previous Logical (TRUE/FALSE) to use earlier version of import.
##'   Default FALSE uses the most recent version of import. Alternative TRUE
##'   accesses earlier alternative.
##' @param output Type of data object to be generated. Default "final" returns
##'   a standard data set for use in openair. Alternative "working" returns a
##'   list of file components without testing file structure. This Option is
##'   intended to be used with wrapper functions.
##' @return Using the default \code{output = "final"} setting, the function
##'   returns a data frame for use in openair. By comparison to the original
##'   file, the resulting data frame is modified as follows: Time and date
##'   information will combined in a single column "date", formatted as a
##'   conventional timeseries (as.POSIX*). Time adjustments may also be made,
##'   subject to bad.24 and correct.time argument settings. Columns identified
##'   as wind speed and wind direction information using "is.ws" and "is.wd",
##'   respectively, will be renamed "ws" and "wd", respectively.  An additional
##'   "site" column will be generated if enabled by "is.site". Any additional
##'   information (as defined in "misc.info") and data adjustments (as set
##'   in''bad.24' and 'correct.time') will be retained in the data frame
##'   comment.
##'
##' Using the alternative \code{output = "working"} setting, the function
##'   returns a list containing separate data frames for the different elements
##'   of the data frame (data, names, date, misc.info, etc.).
##' @author Karl Ropkins
##' @export
##' @seealso Dedicated import functions available for selected file types, e.g.
##'   : \code{\link{importAURN}}, \code{\link{importAURNCsv}},
##'   \code{\link{importKCL}}, \code{\link{importADMS}}, etc.
##' @keywords methods
##' @examples
##'
##'
##' ##########
##' # example 1
##' ##########
##' # data obtained from http://www.openair-project.org
##'
##' #import data as mydata
##' # basic plot
##' \dontrun{mydata <- import("example data long.csv")}
##'
##' #use openair function
##' \dontrun{polar.plot(mydata, pollutant="nox")}
##'
##'
##'
import <- function (file = file.choose(), file.type = "csv", sep = NULL, header.at = 1,
    data.at = 2, eof.report = NULL, na.strings = c("", "NA"),
    quote = "\"", date.name = "date", date.break = "/", date.order = "dmy",
    time.name = "date", time.break = ":", time.order = "hm",
    time.format = "GMT", cipher = NULL, is.ws = NULL, is.wd = NULL, is.site = NULL,
    misc.info = NULL, bad.24 = FALSE, correct.time = NULL, previous = FALSE,
    output = "final")
{

    ################################
    #multi-format data importer
    ################################
    #kr version 0.2.2
    #09/05/2011
    ###############################
    #

    ##############################
    #recent changes
    ##############################
    #brought forward file and name import
    ##no longer impossible to defer to last min!!!
    #add new date.time handler cipher
    ##see date.time.cipher at end
    #

    ##################
    #to do
    ##################
    #

    ##################
    #suggestions
    ##################
    #error catcher on read in
    #

##################
#access to older importer via previous
#wraps around main function
#import.2 is previous 'hidden' version
##################
#when move to previous
#remove access to previous
#else messy!
##################
if(previous)
    import.2(file = file, file.type = file.type, header.at = header.at,
    data.at = data.at, eof.report = eof.report, na.strings = na.strings,
    quote = quote, date.name = date.name, date.break = date.break, date.order = date.order,
    time.name = time.name, time.break = time.break, time.order = time.order,
    time.format = time.format, is.ws = is.ws, is.wd = is.wd, is.site = is.site,
    misc.info = misc.info, bad.24 = bad.24, correct.time = correct.time,
    output = output) else {


    ###################
    #main body
    ###################

    ##################
    #file type
    ##################
    #overridden by sep if character
    #

    if(!is.character(sep)){
        sep <- if(file.type == "txt")
                   "\t" else ","
    }

    ##################
    #file data read in
    ##################
    file.data <- read.table(file, header = FALSE, sep = sep,
        skip = (data.at - 1), nrows = -1, na.strings = na.strings,
        quote = quote, fill = TRUE)
    if (!is.null(eof.report)) {
        if (is.na(match(eof.report, as.character(file.data$V1))) ==
            FALSE) {
            file.data <- file.data[1:(match(eof.report, as.character(file.data$V1)) -
                1), ]
        }
    }

    #################
    #file names read in
    #################
    file.names <- if(is.null(header.at) || header.at < 1)  paste("...XxX", 1:ncol(file.data), sep="") else
        read.table(file, header = FALSE, sep = sep,
            quote = quote, skip = (header.at - 1), nrows = 1, colClasses = "character")
    file.names <- as.character(file.names)

    ###############
    #handle numerics and header=NULL
    ##############

    #if header not set
    #need numeric or null for field sources
    if(is.null(header.at) || is.numeric(header.at) && header.at < 1){
        temp <- unlist(lapply(c("date.name", "time.name", "is.ws", "is.wd", "is.site"),
            function(x) if(!is.null(get(x)) & !is.numeric(get(x))) x))
        if(!is.null(temp))
            stop("Invalid import options for data import without header,\n",
                 "       [reset header or use numeric data field/column identifiers]\n",
                 "       [current conflicts: ", paste(temp, collapse = ", "), "]",
                 call. = FALSE)
    }

    #handle numeric field sources
    temp <- function(x)
        if(is.numeric(x)) {
        x <- file.names[subset(x <- as.integer(x), x > 0 & x <= length(file.names))]
        x <- if(length(x) < 1) NULL else x
    } else x

    date.name <- temp(date.name)
    time.name <- temp(time.name)
    is.ws <- temp(is.ws)
    is.wd <- temp(is.wd)
    is.site <- temp(is.site)

    temp <- c(unique(c(date.name, time.name)), is.ws, is.wd, is.site)
    temp <- temp[duplicated(temp)]
    temp <- unlist(lapply(c("date.name", "time.name", "is.ws", "is.wd", "is.site"),
               function(x) if(any(get(x) %in% temp)) x))
    if(!is.null(temp))
        stop("Invalid import option combination,\n",
             "       [conflicting options: ",
             paste(temp, collapse = ", "), "]",
            call. = FALSE)

    ###################
    #check date and time names
    ###################
    time.name <- time.name[!time.name %in% date.name]
    if (length(c(date.name, time.name)) == 0) {
        stop("No valid date or times set\n       [openair import currently require at least one]",
            call. = FALSE)
    }

    ####################
    #misc.info read in
    ####################
    if (!is.null(misc.info[1])) {
        if (is.numeric(misc.info)) {
            file.misc <- readLines(file, n = max(misc.info))
            file.misc <- file.misc[misc.info]
        }
        else {
            file.misc <- misc.info
        }
    }

    ####################
    #date/time check
    ###################
    if (any(!c(date.name, time.name) %in% file.names)) {
        missing.date.name <- date.name[(!date.name %in% file.names)]
        missing.time.name <- time.name[(!time.name %in% file.names)]
        reply <- "Import conflicts;"
        if (length(missing.date.name) > 0) {
            reply <- paste(reply, "\n       missing date.name",
                sep = "")
            if (length(missing.date.name) > 1) {
                reply <- paste(reply, "s", sep = "")
            }
            reply <- paste(reply, paste(missing.date.name, collapse = ", ",
                sep = ""), sep = ": ")
        }
        if (length(missing.time.name) > 0) {
            reply <- paste(reply, "\n       missing time.name",
                sep = "")
            if (length(missing.time.name) > 1) {
                reply <- paste(reply, "s", sep = "")
            }
            reply <- paste(reply, paste(missing.time.name, collapse = ", ",
                sep = ""), sep = ": ")
        }
        reply <- paste(reply, "\n       [compare openair import settings and data structure]",
            sep = "")
        stop(reply, call. = FALSE)
    }

    ######################
    #other field setup
    ######################
    if (!is.null(is.ws)) {
        file.names <- gsub(is.ws, "ws", file.names, ignore.case = FALSE)
    }
    if (!is.null(is.wd)) {
        file.names <- gsub(is.wd, "wd", file.names, ignore.case = FALSE)
    }
    if (!is.null(is.site)) {
        file.names <- gsub(is.site, "site", file.names, ignore.case = FALSE)
    }

    #########################
    #check name and data dimensions match
    #########################
    if(ncol(file.data)!=length(file.names)){
        if(ncol(file.data)<length(file.names)) {
            file.names <- file.names[1:ncol(file.data)]
            warning("Unexpected extra names extracted, dropped unassigned names\n       [check openair import settings and data structure if unexpected]"
                , call. = FALSE)
        } else {
            file.names <- c(file.names, paste("new", 1:(ncol(file.data)-length(file.names)), sep="."))
            warning("Unexpected extra data extracted, extra names created\n       [check openair import settings and data structure if unexpected]"
                , call. = FALSE)

        }
    }

    ######################
    #bind data and names
    ######################
    #includes forcing names to valid R

    temp <- make.names(file.names, unique = TRUE)
    if (!identical(file.names, temp)){
        date.name <- make.names(date.name)
        time.name <- make.names(time.name)
        is.ws <- make.names(is.ws)
        is.wd <- make.names(is.wd)
        is.site <- make.names(is.site)
        warning("Non-unique or non-R names extracted, names modifications applied\n       [check openair import settings and data structure if unexpected]",
                call. = FALSE)
    }
    names(file.data) <- temp

    #####################
    #date/time setup
    #####################
    #old and new handlers

    if(is.null(cipher)==FALSE && is.character(cipher)){
        #new handler
        #overrides old
        #uses cipher and date.break, time.break if pasting things together
        date.order <- date.time.cipher(cipher[1])
    } else {
        #old handler
        #uses date.break, date.order, time.break, time.order
        if(tolower(substr(date.order,1,5))=="posix") {
            date.order <- gsub("posix", "", date.order, ignore.case = TRUE)
            date.order <- gsub("(^ +)|( +$)", "", date.order)
        } else {
            date.order <- gsub("d", paste("%d", date.break, sep = ""),
                date.order, ignore.case = TRUE)
            date.order <- gsub("j", paste("%j", date.break, sep = ""),
                date.order, ignore.case = TRUE)
            date.order <- gsub("m", paste("%m", date.break, sep = ""),
                date.order, ignore.case = TRUE)
            date.order <- gsub("y", paste("%Y", date.break, sep = ""),
                date.order, ignore.case = TRUE)
            date.order <- substr(date.order, 1, (nchar(date.order) -
                1))
        }
        if(tolower(substr(time.order,1,5))=="posix") {
            time.order <- gsub("posix", "", time.order, ignore.case = TRUE)
            time.order <- gsub("(^ +)|( +$)", "", time.order)
        } else {
            time.order <- gsub("h", paste("%H", time.break, sep = ""),
                time.order, ignore.case = TRUE)
            time.order <- gsub("m", paste("%M", time.break, sep = ""),
                time.order, ignore.case = TRUE)
            time.order <- gsub("s", paste("%S", time.break, sep = ""),
                time.order, ignore.case = TRUE)
            time.order <- substr(time.order, 1, (nchar(time.order) -
                1))
        }
        date.order <- paste(date.order, time.order, sep = " ")
    }



    if (length(date.name) > 0) {
        if (length(date.name) == 1) {
            a <- as.character(file.data[, date.name])
        }
        else {
            a <- apply(file.data[, date.name], 1, paste, collapse = date.break)
        }
    }
    else {
        a <- NULL
    }

    if (length(time.name) > 0) {
        if (length(time.name) == 1) {
            b <- as.character(file.data[, time.name])
        }
        else {
            b <- apply(file.data[, time.name], 1, paste, collapse = time.break)
        }
    }
    else {
        b <- NULL
    }

    b <- apply(cbind(a, b), 1, paste, collapse = " ")
    a <- as.POSIXct(strptime(b, format = date.order, time.format))

    ######################
    #removed yy/yyyy tester
    #due to 2.11/2.12 handling
    ######################
    ###yy/yyyy tester
    ###if invalid try year
    ###NOTE: Can't test for Y first
    ###(Y%=01 is year 0001!, etc
    ##if(all(is.na(a))){
    ##    date.order <- gsub("y", "Y", date.order)
    ##    a <- as.POSIXct(b, format = paste(date.order, time.order,
    ##        sep = " "), time.format)
    ##}

    if (bad.24 == TRUE) {
        bad.time <- gsub("%H", "24", time.order, ignore.case = TRUE)
        bad.time <- gsub("%M", "00", bad.time, ignore.case = TRUE)
        bad.time <- gsub("%S", "00", bad.time, ignore.case = TRUE)
        good.time <- gsub("24", "00", bad.time)
        ###########################
        ##where bad.time replace with good and add day
        a[grep(bad.time, b, ignore.case = TRUE)] <-
             as.POSIXct(strptime(gsub(bad.time, good.time, b[grep(bad.time, b, ignore.case = TRUE)]),
             format = date.order, tz = time.format)) + 86400
        if (is.null(misc.info)) {
            misc.info <- 1
            file.misc <- "import operation: bad.24 applied (reset 24:00:00 to 00:00:00 next day)"
        }
        else {
            file.misc <- c(file.misc, "import operation: bad.24 applied (reset 24:00:00 to 00:00:00 next day)")
        }
    }
    if (!is.null(correct.time)) {
        a <- a + correct.time
        if (is.null(misc.info)) {
            misc.info <- 1
            file.misc <- paste("import operation: correct.time applied (",
                correct.time, " seconds)", sep = "")
        }
        else {
            file.misc <- c(file.misc, paste("import operation: correct.time applied (",
                correct.time, " seconds)", sep = ""))
        }
    }

    file.data <- file.data[!names(file.data) %in% c(date.name,
        time.name)]

    ###############
    #tidy fields are editoring
    ###############
    names(file.data) <- gsub("[.][.][.]XxX", "x", names(file.data))
    file.names <- gsub("[.][.][.]XxX", "x", file.names)
    file.names2 <- file.names[!file.names %in% c(date.name, time.name)]

    ###################
    #outputs
    ###################
    if (!output == "working") {
        if (!is.null(misc.info)) {
            comment(file.data) <- file.misc
        }
        ans <- cbind(date = a, file.data)
        ids <- which(is.na(ans$date))
        if (length(ids) > 0) {
            if (length(ids) == nrow(ans)) {
                stop("Invalid date (and time) format requested\n       [compare openair import settings and data structure]",
                  call. = FALSE)
            }
            ans <- ans[-ids, ]
            warning(paste("Missing dates detected, removing",
                length(ids), "lines"), call. = FALSE)
        }
        print(unlist(sapply(ans, class)))
        return(ans)
    }
    else {
        ans <- list(data = file.data, names = file.names, names2 = file.names2,
            date = a, ops = list(sep = sep))
        if (!is.null(misc.info)) {
            ans$misc <- file.misc
        }
        return(ans)
    }
##################
#access to older importer via previous
##################
}

}


date.time.cipher <- function(cipher){

###########################
#cipher handler for date.time format
###########################
#v 0.0.1
#kr 31 10 2010
#

###########################
#notes
###########################
#alternative format control for import
#

###########################
#to do
###########################
#month


#local day of week
cipher <- gsub(make.weekday.abbs()[6], "%a", cipher)
cipher <- gsub(make.weekday.names()[6], "%A", cipher)

#day of year
cipher <- gsub("32", "%j", cipher)

#day of month
cipher <- gsub("01", "%d", cipher)

#month of year
cipher <- gsub("02", "%m", cipher)
cipher <- gsub(make.month.abbs()[2], "%b", cipher)
cipher <- gsub(make.month.names()[2], "%B", cipher)

#year
cipher <- gsub("2003", "%Y", cipher)
cipher <- gsub("03", "%y", cipher)

#hour
cipher <- gsub("04", "%I", cipher)
cipher <- gsub("16", "%H", cipher)

#am/pm
cipher <- gsub("PM", "%p", cipher)

#mins
cipher <- gsub("05", "%M", cipher)

#secs
cipher <- gsub("06.", "%OS", cipher)
cipher <- gsub("06", "%S", cipher)

#note:
#time zone, output only
#so, handled in main structure at moment

#at end tidy
#strip out unknown numerics
cipher <- gsub("[0-9]", "", cipher)
cipher <- gsub("posix", "", cipher, ignore.case = TRUE)
cipher <- gsub("(^ +)|( +$)", "", cipher)

cipher
}


