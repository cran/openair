import <- function (file = file.choose(), file.type = "csv", header.at = 1, 
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
    #kr version 0.2.1
    #21/10/2010
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

    if (file.type == "txt") {
        sep <- "\t"
    }
    else {
        sep <- ","
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
    date.name <- make.names(date.name[date.name != ""])
    time.name <- make.names(time.name[time.name != ""])
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
    temp <- make.names(file.names, unique = TRUE)
    if(!identical(file.names,temp))
        warning("Non-unqiue or non-R names extracted, names modifications applied\n       [check openair import settings and data structure if unexpected]"
                , call. = FALSE) 
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
    a <- as.POSIXct(b, format = date.order, time.format)

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
        a[grep(bad.time, b, ignore.case = TRUE)] <- 
            as.POSIXct(
                as.POSIXlt(strptime(as.character(b[grep(bad.time, b, ignore.case = TRUE)]), 
                format = date.order, tz = time.format))) + 86400
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


