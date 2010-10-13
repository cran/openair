                                        # Check input file and prepare data
                                        #
                                        # Author: DCC
###############################################################################

checkPrep <- function(mydata, Names, type, remove.calm = TRUE) {

    ## deal with conditioning variable if present, if user-defined, must exist in data
    ## pre-defined types
    ## existing conditioning variables that only depend on date (which is checked)
    conds <- c("default", "year", "hour", "month", "season", "weekday", "weekend", "monthyear", "gmtbst", "bstgmt")
    all.vars <- unique(c(names(mydata), conds))

    varNames <- c(Names, type) ## names we want to be there
    matching <- varNames %in% all.vars

    if (any(!matching)) {
        ## not all variables are present
        stop(cat("Can't find the variable(s)", varNames[!matching], "\n"))
    }

    ## add type to names if not in pre-defined list
    if (type %in% conds == FALSE) Names <- c(Names, type)

    ## just select data needed
    mydata <- mydata[, Names]

    ## check to see if there are any missing dates, stop if there are
    if ("date" %in% names(mydata)) {
        if (any(is.na(mydata$date))) {
            stop (cat("There are some missing dates on line(s)", which(is.na(mydata$date))),"\n")
        }
    }

        ## sometimes ratios are considered which can results in infinite values
        ## make sure all infinite values are set to NA
        mydata[] <- lapply(mydata, function(x){replace(x, x == Inf | x == -Inf, NA)})

        ## round wd to make processing obvious
        ## data already rounded to nearest 10 degress will not be affected
        ## data not rounded will be rounded to nearest 10 degrees
        ## assumes 10 is average of 5-15 etc

        if ("wd" %in% Names) {
            ## force to be numeric
            ## mydata$wd <- as.numeric(mydata$wd)

                                        # #check for wd <0 or > 360
            if (any(sign(mydata$wd[!is.na(mydata$wd)]) == -1 | mydata$wd[!is.na(mydata$wd)] > 360)) {

                warning("Wind direction < 0 or > 360; removing these data")
                mydata$wd[mydata$wd < 0] <- NA
                mydata$wd[mydata$wd > 360] <- NA
            }

            if (remove.calm) {
                mydata$wd[mydata$wd == 0] <- NA

                ## round wd for use in functions - except windRose/pollutionRose
                mydata$wd <- 10 * round(mydata$wd / 10)
                mydata$wd[mydata$wd == 0] <- 360   # angles <5 should be in 360 bin 
            } else { ## only used for windRose
                mydata$wd[mydata$wd == 0] <- -999
                mydata$wd[mydata$ws == 0] <- -999 ## some times met data wrong, need both here!
                mydata$ws[mydata$wd == -999] <- 0
            }
       
        }

        if ("ws" %in% Names) {
            ## force to be numeric
            ## mydata$ws <- as.numeric(mydata$ws)
            ## check for negative wind speeds
            if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {

                warning("Wind speed <0; removing negative data")
                mydata$ws[mydata$ws < 0] <- NA
            }
        }

        ## make sure date is ordered in time if present
        if ("date" %in% Names) {

            ## if date in format dd/mm/yyyy hh:mm (basic check)
            if (length(grep("/", as.character(mydata$date[1]))) > 0) {

                mydata$date <- as.POSIXct(strptime(mydata$date, "%d/%m/%Y %H:%M"), "GMT")

            }

            mydata <- mydata[order(mydata$date), ]

            ## make sure date is the first field
            mydata <- cbind(subset(mydata, select = date), subset(mydata,select = -date))
        }

        ## return data frame
        mydata
    }



