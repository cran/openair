

## Function to flexibly calculate different averagring times for data frames with
## option to also set data.thresh threshold.
## Note that "period" uses those defined in cut.POSIXct e.g. "days", "5 days", and
## is therefore extremely flexible.
## The function will also work with mutiple sites.
## start.date helps to get data frame into nice time sequence
## NOTE - will only return numeric data apart from site name

timeAverage <- function(mydata, avg.time = "day", data.thresh = 0,
                        statistic = "mean", percentile = NA, start.date = NA) {

    ## extract variables of interest
    vars <- names(mydata)

    mydata <- checkPrep(mydata, vars, type = "default", remove.calm = FALSE)

    ## time zone of data
    TZ <- attr(mydata$date, "tzone")
    if (is.null(TZ)) TZ <- "GMT" ## as it is on Windows for BST

    if (!is.na(percentile)) {
        percentile <- percentile / 100
        if (percentile < 0 | percentile > 100) stop("Percentile range outside 0-100")
    }
    if (data.thresh < 0 | data.thresh > 100) stop("Data capture range outside 0-100")

    if (statistic == "mean") form <- "mean(x, na.rm = TRUE)"
    if (statistic == "max") form <- "suppressWarnings(newMax(x))"
    if (statistic == "min") form <- "suppressWarnings(min(x, na.rm = TRUE))"
    if (statistic == "median") form <- "median(x, na.rm = TRUE)"
    if (statistic == "sum") form <- "sum(x, na.rm = TRUE)"
    if (statistic == "sd") form <- "sd(x, na.rm = TRUE)"
    if (statistic == "frequency") form <- "length(na.omit(x))"
    if (statistic == "percentile") form <- "quantile(x, probs = percentile, na.rm = TRUE)"

    ## max retruns -Inf if all are missing (never understood this!) therefore choose to
    ## return NA if all are missing
    newMax <- function(x) {if (all(is.na(x))) return(NA) else max(x, na.rm = TRUE)}

    calc.mean <- function(mydata, start.date) { ## function to calculate means

        ## pad out missing data
        mydata <- date.pad(mydata)

        ## time diff in seconds of orginal data
        timeDiff <-  as.numeric(strsplit(find.time.interval(mydata$date), " ")[[1]][1])

        ## time diff of new interval
        by2 <- strsplit(avg.time, " ", fixed = TRUE)[[1]]

        seconds <- 1
        if (length(by2) > 1) seconds <- as.numeric(by2[1])
        units <- by2[length(by2)]


        if (units == "sec") int <- 1
        if (units == "min") int <- 60
        if (units == "hour") int <- 3600
        if (units == "day") int <- 3600 * 24
        if (units == "week") int <- 3600 * 24 * 7
        if (units == "month") int <- 3600 * 24 * 30 ## approx
        if (units == "month") int <- 3600 * 24 * 30 ## approx
        if (units == "quarter") int <- 3600 * 24 * 30 * 3 ## approx
        if (units == "year") int <- 3600 * 8760 ## approx

        seconds <- seconds * int ## interval in seconds

        ## check to see if we need to expand data rather than aggregate it
        ## i.e. chosen time interval less than that of data
        if (seconds < timeDiff) {

            ## orginal dates
            theDates <- mydata$date

            ## need to add a date to the end when expanding times
            interval <- find.time.interval(mydata$date)
            allDates <- seq(min(mydata$date), max(mydata$date), by = interval)
            allDates <- c(allDates, max(allDates) + timeDiff)

            ## all data with new time interval
            allData <- data.frame(date = seq(min(allDates), max(allDates), avg.time))

            ## merge with orginal data, which leaves gaps to fill
            mydata <- merge(mydata, allData, by = "date", all = TRUE)

            ## number of additional lines to fill
            inflateFac <-  timeDiff / seconds
            if (timeDiff %% seconds != 0) stop("Non-regular time expansion selected.")

            ## ids of orginal dates in new dates
            ids <- which(mydata$date %in% theDates)

            date <- mydata$date
            mydata <-subset(mydata, select = -date)

            for (i in 1:(inflateFac - 1)) {
                mydata[ids + i, ] <-  mydata[ids, ]
            }

            mydata <- cbind(date, mydata)
            mydata <- mydata[1:nrow(mydata) - 1, ] ## don't need last row
            return(mydata)

        }

         ## start from a particular time, if given
        if (!is.na(start.date)) {

            firstLine <- data.frame(date = as.POSIXct(start.date))

            mydata <- rbind.fill(firstLine, mydata)
            mydata <- date.pad(mydata)
            ## for cutting data must ensure it is in GMT because combining
            ## data frames when system is not GMT puts it in local time!...
            ## and then cut makes a string/factor levels with tz lost...

            mydata$date <- as.POSIXct(format(mydata$date), tz = TZ)

        }

        if (all(c("wd", "wd") %in% names(mydata))) {
            if (is.numeric(mydata$wd)) {
                mydata$u <- mydata$ws * sin(2 * pi * mydata$wd / 360)
                mydata$v <- mydata$ws * cos(2 * pi * mydata$wd / 360)
            }
        }


        ## cut into sections dependent on period
        mydata$cuts <- cut(mydata$date, avg.time)


        if (data.thresh > 0) {

            ## two methods of calculating stats, one that takes account of data capture (slow), the
            ## other not (faster)
            newMethod <- function(x, data.thresh, na.rm) {
                ## calculate mean only if above data capture threshold
                if (length(na.omit(x)) >= round(length(x) * data.thresh / 100)) {
                    res <- eval(parse(text = form))
                } else {
                    res <- NA
                }
                res
            }

            dailymet <- aggregate(mydata[ , sapply(mydata, class) %in% c("numeric", "integer"),
                                         drop = FALSE], list(date = mydata$cuts), newMethod,
                                  na.rm = TRUE,  data.thresh = data.thresh)
        }

        if (data.thresh == 0 & statistic != "mean") {

            dailymet <- aggregate(mydata[ , sapply(mydata, class) %in% c("numeric", "integer"),
                                         drop = FALSE], list(date = mydata$cuts),
                                  function (x) eval(parse(text = form)))

        }

        if (data.thresh == 0 & statistic == "mean") {

            dailymet <- aggregate(mydata[ , sapply(mydata, class) %in% c("numeric", "integer"),
                                         drop = FALSE], list(date = mydata$cuts), mean, na.rm = TRUE)

        }
        ## return same date class as went in...
        if (class(mydata$date)[1] == "Date") {
            dailymet$date <- as.Date(dailymet$date)


        } else {
            ## return the same TZ that we started with
            dailymet$date <- as.POSIXct(format(dailymet$date), tz = TZ)

        }

        if ("wd" %in% names(mydata)) {
            if (is.numeric(mydata$wd)) {
                ## mean wd
                dailymet <- within(dailymet, wd <- as.vector(atan2(u, v) * 360 / 2 / pi))

                ## correct for negative wind directions
                ids <- which(dailymet$wd < 0)  ## ids where wd < 0
                dailymet$wd[ids] <- dailymet$wd[ids] + 360

                dailymet <- subset(dailymet, select = c(-u, -v))
            }
        }

        if ("site" %in% names(mydata)) dailymet$site <- mydata$site[1]

        dailymet

    }

    ## split if several sites
    if ("site" %in% names(mydata)) { ## split by site
        mydata$site <- factor(mydata$site)
        mydata <- ddply(mydata, .(site), calc.mean, start.date)
        mydata
    } else {
        mydata <- calc.mean(mydata, start.date)
        mydata
    }
}
