

## Function to flexibly calculate different averagring times for data frames with
## option to also set data.thresh threshold.
## Note that "period" uses those defined in cut.POSIXct e.g. "days", "5 days", and
## is therefore extremely flexible.
## The function will also work with mutiple sites.
## start.date helps to get data frame into nice time sequence
## NOTE - will only return numeric data apart from site name

timeAverage <- function(mydata, period = "day", data.thresh = 0,
                        statistic = "mean", percentile = NA, start.date = NA) {
    

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

        ## start from a particular time, if given
        if (!is.na(start.date)) {
            firstLine <- data.frame(date = as.POSIXct(start.date, "GMT"))
            mydata <- rbind.fill(firstLine, mydata)
        }

        if ("wd" %in% names(mydata)) {
            if (is.numeric(mydata$wd)) {
                mydata$u <- sin(2 * pi * mydata$wd / 360)
                mydata$v <- cos(2 * pi * mydata$wd / 360)
            }
        }

        ## cut into sections dependent on period
        mydata$cuts <- cut(mydata$date, period)
        
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

            dailymet$date <- as.POSIXct(dailymet$date, "GMT")
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
