## TODO: Add comment
                                        #
## Author: David Carslaw
## useful utility functions
## with some updates and modification by Karl Ropkins
###############################################################################

startYear <- function(dat) as.numeric(format(min(dat[order(dat)]), "%Y"))
endYear <- function(dat) as.numeric(format(max(dat[order(dat)]), "%Y"))
startMonth <- function(dat) as.numeric(format(min(dat[order(dat)]), "%m"))
endMonth <- function(dat) as.numeric(format(max(dat[order(dat)]), "%m"))

## these are pre-defined type that need a field "date"; used by cutData
dateTypes <- c("year", "hour", "month", "season", "weekday", "weekend", "monthyear",
                   "gmtbst", "bstgmt", "daylight")

###############################################################################

## function to find averaging period of data, returns "xx sec"
## for use in filling in gaps in time series data
## it finds the table of values of time gaps and picks the biggest
## can't think of better way unless user specifies what the time interval is meant to be

find.time.interval <- function(dates) {

    ## could have several sites, dates may be unordered
    ## find the most common time gap in all the data
    dates <- unique(dates)  ## make sure they are unique

    len <- length(dates)

    ## sample only 100 rather than everything
    len <- min(c(100, len))

    id <- which.max(table(diff(as.numeric(dates[order(dates[1 : len])]))))
    seconds <- as.numeric(names(id))

    if ("POSIXt" %in% class(dates)) seconds <- paste(seconds, "sec")

    if (class(dates)[1] == "Date") {
        seconds <- 3600 * 24
        seconds <- paste(seconds, "sec")
    }

    seconds
}

###############################################################################
## when interval is known
date.pad2 <- function(mydata, type = "default", interval = "month") {

    date.pad.site <- function(mydata, type = type, interval = interval) {
        ## function to fill missing data gaps
        ## assume no missing data to begin with
        if (type == "site" ) site <- mydata$site[1]

        ## pad out missing data for better looking plot
        start.date <- min(mydata$date, na.rm = TRUE)
        end.date <- max(mydata$date, na.rm = TRUE)

        all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
        mydata <- merge(mydata, all.dates, all = TRUE)


        if (type == "site") mydata$site <- site
        mydata
    }

    if (type == "site") {
        mydata <- split(mydata, mydata$site)
        mydata <- lapply(mydata, date.pad.site, type, interval)
        mydata <- do.call(rbind, mydata)
    } else {
        mydata <- date.pad.site(mydata, type, interval
                                )
    }
    mydata
}
#############################################################################################
## Function to pad out missing time data, optionally dealing with conditioning variable "site"
date.pad <- function(mydata, type = "default") {

    date.pad.site <- function(mydata) {
        ## function to fill missing data gaps
        ## assume no missing data to begin with
        if (type == "site" ) site <- mydata$site[1]

        ## pad out missing data for better looking plot
        start.date <- min(mydata$date, na.rm = TRUE)
        end.date <- max(mydata$date, na.rm = TRUE)

        ## find time interval of data
        if (class(mydata$date)[1] == "Date") {
            interval <- "day"
        } else {
            interval <- openair:::find.time.interval(mydata$date)
        }

        ## only pad if there are missing data
        if (length(unique((diff(mydata$date)))) != 1L) {

            all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
            mydata <- merge(mydata, all.dates, all = TRUE)

        }
        if (type == "site") mydata$site <- site
        mydata
    }

    if (type == "site") {
        mydata <- split(mydata, mydata$site)
        mydata <- lapply(mydata, date.pad.site)
        mydata <- do.call(rbind, mydata)
    } else {
        mydata <- date.pad.site(mydata)
    }
    mydata
}
#############################################################################################

## Function to pad out missing time data, optionally dealing with conditioning variable "site"
## version where interval is given
date.pad2 <- function(mydata, type = "default", interval = "month") {

    date.pad.site <- function(mydata) {
        ## function to fill missing data gaps
        ## assume no missing data to begin with
        if (type == "site" ) site <- mydata$site[1]

        ## pad out missing data for better looking plot
        start.date <- min(mydata$date, na.rm = TRUE)
        end.date <- max(mydata$date, na.rm = TRUE)

        all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
        mydata <- merge(mydata, all.dates, all = TRUE)


        if (type == "site") mydata$site <- site
        mydata
    }

    if (type == "site") {
        mydata <- split(mydata, mydata$site)
        mydata <- lapply(mydata, date.pad.site)
        mydata <- do.call(rbind, mydata)
    } else {
        mydata <- date.pad.site(mydata)
    }
    mydata
}



##' Calculate rollingMean values
##'
##' Calculate rollingMean values taking account of data capture thresholds
##'
##' This is a utility function mostly designed to calculate rolling mean
##' statistics relevent to some pollutant limits e.g. 8 hour rolling means for
##' ozone and 24 hour rollingMeans for PM10.
##'
##' @param mydata A data frame containing a \code{date} field.
##' @param pollutant The name of a pollutant e.g. \code{pollutant = "o3"}.
##' @param hours The averaging period to use e.g. \code{hours = 8} will
##'   generate 8-hour rollingMean values.
##' @param new.name The name given to the new rollingMean variable. If not
##'   supplied it will create a name based on the name of the pollutant and the
##'   averaging period used.
##' @param data.thresh The data capture threshold in %. No values are
##'   calculated if data capture over the period of interest is less than this
##'   value. For example, with \code{hours = 8} and \code{data.thresh = 75} at
##'   least 6 hours are required to calculate the mean, else \code{NA} is
##'   returned.
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## rolling 8-hour mean for ozone
##' mydata <- rollingMean(mydata, pollutant = "o3", hours = 8, new.name =
##' "rollingo3", data.thresh = 75)
##'
##'
rollingMean <- function(mydata, pollutant = "o3", hours = 8, new.name = "rolling",
                         data.thresh = 75){
    ## function to calculate rolling means
    ## as fast as rollapply (zoo) but can handle wide "windows" e.g. annual means

    if (missing(new.name)) new.name <- paste("rolling", hours, pollutant, sep = "")

    calc.rolling <- function(mydata, pollutant, hours, new.name, data.thresh) {

        ## pad missing hours
        mydata <- date.pad(mydata)

        roll <- function(x, i, hours, new.name, data.thresh) {
            dat <- x[i:(i + hours - 1)]

            if (length(na.omit(dat)) >= round(hours * data.thresh / 100)) {
                res <- mean(dat, na.rm = TRUE)
            } else {
                res <- NA
            }
            res
        }

        res <- sapply(1:(nrow(mydata) - hours + 1), function(i) roll(mydata[ , pollutant], i,
                                                                     hours, new.name, data.thresh))

        res <- c(rep(NA, (hours - 1)), res) ## pad missing data
        mydata <- cbind(mydata, res)
        names(mydata)[ncol(mydata)] <- new.name
        mydata
    }

    ## split if several sites
    if ("site" %in% names(mydata)) { ## split by site
        mydata$site <- factor(mydata$site)
        mydata <- split(mydata, mydata$site)
        mydata <- lapply(mydata, function(x) calc.rolling(x, pollutant, hours,
                                                          new.name, data.thresh))

        mydata <- do.call(rbind, mydata)
        mydata
    } else {
        mydata <- calc.rolling(mydata, pollutant, hours, new.name, data.thresh)
        mydata
    }
}





convert.date <- function(mydata, format = "%d/%m/%Y %H:%M") {
    mydata$date <- as.POSIXct(strptime(mydata$date, format = format), "GMT")
    mydata
}



#############################################################################################
## splits data frame into date chunks. Allows users to supply simple dates and labels
## useful for type = "site", interventions



##' Divide up a data frame by time
##'
##' Utility function to prepare input data for use in openair functions
##'
##' This function partitions a data frame up into different time segments. It
##' produces a new column called controlled by \code{name} that can be used in many
##' \code{openair} functions. Note that there must be one more label than there
##' are dates. See examples below and in full \code{openair} documentation.
##'
##' @param mydata A data frame containing a \code{date} field in hourly or high
##'   resolution format.
##' @param dates A date or dates to split data by.
##' @param labels Labels for each time partition.
##' @param name The name to give the new column to identify the periods split
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## split data up into "before" and "after"
##' mydata <- splitByDate(mydata, dates = "1/04/2000",
##' labels = c("before", "after"))
##'
##' ## split data into 3 partitions:
##' mydata <- splitByDate(mydata, dates = c("1/1/2000", "1/3/2003"),
##' labels = c("before", "during", "after"))
##'
##'
splitByDate <- function(mydata, dates = "1/1/2003", labels = c("before", "after"), name = "split.by") {
    ## if date in format dd/mm/yyyy hh:mm (basic check)
    if (missing(mydata)) stop("No data frame was supplied!")

    mydata <- checkPrep(mydata, names(mydata), "default", remove.calm = FALSE)
    ## check there are sufficent labels for number of dates
    if (length(dates) != length(labels) - 1) {
        stop("There is a mis-match between dates and labels. There should be
one more label than date")
    }

    if (length(grep("/", as.character(dates))) > 0) {

        if (class(mydata$date)[1] == "Date") {

            dates <- as.Date(as.POSIXct(strptime(dates, "%d/%m/%Y"), "GMT"))

        } else {

            dates <- as.POSIXct(strptime(dates, "%d/%m/%Y"), "GMT")

        }

    } else { ## asume format yyyy-mm-dd

        if (class(mydata$date)[1] == "Date") {

            dates <- as.Date(dates)

        } else {

            dates <- as.POSIXct(dates, "GMT")

        }


    }


    mydata[ , name] <- cut(as.numeric(mydata$date), breaks = c(0, as.numeric(dates),
                                                max(mydata$date)), labels = labels,
                       ordered_result = TRUE)
    mydata
}
#############################################################################################

## function to make it easy to use d/m/y format for subsetting by date


##' Subset a data frame based on date
##'
##' Utility function to make it easier to select periods from a data frame
##' before sending to a function
##'
##' This function makes it much easier to select periods of interest from a
##' data frame based on dates in a British format. Selecting date/times in R
##' format can be intimidating for new users. This function can be used to
##' select quite complex dates simply - see examples below.
##'
##' Dates are assumed to be inclusive, so \code{start = "1/1/1999"}
##' means that times are selected from hour zero. Similarly, \code{end
##' = "31/12/1999"} will include all hours of the 31st
##' December. \code{start} and \code{end} can also be in standard R
##' format as a string i.e. "YYYY-mm-dd", so \code{start =
##' "1999-01-01"} is fine.
##'
##' All options are applied in turn making it possible to select quite complex
##' dates
##'
##' @param mydata A data frame containing a \code{date} field in hourly or high
##'   resolution format.
##' @param start A start date string in the form d/m/yyyy
##' e.g. "1/2/1999" or in 'R' format i.e. "YYYY-mm-dd", "1999-02-01"
##' @param end See \code{start} for format.
##' @param year A year or years to select e.g. \code{year = 1998:2004} to
##'   select 1998-2004 inclusive or \code{year = c(1998, 2004)} to select 1998
##'   and 2004.
##' @param month A month or months to select. Can either be numeric e.g.
##'   \code{month = 1:6} to select months 1-6 (January to June), or by name
##'   e.g. \code{month = c("January", "December")}. Names can be abbreviated to
##'   3 letters and be in lower or upper case.
##' @param day A day name or or days to select. For example \code{day = c("Monday",
##'   "Wednesday")}. Names can be abbreviated to 3 letters and be in lower or
##'   upper case. Also accepts "weekday" (Monday - Friday) and "weekend" for
##'   convenience.
##' @param hour An hour or hours to select from 0-23 e.g. \code{hour = 0:12} to
##'   select hours 0 to 12 inclusive.
##' @export
##' @author David Carslaw
##' @keywords methods
##' @examples
##'
##' ## select all of 1999
##' data.1999 <- selectByDate(mydata, start = "1/1/1999", end = "31/12/1999")
##' head(data.1999)
##' tail(data.1999)
##'
##' # or...
##' data.1999 <- selectByDate(mydata, start = "1999-01-01", end = "1999-12-31")
##'
##' # easier way
##' data.1999 <- selectByDate(mydata, year = 1999)
##'
##'
##' # more complex use: select weekdays between the hours of 7 am to 7 pm
##' sub.data <- selectByDate(mydata, day = "weekday", hour = 7:19)
##'
##' # select weekends between the hours of 7 am to 7 pm in winter (Dec, Jan, Feb)
##' sub.data <- selectByDate(mydata, day = "weekend", hour = 7:19, month =
##' c("dec", "jan", "feb"))
##'
selectByDate <- function (mydata, start = "1/1/2008", end = "31/12/2008", year = 2008,
    month = 1, day = "weekday", hour = 1)

{
     ## extract variables of interest
    vars <- names(mydata)

    weekday.names <- format(ISOdate(2000, 1, 3:9), "%A")


    if (!missing(start) & !missing(end)) {

        if (length(grep("/", start)) > 0 & length(grep("/", end)) > 0) {
            ## assume UK date format
            start <- as.Date(start, "%d/%m/%Y")
            end <- as.Date(end, "%d/%m/%Y")
        }

        if (length(grep("-", start)) > 0 & length(grep("-", end)) > 0) {
            ## assume R date format
            start <- as.Date(start)
            end <- as.Date(end)
        }

        mydata <- subset(mydata, as.Date(date) >= start & as.Date(date) <= end)

    }
    if (!missing(year)) {
        mydata <- mydata[as.numeric(format(mydata$date, "%Y")) %in%  year, ]
    }
    if (!missing(month)) {
        if (is.numeric(month)) {
            mydata <- mydata[as.numeric(format(mydata$date, "%m")) %in% month, ]
        }
        else {
            mydata <- subset(mydata, substr(tolower(format(date,
                "%B")), 1, 3) %in% substr(tolower(month), 1, 3))
        }
    }
    if (!missing(hour)) {
        mydata <- mydata[as.numeric(format(mydata$date, "%H")) %in% hour, ]
    }
    if (!missing(day)) {
        days <- day
        if (day[1] == "weekday")
            days <- weekday.names[1:5]
        if (day[1] == "weekend")
            days <- weekday.names[6:7]
        mydata <- subset(mydata, substr(tolower(format(date,
            "%A")), 1, 3) %in% substr(tolower(days), 1, 3))
    }
    mydata
}



#############################################################################################

useOuterStrips <-function (x, strip = strip.default, strip.left = strip.custom(horizontal = FALSE),
                           strip.lines = 1, strip.left.lines = strip.lines)
                                        # direct copy from latticeExtra
{
    dimx <- dim(x)
    stopifnot(inherits(x, "trellis"))
    stopifnot(length(dimx) == 2)
    opar <- if (is.null(x$par.settings))
        list()
    else x$par.settings
    par.settings <- modifyList(opar, list(layout.heights = if (x$as.table) list(strip = c(strip.lines,
                                                                                rep(0, dimx[2] - 1))) else list(strip = c(rep(0, dimx[2] -
                                                                                                                1), 1)), layout.widths = list(strip.left = c(strip.left.lines,
                                                                                                                                              rep(0, dimx[1] - 1)))))
    if (is.character(strip))
        strip <- get(strip)
    if (is.logical(strip) && strip)
        strip <- strip.default
    new.strip <- if (is.function(strip)) {
        function(which.given, which.panel, var.name, ...) {
            if (which.given == 1)
                strip(which.given = 1, which.panel = which.panel[1],
                      var.name = var.name[1], ...)
        }
    }
    else strip
    if (is.character(strip.left))
        strip.left <- get(strip.left)
    if (is.logical(strip.left) && strip.left)
        strip.left <- strip.custom(horizontal = FALSE)
    new.strip.left <- if (is.function(strip.left)) {
        function(which.given, which.panel, var.name, ...) {
            if (which.given == 2)
                strip.left(which.given = 1, which.panel = which.panel[2],
                           var.name = var.name[2], ...)
        }
    }
    else strip.left
    update(x, par.settings = par.settings, strip = new.strip,
           strip.left = new.strip.left, par.strip.text = list(lines = 0.5),
           layout = dimx)
}


## from Deepayan Sarkar
panel.smooth.spline <-
    function(x, y,
             w = NULL, df, spar = NULL, cv = FALSE,
             lwd = lwd, lty = plot.line$lty,col, col.line = plot.line$col,
             type, horizontal = FALSE, all.knots = TRUE,... )
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1)
        return()
    if (!missing(col)) {
        if (missing(col.line))
            col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal) {
        spline <-
            smooth.spline(y[ok], x[ok],
                          w=w, df=df, spar = spar, cv = cv)
        panel.lines(x = spline$y, y = spline$x, col = col.line,
                    lty = lty, lwd = lwd, ...)
    }
    else {
        spline <-
            smooth.spline(x[ok], y[ok],
                          w=w, df=df, spar = spar, cv = cv)
        panel.lines(x = spline$x, y = spline$y, col = col.line,
                    lty = lty, lwd = lwd, ...)
    }

}

### panel functions for plots based on lattice ####################################################

panel.gam <- function (x, y, form = y ~ x, method = "loess", ..., simulate = FALSE, n.sim = 200,
                       autocor = FALSE, se = TRUE,
                       level = 0.95, n = 100, col = plot.line$col, col.se = col,
                       lty = plot.line$lty, lwd = plot.line$lwd, alpha = plot.line$alpha,
                       alpha.se = 0.20, border = NA, subscripts, group.number, group.value,
                       type, col.line, col.symbol, fill, pch, cex, font, fontface,
                       fontfamily)
{

    ## panel function to add a smooth line to a plot
    ## Uses a GAM (mgcv) to fit smooth
    ## Optionally can plot 95% confidence intervals and run bootstrap simulations
    ## to estimate uncertainties. Simple block bootstrap is also available for correlated data

    thedata <- data.frame(x = x, y = y)
    tryCatch({

        if (!simulate) {
            mod <- gam(y ~ s(x), se = TRUE, data = thedata)


            lims <- current.panel.limits()
            xrange <- c(max(min(lims$x), min(x)), min(max(lims$x), max(x)))
            xseq <- seq(xrange[1], xrange[2], length = n)

            pred <- predict(mod, data.frame(x = xseq), se = se)

            if (se) {
                std <- qnorm(level / 2 + 0.5)
                panel.polygon(x = c(xseq, rev(xseq)), y = c(pred$fit -
                                                      std * pred$se, rev(pred$fit + std * pred$se)),
                              col = col.se, alpha = alpha.se, border = border)
                pred <- pred$fit
            }

            panel.lines(xseq, pred, col = col, alpha = alpha, lty = lty, lwd = 2)
        } else { ## simulations required

            sam.size <- length(x)

            lims <- current.panel.limits()
            xrange <- c(max(min(lims$x), min(x)), min(max(lims$x), max(x)))
            xseq <- seq(xrange[1], xrange[2], length = sam.size)

            boot.pred <- matrix(nrow = sam.size, ncol = n.sim)

            print ("Taking bootstrap samples. Please wait...")

            ## set up bootstrap
            block.length <- 1

            if (autocor) block.length <- round(sam.size ^ (1 / 3))
            index <- samp.boot.block(sam.size, n.sim, block.length)

            ## predict first
            mod <- gam(y ~ s(x), data = thedata)

            residuals <- residuals(mod) ## residuals of the model

            pred.input <- predict(mod, thedata)

            for (i in 1:n.sim) {
                ## make new data
                new.data <- data.frame(x = xseq, y = pred.input + residuals[index[, i]])

                mod <- gam(y ~ s(x), data = new.data)

                pred <- predict(mod, new.data)

                boot.pred[, i] <- as.vector(pred)

            }

            ## calculate percentiles
            percentiles <- apply(boot.pred, 1, function(x) quantile(x, probs = c(0.025, 0.975)))

            results <- as.data.frame(cbind(pred = rowMeans(boot.pred),
                                           lower = percentiles[1, ], upper = percentiles[2, ]))

            if (se) {

                panel.polygon(x = c(xseq, rev(xseq)), y = c(results$lower, rev(results$upper)),
                              col = col.se, alpha = alpha.se, border = border)

            }

            panel.lines(xseq, pred.input, col = col, alpha = alpha, lty = lty, lwd = 2)

        }

    }, error = function(x) return)
}





panel.linear <- function (x, y, form = y ~ x, method = "loess", x.nam, y.nam, ..., se = TRUE,
                          level = 0.95, n = 100, col = plot.line$col, col.se = col,
                          lty = plot.line$lty, lwd = plot.line$lwd, alpha = plot.line$alpha,
                          alpha.se = 0.25, border = NA, subscripts, group.number, group.value,
                          type, col.line, col.symbol, fill, pch, cex, font, fontface,
                          fontfamily)
{


    thedata <- data.frame(x = x, y = y)
    tryCatch({mod <- lm(y ~ x, data = thedata)

              lims <- current.panel.limits()
              xrange <- c(max(min(lims$x), min(x)), min(max(lims$x), max(x)))
              xseq <- seq(xrange[1], xrange[2], length = n)

              pred <- predict(mod, data.frame(x = xseq), interval = "confidence")

              if (se) {
                  ## predicts 95% CI by default
                  panel.polygon(x = c(xseq, rev(xseq)), y = c(pred[, 2], rev(pred[, 3])), col = col.se,
                                alpha = alpha.se, border = border)
              }

              pred <- pred[, 1]

              panel.lines(xseq, pred, col = col, alpha = alpha, lty = lty,
                          lwd = lwd)

              x <- current.panel.limits()$xlim[1]

              y <- 0.95 * current.panel.limits()$ylim[2]

              r.sq <- summary(mod)$r.squared
              slope <- coef(mod)[2]
              intercept <- coef(mod)[1]

              panel.text(x, y, quickText(paste(y.nam, "=", format(slope, digits = 2), "[", x.nam, "]", "+",
                                                format(intercept, digits = 2),
                                                " R2=",  format(r.sq, digits = 2),
                                                sep = "")), cex = 0.7, pos = 4)

          }, error = function(x) return)
}

#########################################################################################################



## error in mean from Hmisc

errorInMean <- function (x, mult = qt((1 + conf.int)/2, n - 1), conf.int = 0.95,
                         na.rm = TRUE)
{
    if (na.rm)
        x <- x[!is.na(x)]
    n <- length(x)
    if (n < 2)
        return(c(Mean = mean(x), Lower = NA, Upper = NA))
    xbar <- sum(x)/n
    se <- sqrt(sum((x - xbar)^2)/n/(n - 1))
    c(Mean = xbar, Lower = xbar - mult * se, Upper = xbar + mult *
      se)
}

## bootsrap confidence intervals in the mean from Hmisc
bootMean <- function (x, conf.int = 0.95, B = 1000, na.rm = TRUE, reps = FALSE)
{
    if (na.rm)
        x <- x[!is.na(x)]
    n <- length(x)
    xbar <- mean(x)
    if (n < 2)
        return(c(Mean = xbar, Lower = NA, Upper = NA))
    z <- unlist(lapply(1:B, function(i, x, N)
                       sum(x[.Internal(sample(N, N, TRUE, NULL))]), x = x, N = n)) / n
    quant <- quantile(z, c((1 - conf.int) / 2, (1 + conf.int) / 2))
    names(quant) <- NULL
    res <- c(Mean = xbar, Lower = quant[1], Upper = quant[2])
    if (reps)
        attr(res, "reps") <- z
    res
}

bootMeanDiff <- function (mydata, x = "x", y = "y", conf.int = 0.95, B = 1000, na.rm = TRUE, reps = TRUE)
{

    ## calculates bootstrap mean differences
    ## assumes y - x
    x.name <- x
    y.name <- y
    x <- na.omit(mydata[ , x])
    y <- na.omit(mydata[ , y])
    Mean <- mean(y) - mean(x)

    if (nrow(mydata) < 2) {
        res1 <- data.frame(variable = x.name, Mean = mean(x), Lower = NA, Upper = NA)
        res2 <- data.frame(variable = y.name, Mean = mean(y), Lower = NA, Upper = NA)
        res <- data.frame(variable = paste(y.name, "-", x.name), Mean = Mean, Lower = NA, Upper = NA)

        res <- rbind.fill(res1, res2, res)
        res$variable <- factor(res$variable)
        return(res)

    }

    x <- attr(bootMean(x,  B = B, reps = TRUE), 'reps')
    y <- attr(bootMean(y,  B = B, reps = TRUE), 'reps')
    quant1 <- quantile(x, c((1 - conf.int) / 2, (1 + conf.int) / 2))
    quant2 <- quantile(y, c((1 - conf.int) / 2, (1 + conf.int) / 2))
    quant <- quantile(y - x, c((1 - conf.int) / 2, (1 + conf.int) / 2))
    names(quant1) <- NULL
    names(quant2) <- NULL
    names(quant) <- NULL

    res1 <- data.frame(variable = x.name, Mean = mean(x), Lower = quant1[1], Upper = quant1[2])
    res2 <- data.frame(variable = y.name, Mean = mean(y), Lower = quant2[1], Upper = quant2[2])
    res <- data.frame(variable = paste(y.name, "-", x.name), Mean = Mean, Lower = quant[1], Upper = quant[2])

    res <- rbind.fill(res1, res2, res)
    res$variable <- factor(res$variable)
    res
}

###########################################################################################################

## list update function
## for lattice type object structure and ... handling

## (currently used by)
## (all openair plots that include colorkey controlled by drawOpenKey)

##listUpdate function
#[in development]
listUpdate <- function(a, b, drop.dots = TRUE,
                       subset.a = NULL, subset.b = NULL){
    if(drop.dots){
        a <- a[names(a) != "..."]
        b <- b[names(b) != "..."]
    }
    if(!is.null(subset.a))
        a <- a[names(a) %in% subset.a]
    if(!is.null(subset.b))
        b <- b[names(b) %in% subset.b]
    if(length(names(b) > 0))
        a <- modifyList(a, b)
    a
}

#############################################################################################################

## makeOpenKeyLegend v0.1

##common code for making legend list
##objects for use with drawOpenkey outputs

##uses listUpdate in utilities

makeOpenKeyLegend <- function(key, default.key, fun.name = "function"){
    #handle logicals and lists
    if (is.logical(key)) {
        legend <- if (key) default.key else NULL
    } else if (is.list(key)) {
            legend <- listUpdate(default.key, key)
        } else {
            if(!is.null(key))
                warning(paste("In ", fun.name, "(...):\n unrecognised key not exported/applied\n",
                              " [see ?drawOpenKey for key structure/options]", sep = ""),
                        call. = FALSE)
            legend <- NULL
    }

    #structure like legend for drawOpenKey
    if(!is.null(legend)){
        legend <- list(right = list(fun = drawOpenKey, args = list(key = legend),
                         draw =FALSE))
        if("space" %in% names(legend$right$args$key))
            names(legend)[[1]] <- legend$right$args$key$space
    }
    legend
}
