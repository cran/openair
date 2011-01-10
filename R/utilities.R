## TODO: Add comment
                                        #
## Author: David Carslaw
## useful utility functions
## with some updates and modification by Karl Ropkins
###############################################################################

## make month and weekday names and abbs using local conventions
## This must be run locally 

## make months locally
make.month.names <- function(){
    month.names <- format(ISOdate(2000, 1:12, 1), "%B")
}
make.month.abbs <- function(){
    month.abbs <- format(ISOdate(2000, 1:12, 1), "%b")
}

## make weekdays locally then, order Monday to Sunday for time.variation
make.weekday.names <- function(){
    weekday.names <- ISOdate(2000, 1, 1:14)
    weekday.names <- unique(format(weekday.names, "%A"))[order(unique(as.numeric(format(weekday.names, "%w"))))][c(2:7, 1)]
}
make.weekday.abbs <- function(){
    weekday.abbs <- ISOdate(2000, 1, 1:14)
    weekday.abbs <- unique(format(weekday.abbs, "%a"))[order(unique(as.numeric(format(weekday.abbs, "%w"))))][c(2:7, 1)]
}

weekday.names <- make.weekday.names()
weekday.abbs <- make.weekday.abbs()
month.names <- make.month.names()
month.abbs <- make.month.abbs()

startYear <- function(dat) as.numeric(format(min(dat[order(dat)]), "%Y"))
endYear <- function(dat) as.numeric(format(max(dat[order(dat)]), "%Y"))
startMonth <- function(dat) as.numeric(format(min(dat[order(dat)]), "%m"))
endMonth <- function(dat) as.numeric(format(max(dat[order(dat)]), "%m"))

###############################################################################

## function to find averaging period of data, returns "xx sec"
## for use in filling in gaps in time series data
## it finds the table of values of time gaps and picks the biggest
## can't think of better way unless user specifies what the time interval is meant to be

find.time.interval <- function(dates) {
    ## assumes date is ordered before we get here

    ## could have several sites, dates may be unordered
    ## find the most common time gap in all the data
    dates <- unique(dates)  ## make sure they are unique
 
    id <- which.max(table(diff(as.numeric(dates[order(dates)]))))
    seconds <- as.numeric(names(id))

    if ("POSIXt" %in% class(dates)) seconds <- paste(seconds, "sec")

    if (class(dates)[1] == "Date") {
        seconds <- 3600 * 24
        seconds <- paste(seconds, "sec")
    }

    seconds
}

###############################################################################

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
            interval <- find.time.interval(mydata$date)
        }

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
#############################################################################################


rollingMean <- function(mydata, pollutant = "o3", hours = 8, new.name = "rolling",
                         data.capture = 75){
    ## function to calculate rolling means
    ## as fast as rollapply (zoo) but can handle wide "windows" e.g. annual means

    if (missing(new.name)) new.name <- paste("rolling", hours, pollutant, sep = "")

    calc.rolling <- function(mydata, pollutant, hours, new.name, data.capture) {

        ## pad missing hours
        mydata <- date.pad(mydata)

        roll <- function(x, i, hours, new.name, data.capture) {
            dat <- x[i:(i + hours - 1)]

            if (length(na.omit(dat)) >= round(hours * data.capture / 100)) {
                res <- mean(dat, na.rm = TRUE)
            } else {
                res <- NA
            }
            res
        }

        res <- sapply(1:(nrow(mydata) - hours + 1), function(i) roll(mydata[ , pollutant], i,
                                                                     hours, new.name, data.capture))

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
                                                          new.name, data.capture))

        mydata <- do.call(rbind, mydata)
        mydata
    } else {
        mydata <- calc.rolling(mydata, pollutant, hours, new.name, data.capture)
        mydata
    }
}





convert.date <- function(mydata, format = "%d/%m/%Y %H:%M") {
    mydata$date <- as.POSIXct(strptime(mydata$date, format = format), "GMT")
    mydata
}


#############################################################################################
## for processing model output. Given a data frame with two variables (and date + maybe others)
## this function will reshape the data suitable for use in many openair functions

prepareModel <- function(mydata, measured = "obs", modelled = "mod", pollutant = "nox") {
    ##library(reshape)
    if (missing(mydata)) stop("No data frame was supplied!")

    ## make sure there is not a field called site
    if ("site" %in% names(mydata))  mydata <- subset(mydata, select = -site)
    mydata <- melt(mydata, measure.vars = c(measured, modelled))

    ## change name to "site"
    names(mydata)[names(mydata) == "variable"] <- "site"
    names(mydata)[names(mydata) == "value"] <- pollutant
    mydata
}
#############################################################################################
## splits data frame into date chunks. Allows users to supply simple dates and labels
## useful for type = "site", interventions

splitByDate <- function(mydata, dates = "1/1/2003", labels = c("before", "after")) {
    ## if date in format dd/mm/yyyy hh:mm (basic check)
    if (missing(mydata)) stop("No data frame was supplied!")

    if ("site" %in% names(mydata)) {
        if (length(levels(factor(mydata$site))) > 1 & any(duplicated(mydata$date))) {
            stop("More than one site detected - can only deal with a single site at the moment!")
        }
    }

    mydata <- checkPrep(mydata, names(mydata), "default")
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


    mydata$site <- cut(as.numeric(mydata$date), breaks = c(0, as.numeric(dates),
                                                max(mydata$date)), labels = labels,
                       ordered_result = TRUE)
    mydata
}
#############################################################################################

## function to make it easy to use d/m/y format for subsetting by date
selectByDate <- function(mydata, start = "1/1/2008", end = "31/12/2008", year = 2008,
                           month = 1, hour = 1, day = "weekday") {

    weekday.names <- make.weekday.names()

    if (!missing(start) & !missing(end)) {
        start <- as.POSIXct(strptime(start, format = "%d/%m/%Y"), "GMT")
        end <- as.POSIXct(strptime(end, format = "%d/%m/%Y"), "GMT") + (23 * 3600)
        mydata <- subset(mydata, date >= start & date <= end)
    }

    if (!missing(year)) {
        mydata <- mydata[as.numeric(format(mydata$date, "%Y")) %in% year, ]

    }

    if (!missing(month)) {
        if (is.numeric(month)) {

            mydata <- mydata[as.numeric(format(mydata$date, "%m")) %in% month, ]
        } else {
            mydata <- subset(mydata, substr(tolower(format(date, "%B")), 1, 3) %in%
                             substr(tolower(month), 1, 3))
        }
    }

    if (!missing(hour)) {
        mydata <- mydata[as.numeric(format(mydata$date, "%H")) %in% hour, ]

    }

    if (!missing(day)) {
        days <- day
        if (day == "weekday") days <- weekday.names[1:5]
        if (day == "weekend") days <- weekday.names[6:7]
        mydata <- subset(mydata, substr(tolower(format(date, "%A")), 1, 3) %in%
                         substr(tolower(days), 1, 3))
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
             lwd = plot.line$lwd, lty = plot.line$lty,col, col.line = plot.line$col,
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
            print(sam.size)
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

                mod <- gam(y ~ s(x),  data = new.data)

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
