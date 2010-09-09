## functions to calculate Mann-Kendall and Sen-Theil slopes
## Uncertainty in slopes are calculated using bootstrap methods
## The block bootstrap used should be regarded as an ongoing development
## see http://www-rcf.usc.edu/~rwilcox/
##
## Author: DCC with Mann-Kendall and Sen-Theil functions from
## Rand Wilcox
###############################################################################


MannKendall <- function(mydata,
                        pollutant = "nox",
                        deseason = FALSE,
                        type = "default",
                        period = "monthly",
                        statistic = "mean",
                        percentile = 95,
                        data.thresh = 0,
                        simulate = FALSE,
                        alpha = 0.05,
                        dec.place = 2,
                        ylab = pollutant,
                        xlab = "year",
                        main = "",
                        auto.text = TRUE,
                        autocor = FALSE,
                        slope.percent = FALSE,
                        date.breaks = 7,...)  {

    
    vars <- c("date", pollutant)
    ## if autocor is TRUE, then need simulations
    if (autocor) simulate <- TRUE

    ## data checks
    mydata <- checkPrep(mydata, vars, type)

    ## cutData depending on type
    mydata <- cutData(mydata, type)

    ## sometimes data have long trailing NAs, so start and end at first and last data
    min.idx <- min(which(!is.na(mydata[, pollutant])))
    max.idx <- max(which(!is.na(mydata[, pollutant])))
    mydata <- mydata[min.idx:max.idx, ]

    ## for overall data and graph plotting
    start.year <- startYear(mydata$date)
    end.year <-  endYear(mydata$date)
    start.month <- startMonth(mydata$date)
    end.month <-  endMonth(mydata$date)

    process.cond <- function(mydata) {

        ## sometimes data have long trailing NAs, so start and end at
        ## first and last data
        min.idx <- min(which(!is.na(mydata[, pollutant])))
        max.idx <- max(which(!is.na(mydata[, pollutant])))
        mydata <- mydata[min.idx:max.idx, ]

        ## these subsets may have different dates to overall
        start.year <- startYear(mydata$date)
        end.year <-  endYear(mydata$date)
        start.month <- startMonth(mydata$date)
        end.month <-  endMonth(mydata$date)

        cond <- as.character(unique(na.omit(mydata$cond)))

        if (period == "monthly") {

            mydata <- timeAverage(mydata, period = "month", statistic = statistic,
                                   percentile = percentile,
                                   data.thresh = data.thresh)
            mydata$date <- as.Date(mydata$date)
            
            deseas <- mydata[, pollutant]

            ## can't deseason less than 2 years of data
            if (nrow(mydata) < 24) deseason <- FALSE

            if (deseason) {
                ## interpolate missing data using zoo
                mydata[, pollutant] <- na.approx(mydata[, pollutant])

                myts <- ts(mydata[, pollutant], start = c(start.year, start.month),
                           end = c(end.year, end.month), frequency = 12)
                ## key thing is to allow the seanonal cycle to vary, hence
                ## s.window should not be "periodic"; set quite high to avoid
                ## overly fitted seasonal cycle
                ## robustness also makes sense for sometimes noisy data
                ssd <- stl(myts, s.window = 35, robust = TRUE, s.degree = 0)

                deseas <- ssd$time.series[, "trend"] + ssd$time.series[, "remainder"]

                deseas <- as.vector(deseas)
            }

            all.results <- data.frame(date = mydata$date, conc = deseas, cond = cond)

            if (type == "month" | type == "season")

                
                ## need to do some aggregating for season
                if (type == "season") {
                    ## winter stradles 2 years, need to deal with this
                    all.results <- subset(all.results, select = -cond) ## remove for aggregation
                    all.results$month <- as.numeric(format(all.results$date, "%m"))
                    all.results$year <- as.numeric(format(all.results$date, "%Y"))
                    all.results$year[all.results$month == 12] <-
                        all.results$year[all.results$month == 12] + 1
                    ## remove missing for proper aggregation
                    all.results <- na.omit(all.results)

                    results <- aggregate(all.results, list(all.results$year), mean, na.rm = TRUE)
                    class(results$date) <- "Date"
                    results$cond <- cond
                    all.results <- results
                }
            
            results <- na.omit(all.results)


        } else {

            ## assume annual
            means <- tapply(mydata[, pollutant], format(mydata$date, "%Y"),
                            mean, na.rm = TRUE)
            dates <- unique(as.numeric(names(means)))
            dates <- as.Date(ISOdate(dates, 7, 1)) ## convert to years
            means <- as.vector(means)

            all.results <- data.frame(date = dates, conc = means, cond = cond)
            results <- na.omit(all.results)
        }

        ## now calculate trend, uncertainties etc ###############################################

        MKresults <- MKstats(results$date, results$conc, alpha, simulate, autocor)
        
        ## make sure missing data are put back in for plotting
        results <- suppressWarnings(merge(all.results, MKresults, by = "date", all = TRUE))
        results
    }

    split.data <- split(mydata, mydata$cond)
    split.data <- lapply(split.data, function(x) process.cond(x))
    split.data <- split.data[which(lapply(split.data, nrow) >= 4)] ## need at least 4 points
    split.data <- do.call(rbind, split.data)

    ## define the levels for plotting

    if (type == "wd") layout = c(3, 3)

    strip <- TRUE
    skip <- FALSE
    if (type == "default") strip <- FALSE ## remove strip
    if (type == "wd") skip <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)

    ## plot in date format for nice formatting of plots, trend statistics based on numerical dates (days)
    if (type == "month" | type == "season") split.data <- na.omit(split.data)
    ## there are "missing" data

#### calculate slopes etc ###############################################################################

    split.data <- transform(split.data, slope = 365 * b, intercept = a,
                            intercept.lower = lower.a, intercept.upper = upper.a,
                            lower = 365 * upper.b, upper = 365 * lower.b)

    ## aggregated results
    res2 <- aggregate(subset(split.data, select = c(-date, - cond, - p.stars)),
                      list(variable = split.data$cond, p.stars = split.data$p.stars), mean)

    ## calculate percentage changes in slope and uncertainties
    ## need start and end dates (in days) to work out concentrations at those points
    ## percentage change defind as 100.(C.end/C.start -1) / (Date.end = Date.start)

    start <- aggregate(split.data, list(variable = split.data$cond), function (x) head(x, 1))
    end <- aggregate(split.data, list(variable = split.data$cond), function (x) tail(x, 1))
    percent.change <- merge(start, end, by = "variable", suffixes = c(".start", ".end"))

    percent.change <- transform(percent.change, slope.percent = 100 * 365 *
                                ((slope.start * date.end / 365 + intercept.start) /
                                 (slope.start * date.start / 365 + intercept.start) - 1) /
                                (date.end - date.start))

    percent.change <- transform(percent.change, lower.percent = slope.percent / slope.start * lower.start,
                                upper.percent = slope.percent / slope.start * upper.start)

    percent.change <- subset(percent.change, select = c(variable, slope.percent,
                                             lower.percent, upper.percent))

    
    split.data <- merge(split.data, percent.change, by.x = "cond", by.y = "variable")

    res2 <- merge(res2, percent.change, by = "variable")
########################################################################################################
    
    plt <- xyplot(conc ~ date | cond, data = split.data,
                  ylab = quickText(ylab, auto.text),
                  main = quickText(main, auto.text),
                  xlab = quickText(xlab, auto.text),
                  as.table = TRUE,
                  layout = layout,
                  skip = skip,
                  strip = strip,
                  scales = list(x = list(at = dateBreaks(split.data$date, date.breaks)$major, format =
                                dateBreaks(split.data$date)$format)),...,

                  panel = function(x, y, subscripts,...){
                      ## year shading
                      panel.shade(split.data, start.year, end.year, ylim = current.panel.limits()$ylim)

                      panel.xyplot(x, y, type = "b",...)

                      sub.dat <- na.omit(split.data[subscripts, ])

                      panel.abline(a = sub.dat[1, "intercept"], b = sub.dat[1, "slope"] / 365,
                                   col = "red", lwd = 2)
                      panel.abline(a = sub.dat[1, "intercept.lower"], b = sub.dat[1, "upper"] / 365, lty = 5,
                                   col = "red")
                      panel.abline(a = sub.dat[1, "intercept.upper"], b = sub.dat[1, "lower"] / 365, lty = 5,
                                   col = "red")

                      ## for text on plot - % trend or not?
                      slope <- "slope"
                      lower <- "lower"
                      upper <- "upper"
                      units <- "units"
                      
                      if (slope.percent) {
                          slope <- "slope.percent"
                          lower <- "lower.percent"
                          upper <- "upper.percent"
                          units <- "%"
                      }
                      
                      panel.text(min(split.data$date), max(split.data$conc, na.rm = TRUE),
                                 paste(round(sub.dat[1, slope], dec.place), " ", "[",
                                       round(sub.dat[1, lower], dec.place), ", ",
                                       round(sub.dat[1, upper], dec.place), "] ",
                                       units, "/", xlab, " ", sub.dat[1, "p.stars"], sep = ""),
                                 cex = 0.7, pos = 4, col = "forestgreen")
                      
                  }
                  )
    
    print(plt)
    invisible(list(split.data, res2))
}



panel.shade <- function(split.data, start.year, end.year, ylim) {
    ## provides annual shaded 'bands' on plots to help show years
    
    x1 <- as.POSIXct(seq(ISOdate(start.year, 1, 1),
                         ISOdate(end.year + 1, 1, 1), by = "2 years"), "GMT")
    x2 <- as.POSIXct(seq(ISOdate(start.year + 1, 1, 1),
                         ISOdate(end.year + 2, 1, 1), by = "2 years"), "GMT")
    if (class(split.data$date)[1]  == "Date") {x1 <- as.Date(x1)
                                               x2 <- as.Date(x2)
                                           }

    rng <- range(split.data$conc, na.rm = TRUE) ## range of data
    y1 <- min(split.data$conc, na.rm = TRUE) - 0.1 * abs(rng[2] - rng[1])
    y2 <- max(split.data$conc, na.rm = TRUE) + 0.1 * abs(rng[2] - rng[1])

    ## if user selects specific limits

    if (!missing(ylim)) {
        y1 <- ylim[1] - 0.1 * abs(ylim[2] - ylim[1])
        y2 <- ylim[2] + 0.1 * abs(ylim[2] - ylim[1])
    }

    sapply(seq_along(x1), function(x) lpolygon(c(x1[x], x1[x], x2[x], x2[x]),
                                               c(y1, y2, y2, y1),
                                               col = "grey95", border = "grey95"))
}

MKstats <- function(x, y, alpha, simulate, autocor) {
    ## function to calculate Mann-Kendall stats with different options
    if (simulate) {

        block.length <- 1
        ## block length equal to length ts^(1/3) - need reference
        if (autocor) block.length <- round(length(x) ^ (1 / 3))

        MKtau <- function(z) tau.boot(z)$cor

        boot.res <- tsboot(y, MKtau, R = 1000, l = block.length, sim = "fixed")

        ## approx p value; see ?boot for this (which I think is wrong!)
        p <- 1 - sum(abs(boot.res$t[, 1] - 1) > abs(boot.res$t0[1] - 1)) / (1 + boot.res$R)

    } else {
        ## trend information in days
        MKtau <- tau(as.numeric(x), y, alpha = alpha)
        p <- MKtau$siglevel ## signficance level of trend
    }

    coef <- tsp1reg(as.numeric(x), y)$coef
    uncer <- regci(as.numeric(x), y, alpha = alpha, autocor = autocor)$regci

    if (p >= 0.1) stars <- ""
    if (p < 0.1 & p >= 0.05) stars <- "+"
    if (p < 0.05 & p >= 0.01) stars <- "*"
    if (p < 0.01 & p >= 0.001) stars <- "**"
    if (p < 0.001) stars <- "***"

    ## make a data frame with all the results, wanring is about row name
    results <- suppressWarnings(data.frame(date = x, a = coef[1], b = coef[2],
                                           lower.a = uncer[1, 1],
                                           lower.b = uncer[2, 2], upper.a = uncer[1, 2],
                                           upper.b = uncer[2, 1],  p = p, p.stars = stars))
    results
}
