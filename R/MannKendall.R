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
                        period = "month",
                        statistic = "mean",
                        percentile = NA,
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

    if (!period %in% c("year", "month")) stop ("Period can only be 'month' or 'year'.")

    ## data checks
    mydata <- checkPrep(mydata, vars, type)

    ## cutData depending on type
    mydata <- cutData(mydata, type)

    ## for overall data and graph plotting
    start.year <- startYear(mydata$date)
    end.year <-  endYear(mydata$date)
    start.month <- startMonth(mydata$date)
    end.month <-  endMonth(mydata$date)

    ## calculate means
    mydata <- ddply(mydata, type, timeAverage, period = period, statistic = statistic,
                    percentile = percentile, data.thresh = data.thresh)      

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
        

        if (period == "month") {
            
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

            all.results <- data.frame(date = mydata$date, conc = deseas)                      
            results <- na.omit(all.results)
            

        } else {

            ## assume annual            
            all.results <- data.frame(date = as.Date(mydata$date), conc = mydata[ , pollutant])
            results <- na.omit(all.results)
        }

        ## now calculate trend, uncertainties etc ###############################################

        MKresults <- MKstats(results$date, results$conc, alpha, simulate, autocor)
        
        ## make sure missing data are put back in for plotting
        results <- suppressWarnings(merge(all.results, MKresults, by = "date", all = TRUE))
        results
    }

    split.data <- ddply(mydata, type,  process.cond)

    ## proper names of labelling ##############################################################################
    pol.name <- sapply(levels(split.data[ , type[1]]), function(x) quickText(x, auto.text))
    strip <- strip.custom(factor.levels = pol.name)

    if (length(type) == 1 ) {
        
        strip.left <- FALSE
        
    } else { ## two conditioning variables        
        
        pol.name <- sapply(levels(split.data[ , type[2]]), function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels = pol.name)       
    }
    ## ########################################################################################################
    
    skip <- FALSE
    layout <- NULL

    if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
    
    if (length(type) == 1 & type[1] == "wd") {
        skip <-  c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
        layout = if (type == "wd") c(3, 3) else NULL
    }
    

#### calculate slopes etc ###############################################################################

    split.data <- transform(split.data, slope = 365 * b, intercept = a,
                            intercept.lower = lower.a, intercept.upper = upper.a,
                            lower = 365 * upper.b, upper = 365 * lower.b)

    ## aggregated results

    res2 <- ddply(split.data, c(type, "p.stars"), numcolwise(mean), na.rm = TRUE)

    ## calculate percentage changes in slope and uncertainties
    ## need start and end dates (in days) to work out concentrations at those points
    ## percentage change defind as 100.(C.end/C.start -1) / duration
    

    start <- ddply(split.data, type, function (x) head(x, 1))
    end <- ddply(split.data, type, function (x) tail(x, 1))
    percent.change <- merge(start, end, by = type, suffixes = c(".start", ".end"))
   
    percent.change <- transform(percent.change, slope.percent = 100 * 365 *
                                ((slope.start * as.numeric(date.end) / 365 + intercept.start) /
                                 (slope.start * as.numeric(date.start) / 365 + intercept.start) - 1) /
                                (as.numeric(date.end) - as.numeric(date.start)))
   
    percent.change <- transform(percent.change, lower.percent = slope.percent / slope.start * lower.start,
                                upper.percent = slope.percent / slope.start * upper.start)

    percent.change <- percent.change[ ,  c(type, "slope.percent", "lower.percent", "upper.percent")]

    
    split.data <- merge(split.data, percent.change, by = type)

    res2 <- merge(res2, percent.change, by = type)
########################################################################################################

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("conc ~ date| ", temp, sep = ""))
  
    plt <- xyplot(myform, data = split.data,
                  ylab = quickText(ylab, auto.text),
                  main = quickText(main, auto.text),
                  xlab = quickText(xlab, auto.text),
                  par.strip.text = list(cex = 0.8),
                  as.table = TRUE,
                  layout = layout,
                  skip = skip,
                  strip = strip,
                  strip.left = strip.left,
                  scales = list(x = list(at = dateBreaks(split.data$date, date.breaks)$major, format =
                                dateBreaks(split.data$date)$format)),...,

                  panel = function(x, y, subscripts,...){
                      ## year shading
                      panel.shade(split.data, start.year, end.year, ylim = current.panel.limits()$ylim)
                      panel.grid(-1, 0)

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
                      
                      panel.text(min(split.data$date), 0.95 * current.panel.limits()$ylim[2],
                                 paste(round(sub.dat[1, slope], dec.place), " ", "[",
                                       round(sub.dat[1, lower], dec.place), ", ",
                                       round(sub.dat[1, upper], dec.place), "] ",
                                       units, "/", xlab, " ", sub.dat[1, "p.stars"], sep = ""),
                                 cex = 0.7, pos = 4, col = "forestgreen")                     
                  }
                  )
    
#################
                                        #output
#################
    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- list(main.data = split.data, res2 = res2, subsets = c("main.data", "res2"))
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)  

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
