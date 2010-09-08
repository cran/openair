timePlot <- function(mydata,
                      pollutant = "nox",
                      group = FALSE,
                      stack = FALSE,
                      normalise = FALSE,
                      avg.time = "default",
                      data.thresh = 0,
                      statistic = "mean",
                      percentile = 95,
                      date.pad = FALSE,
                      type = "default",
                      layout = c(1, 1),
                      cols = "brewer1",
                      main = "",
                      ylab = pollutant,
                      lty = 1:length(pollutant),
                      lwd = 1,
                      key = TRUE,
                      strip = TRUE,
                      log = FALSE,
                      smooth = FALSE,
                      ci = TRUE,
                      key.columns = 1,
                      name.pol = pollutant,
                      date.breaks = 7,
                      auto.text = TRUE, ...)   {


    ## basic function to plot single/multiple time series in flexible ways
    ## optionally includes several pre-deifined averaging periods
    ## can deal with wide range of date/time formats e.g. minute, 15-min, hourly, daily

    ## note that in teh case of type "site", each site is thought of as a "pollutant"

    ## Author: David Carslaw 11 Sep. 09
    ## CHANGES:
   

### EXPERIMENTAL LOG SCALING###############################################
    if(log) nlog <- 10 else nlog <- FALSE
    yscale.components.log10 <- function(lim, ...) {
        ans <- yscale.components.default(lim = lim, ...)
        if(!log) return(ans)
        tick.at <- logTicks(10^lim, loc = 1:9)
        tick.at.major <- logTicks(10^lim, loc = 1)
        major <- tick.at %in% tick.at.major
        ans$left$ticks$at <- log(tick.at, 10)
        ans$left$ticks$tck <- ifelse(major, 1.5, 0.75)
        ans$left$labels$at <- log(tick.at, 10)
        ans$left$labels$labels <- as.character(tick.at)
        ans$left$labels$labels[!major] <- ""
        ans$left$labels$check.overlap <- FALSE
        ans
    }


    logTicks <- function (lim, loc = c(1, 5)) {
        ii <- floor(log10(range(lim))) + c(-1, 2)
        main <- 10^(ii[1]:ii[2])
        r <- as.numeric(outer(loc, main, "*"))
        r[lim[1] <= r & r <= lim[2]]
    }
###################################################################################

    vars <- c("date", pollutant)

##### warning messages and other checks ################################################################
    if (type =="site" & length(pollutant) > 1) stop("Only one pollutant allowed
with option type = 'site'")

    ## also ckeck that column "site" is present when type set to "default"
    ## but also check to see if dates are duplicated, if not, OK to proceed
    len.all <- length(mydata$date)
    len.unique <- length(unique(mydata$date))

    if (type == "default" & "site" %in% names(mydata) & len.all != len.unique) {
        if (length(unique(factor(mydata$site))) > 1) stop("More than one site has been detected: choose type = 'site' and a single pollutant")
    }

    if (length(percentile) > 1 & length(pollutant) > 1) {stop("Only one pollutant allowed when considering more than one percentile")}

    if (!missing(statistic) & missing(avg.time)) {
        warning("No averaging time applied, using avg.time ='month'")
        avg.time <- "month"}

#######################################################################################################

    ## data checks
    mydata <- checkPrep(mydata, vars, type)

    ## pad out any missing date/times so that line don't extend between areas of missing data

    theStrip <- strip

    if (date.pad) mydata <- date.pad(mydata, type)

    ## average the data if necessary (default does nothing)
    if (avg.time != "default") {
        ## deal with mutiple percentile values
        if (length(percentile) > 1) {

            mydata <- calcPercentile(mydata, pollutant = pollutant, period = avg.time,
                                      data.thresh = data.thresh, percentile = percentile)
            pollutant <-  paste("percentile.", percentile,  sep = "")
            vars <- names(mydata) ## new variables to use
            if (missing(group)) group <- TRUE

        } else {
            mydata <- timeAverage(mydata, period = avg.time,
                                   data.thresh = data.thresh, statistic = statistic,
                                   percentile = percentile)
        }
    }

    mydata <- cutData(mydata, type)

    ## The aim to to get colums "date", "site" then turn to column data using melt
    ## Finally end up with "date", "value", "variable"

    ## don't need type, now a condition
    vars <-  c(vars, "cond")
    vars <- vars[vars != type]
    mydata <- mydata[, vars]
    mydata <- rename(mydata, c(cond = "site")) ## change to name "site"

    if (type == "default") {
        mydata <- melt(mydata, id.var = c("date", "site"))
    } else {
        ## should always be in this order
        names(mydata)[2:3] <- c("value", "variable")
    }

    ## number of pollutants (or sites for type = "site")
    npol <- length(unique(mydata$variable)) ## number of pollutants

    ## layout - stack vertically
    if (missing(layout) & !group) layout <- c(1, npol)

    ## function to normalise data ##################################
    divide.by.mean <- function(x) {
        Mean <- mean(x$value, na.rm = TRUE)
        x$value <- x$value / Mean
        x
    }

    if (normalise) {
        ylab <- "normalised level"
        mydata <-  ddply(mydata, .(variable), divide.by.mean)
    }

    ## ylabs for more than one pollutant
    if (missing(ylab)) ylab <-  paste(pollutant, collapse = ", ")

    mylab <- sapply(seq_along(pollutant), function(x) quickText(pollutant[x], auto.text))

    if (type == "site") {
        mylab <- levels(mydata$variable)
        if (!group) layout <- c(1, npol)
        if (group) layout <- c(1, 1)
    }

    ## user-supplied names
    if (!missing(name.pol)) {mylab <- sapply(seq_along(name.pol), function(x)
                                             quickText(name.pol[x], auto.text))
                         }

    ## set up colours
    myColors <- openColours(cols, npol)

    ## basic function for lattice call + defaults
    myform <- formula("value ~ date")
    strip <- TRUE
    strip.left <- FALSE
    dates <- dateBreaks(mydata$date, date.breaks)$major ## for date scale
    formats <- dateBreaks(mydata$date, date.breaks)$format

    scales <- list(x = list(at = dates, format = formats), y = list(log = nlog))

    xlim <- range(mydata$date)

    ## layout changes depening on plot type

    if (!group) { ## sepate panels per pollutant
        strip <- FALSE
        myform <- formula("value ~ date | variable")
        ## proper names of labelling
        pol.name <- sapply(levels(mydata$variable),  function(x) quickText(x, auto.text))

        if (npol == 1) {
            strip.left <- FALSE
        } else {
            strip.left <- strip.custom(par.strip.text = list(cex = 0.9), horizontal = FALSE,
                                       factor.levels = pol.name)
        }

        scales <- list(x = list(at = dates, format = formats), y = list(relation = "free",
                                                               rot = 0, log = nlog))

        if (missing(lty)) lty <- 1 ## don't need different line types here
    }

    ## if stacking of plots by year is needed
    if (stack) {
        mydata$year <- format(mydata$date, "%Y")
        layout <- c(1, length(unique(mydata$year)))
        strip <- FALSE
        myform <- formula("value ~ date | year")
        strip.left <- strip.custom(par.strip.text = list(cex = 0.9), horizontal = FALSE)
        dates <- as.POSIXct(unique(trunc(mydata$date, "months")), "GMT")

        scales <- list(x = list(at = dates, format = "%d-%b", relation = "sliced"), y = list(log = nlog))

        xlim <- dlply(mydata, .(year), function (x) range(x$date))

    }

    if (missing(key.columns)) key.columns <- npol

    ## keys and strips - to show or not


    if (key) {
        key <- list(lines = list(col = myColors[1:npol], lty = lty, lwd = lwd),
                    text = list(lab = mylab),  space = "bottom", columns = key.columns)
    } else {
        key <- NULL ## either there is a key or there is not
    }

    if (theStrip) {
        strip <- strip
        strip.left <- strip.left
    } else {
        strip <- FALSE
        strip.left <- FALSE
    }

    xyplot(myform,  data = mydata, groups = variable,
           as.table = TRUE,
           layout = layout,
           lty = lty,
           lwd = lwd,
           xlim = xlim,
           main = quickText(main),
           ylab = quickText(ylab, auto.text),
           scales = scales,
           key = key,
           strip = strip,
           strip.left = strip.left,
           yscale.components = yscale.components.log10,
           panel =  panel.superpose,...,
           panel.groups = function(x, y, col.line, col, col.se, type, group.number, lty, lwd, subscripts,...) {

               if (group.number == 1) {
                   panel.grid(-1, 0)
                   panel.abline(v = dates, col = "grey90")

               }
               if (!group & !stack) {
                   panel.abline(v = dates, col = "grey90")
                   panel.grid(-1, 0)
               }

               panel.xyplot(x, y, type = "l", lty = lty, lwd = lwd, col.line = myColors[group.number],...)
               if (smooth) panel.gam(x, y, col = myColors[group.number] , col.se =  myColors[group.number],
                                     lty = 1, lwd = 1, se = ci, ...)

           }
           )
}



