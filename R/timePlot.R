timePlot <- function(mydata,
                     pollutant = "nox",
                     group = FALSE,
                     stack = FALSE,
                     normalise = FALSE,
                     avg.time = "default",
                     data.thresh = 0,
                     statistic = "mean",
                     percentile = NA,
                     date.pad = FALSE,
                     type = "default",
                     layout = c(1, 1),
                     cols = "brewer1",
                     main = "",
                     ylab = pollutant,
                     plot.type = "l",
                     lty = 1:length(pollutant),
                     lwd = 1,
                     pch = NA,
                     key = TRUE,
                     strip = TRUE,
                     log = FALSE,
                     smooth = FALSE,
                     ci = TRUE,
                     ref.x = NULL,
                     ref.y = NULL,
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

                                        #greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
                                        #strip only
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
    }


##### warning messages and other checks ################################################################

    ## also ckeck that column "site" is present when type set to "default"
    ## but also check to see if dates are duplicated, if not, OK to proceed
    len.all <- length(mydata$date)
    len.unique <- length(unique(mydata$date))

    if (type == "default" & "site" %in% names(mydata) & len.all != len.unique) {
        if (length(unique(factor(mydata$site))) > 1) stop("More than one site has been detected: choose type = 'site' and pollutant(s)")
    }

    if (length(percentile) > 1 & length(pollutant) > 1) {stop("Only one pollutant allowed when considering more than one percentile")}

    if (!missing(statistic) & missing(avg.time)) {
        message("No averaging time applied, using avg.time ='month'")
        avg.time <- "month"}

#######################################################################################################

    ## data checks
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    ## pad out any missing date/times so that line don't extend between areas of missing data

    theStrip <- strip

    if (date.pad) mydata <- date.pad(mydata, type)

    mydata <- cutData(mydata, type, ...)


    ## average the data if necessary (default does nothing)
    if (avg.time != "default") {
        ## deal with mutiple percentile values

        if (length(percentile) > 1) {

            mydata <- ddply(mydata, type, calcPercentile, pollutant = pollutant, avg.time = avg.time,
                            data.thresh = data.thresh, percentile = percentile)

            pollutant <-  paste("percentile.", percentile,  sep = "")

            if (missing(group)) group <- TRUE

        } else {

            mydata <- ddply(mydata, type, timeAverage, avg.time = avg.time, statistic = statistic,
                            percentile = percentile, data.thresh = data.thresh)
        }

    }

    mydata <- melt(mydata, id.var = c("date", type))


    if (type != "default") {

        group <- TRUE ## need to group pollutants if conditioning
        if (missing(layout)) layout <- NULL else layout <- layout
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

    mylab <- sapply(seq_along(pollutant), function(x) quickText(pollutant[x], auto.text))

    ## user-supplied names
    if (!missing(name.pol)) {mylab <- sapply(seq_along(name.pol), function(x)
                                             quickText(name.pol[x], auto.text))
                         }

    ## ylabs for more than one pollutant
    if (missing(ylab)) ylab <-  paste(pollutant, collapse = ", ")

    ## set up colours
    myColors <- if (length(cols) == 1 && cols == "greyscale")
        openColours(cols, npol+1)[-1] else openColours(cols, npol)

    ## basic function for lattice call + defaults
    myform <- formula(paste("value ~ date |", type))

    strip <- TRUE
    strip.left <- FALSE
    dates <- dateBreaks(mydata$date, date.breaks)$major ## for date scale
    formats <- dateBreaks(mydata$date, date.breaks)$format

    scales <- list(x = list(at = dates, format = formats), y = list(log = nlog))

    ## layout changes depening on plot type

    if (!group) { ## sepate panels per pollutant
        strip <- FALSE
        myform <- formula("value ~ date | variable")

        if (npol == 1) {
            strip.left <- FALSE
        } else {
            strip.left <- strip.custom(par.strip.text = list(cex = 0.9), horizontal = FALSE,
                                       factor.levels = mylab)
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
        ##  dates <- unique(dateTrunc(mydata$date, "months")) - this does not work?
        dates <- as.POSIXct(unique(paste(format(mydata$date, "%Y-%m"), "-01", sep ="")), "GMT")

        scales <- list(x = list(at = dates, format = "%d-%b", relation = "sliced"), y = list(log = nlog))

        xlim <- dlply(mydata, .(year), function (x) range(x$date))


    }

    if (missing(key.columns)) key.columns <- npol

    ## keys and strips - to show or not


    if (key) {
        ## type of key depends on whether points are plotted or not
        if (any(!is.na(pch))) {
            key <- list(lines = list(col = myColors[1:npol], lty = lty, lwd = lwd),
                        points = list(pch = pch, col = myColors[1:npol]),
                        text = list(lab = mylab),  space = "bottom", columns = key.columns)
        } else {
            key <- list(lines = list(col = myColors[1:npol], lty = lty, lwd = lwd),
                        text = list(lab = mylab),  space = "bottom", columns = key.columns)
        }
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

    ## special layout if type = "wd"
    layout = if (type == "wd") c(3, 3) else layout
    skip <- FALSE
    if (type == "wd") {
        ## re-order to make sensible layout
        wds <-  c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
        mydata$wd <- ordered(mydata$wd, levels = wds)

        ## see if wd is actually there or not
        wd.ok <- sapply(wds, function (x) {if (x %in% unique(mydata$wd)) FALSE else TRUE })
        skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])

        mydata$wd <- factor(mydata$wd)  ## remove empty factor levels

        layout = if (type == "wd") c(3, 3) else NULL
    }

    plt <- xyplot(myform,  data = mydata, groups = variable,
                  as.table = TRUE,
                  layout = layout,
                  skip = skip,
                  lty = lty,
                  lwd = lwd,
                  pch = pch,
                  main = quickText(main, auto.text),
                  par.strip.text = list(cex = 0.8),
                  ylab = quickText(ylab, auto.text),
                  scales = scales,
                  key = key,
                  strip = strip,
                  strip.left = strip.left,
                  yscale.components = yscale.components.log10,
                  panel =  panel.superpose,...,
                  panel.groups = function(x, y, col.line, col.symbol, col, col.se, type, group.number, lty,
                  lwd, pch, subscripts,...) {

                      if (group.number == 1) {
                          panel.grid(-1, 0)
                          panel.abline(v = dates, col = "grey90")

                      }
                      if (!group & !stack) {
                          panel.abline(v = dates, col = "grey90")
                          panel.grid(-1, 0)
                      }

                      panel.xyplot(x, y, type = plot.type, lty = lty, lwd = lwd,
                                   col.line = myColors[group.number],...)
                      ## deal with points separately - useful if missing data where line does not join consequtive points
                      if (any(!is.na(pch))) {
                          lpoints(x, y, type = "p", pch = pch, col.symbol = myColors[group.number],...)
                      }
                      if (smooth) panel.gam(x, y, col = myColors[group.number] , col.se =  myColors[group.number],
                                            lty = 1, lwd = 1, se = ci, ...)

                      ## add reference lines
                      panel.abline(v = ref.x, lty = 5)
                      panel.abline(h = ref.y, lty = 5)

                  }
                  )

#################
                                        #output
#################
    plot(plt)
    newdata <- mydata
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
                                        #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)
    invisible(output)

}



