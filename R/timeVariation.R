timeVariation <- function(mydata,
                           pollutant = "nox",
                           local.time = FALSE,
                           normalise = FALSE,
                           ylab = pollutant,
                           xlab = NULL,
                           name.pol = pollutant,
                           type = "default",
                           ci = TRUE,
                           cols = "hue",
                           main = "",
                           key = NULL,
                           key.columns = 1,
                           auto.text = TRUE,
                           alpha = 0.4, ...)   {

    

    ##update weekday and month locally
    weekday.name <- make.weekday.names()
    weekday.abb <- make.weekday.abbs()
    month.name <- substr(make.month.names(), 1, 1) ## first letter of month name

    ## extract variables of interest
    if (type == "wd") vars <- c("date", pollutant, "wd")
    if (type == "ws") vars <- c("date", pollutant, "ws")
    if (type == "site") vars <- c("date", pollutant, "site")
    if (type != "wd" & type != "ws" & type != "site") vars <- c("date", pollutant)

    if (type != "default" & length(pollutant) > 1) {
        stop("Can only have one pollutant with type = site")}

    ## data checks
    mydata <- checkPrep(mydata, vars, type)
    mydata <- cutData(mydata, type)
    ## don't need type, now a condition
    vars <-  c(vars, "cond")
    vars <- vars[vars != type]
    mydata <- mydata[, vars]
    names(mydata)[names(mydata) == "cond"] <- "site" ## change to name "site"

    mydata <- na.omit(mydata)

    ## title for overall and individual plots
    overall.main <- quickText(main, auto.text)
    main <- ""

    if (local.time) mydata$date <- as.POSIXct(format(mydata$date, tz = "Europe/London"))

    ## ylabs for more than one pollutant
    if (missing(ylab)) ylab <-  paste(pollutant, collapse = ", ")

    if (missing(name.pol)) mylab <- sapply(seq_along(pollutant), function(x)
                                           quickText(pollutant[x], auto.text))

    if (!missing(name.pol)) mylab <- sapply(seq_along(name.pol), function(x)
                                            quickText(name.pol[x], auto.text))

    if (type == "default") {
        mydata <- melt(mydata, id.var = c("date", "site"))
    } else {
        ## should always be in this order
        names(mydata)[2:3] <- c("value", "variable")
        mylab <- levels(factor(mydata$variable))
    }


    divide.by.mean <- function(x) {
        Mean <- mean(x$Mean, na.rm = TRUE)
        x$Mean <- x$Mean / Mean
        x$Lower <- x$Lower / Mean
        x$Upper <- x$Upper / Mean
        x
    }

    if (normalise) ylab <- "normalised level"

    ## calculate temporal components
    mydata <- within(mydata, {
        weekday <- format(date, "%A")
        weekday <- ordered(weekday, levels = weekday.names)
        hour <- as.numeric(format(date, "%H"))
        month <- as.numeric(format(date, "%m"))}
                     )
    ## polygon that can deal with missing data
    poly.na <- function(x1, y1, x2, y2, group.number) {
        for(i in seq(2, length(x1)))
            if (!any(is.na(y2[c(i - 1, i)])))
                lpolygon(c(x1[i - 1], x1[i], x2[i], x2[i - 1]),
                         c(y1[i - 1], y1[i], y2[i], y2[i - 1]),
                         col = myColors[group.number], border = NA, alpha = alpha)
    }

    ## y range taking account of expanded uncertainties
    rng <- function(x) {
        if (ci) {
            lims <- range(c(x$Lower, x$Upper), na.rm = TRUE)
            inc <- 0.04 * abs(lims[2] - lims[1])
            lims <- c(lims[1] - inc, lims[2] + inc)
        } else {
            lims <- range(c(x$value, x$value), na.rm = TRUE)
            inc <- 0.04 * abs(lims[2] - lims[1])
            lims <- c(lims[1] - inc, lims[2] + inc)
        }
        lims
    }

    npol <- length(unique(mydata$variable)) ## number of pollutants

    ## number of columns for key
    if (missing(key.columns)) key.columns <- npol

    myColors <- openColours(cols, npol)

    ## for individual plot keys - useful if only one of the plots is extracted after printing
    if (!is.null(key)) {
        key <- list(rectangles = list(col = myColors[1:npol], border = NA),
                    text = list(lab = mylab),  space = "bottom", columns = key.columns)

        main <- overall.main
    }

    ## day and hour ############################################################################
    data.day.hour <- calc.wd(mydata, vars = "day.hour", pollutant)
    
    if (normalise) data.day.hour <-  ddply(data.day.hour, .(variable), divide.by.mean)

    ids <- which(is.na(data.day.hour$Lower)) ## missing Lower ci, set to mean
    data.day.hour$Lower[ids] <-  data.day.hour$Mean
    ids <- which(is.na(data.day.hour$Upper)) ## missing Upper ci, set to mean
    data.day.hour$Upper[ids] <-  data.day.hour$Mean

    if(is.null(xlab[1])) {
        xlab[1] <- "hour"
    } else {
        if(is.na(xlab[1])) xlab[1] <- "hour"
    }

    day.hour <- xyplot(Mean ~ hour | weekday,  data = data.day.hour, groups = variable,
                       as.table = TRUE,
                       main = main,
                       layout = c(7, 1),
                       xlim = c(0, 23),
                       ylim = rng(data.day.hour),
                       ylab = quickText(ylab, auto.text),
                       xlab = xlab[1],
                       scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                       key = list(rectangles = list(col = myColors[1:npol], border = NA),
                       text = list(lab = mylab),  space = "bottom", columns = key.columns),
                       strip = strip.custom(par.strip.text = list(cex = 0.9)),
                       par.settings = simpleTheme(col = myColors),

                       panel =  panel.superpose,...,
                       panel.groups = function(x, y, col.line, type, group.number,
                       subscripts,...) {
                           ## add grid lines once (otherwise they overwrite the data)
                           if (group.number == 1) {
                               panel.grid(-1, 0)
                               panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                           }
                           panel.xyplot(x, y, type = "l", col.line = myColors[group.number],...)

                           if (ci) {poly.na(x, data.day.hour$Lower[subscripts], x,
                                            data.day.hour$Upper[subscripts], group.number)}
                       })

    ## hour ############################################################################

    data.hour <- calc.wd(mydata, vars = "hour", pollutant)
    if (normalise) data.hour <-  ddply(data.hour, .(variable), divide.by.mean)

    if(is.null(xlab[2])) {
        xlab[2] <- "hour"
    } else {
        if(is.na(xlab[2])) xlab[2] <- "hour"
    }

    hour <- xyplot(Mean ~ hour,  data = data.hour, groups = variable,
                   as.table = TRUE,
                   main = main,
                   ylab = quickText(ylab, auto.text),
                   xlab = xlab[2],
                   xlim = c(0, 23),
                   ylim = rng(data.hour),
                   key = key,
                   scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                   par.settings = simpleTheme(col = myColors),
                   panel =  panel.superpose,...,
                   panel.groups = function(x, y, col.line, type, group.number, subscripts,...) {
                       if (group.number == 1) {
                           panel.grid(-1, 0)
                           panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                       }
                       panel.xyplot(x, y, type = "l", col.line = myColors[group.number],...)

                       if (ci) {poly.na(x, data.hour$Lower[subscripts], x,
                                        data.hour$Upper[subscripts], group.number)}

                   })
    ## weekday ############################################################################

    data.weekday <- calc.wd(mydata, vars = "weekday", pollutant)
    if (normalise) data.weekday <-  ddply(data.weekday, .(variable), divide.by.mean)

    data.weekday$weekday <- substr(data.weekday$weekday, 1, 3)
    data.weekday$weekday <- ordered(data.weekday$weekday, levels = weekday.abb)

    data.weekday$weekday <- as.numeric(as.factor(data.weekday$weekday))

    #note: 4 not 3
    if(is.null(xlab[4])) {
        xlab[4] <- "weekday"
    } else {
        if(is.na(xlab[4])) xlab[4] <- "weekday"
    }


    day <- xyplot(Mean ~ weekday,  data = data.weekday, groups = variable,
                  par.settings = simpleTheme(col = myColors, pch = 16),
                  scales = list(x = list(at = 1:7, labels = weekday.abb)),
                  ylab = quickText(ylab, auto.text),
                  xlab = xlab[4],
                  ylim = rng(data.weekday),
                  key = key,
                  main = main,
                  panel =  panel.superpose,...,
                  panel.groups = function(x, y, col.line, type, group.number, subscripts,...) {
                      if (group.number == 1) {
                          panel.grid(-1, 0)
                          panel.abline(v = 1:7, col = "grey85")
                      }
                      panel.xyplot(x, y, type = "l", col.line = myColors[group.number],...)
                      panel.xyplot(x, y, type = "p", col.point = myColors[group.number],...)

                      if (ci) {panel.rect(x - 0.3, data.weekday$Lower[subscripts], x + 0.3,
                                          data.weekday$Upper[subscripts],
                                          fill = myColors[group.number],
                                          border = NA, alpha = alpha)}
                  })

    ## month ############################################################################

    data.month <- calc.wd(mydata, vars = "month", pollutant)
    if (normalise) data.month <-  ddply(data.month, .(variable), divide.by.mean)

    #note: 3 not 4
    if(is.null(xlab[3])) {
        xlab[3] <- "month"
    } else {
        if(is.na(xlab[3])) xlab[3] <- "month"
    }

    month <- xyplot(Mean ~ month,  data = data.month, groups = variable,
                    ylab = quickText(ylab, auto.text),
                    xlab = xlab[3],
                    ylim = rng(data.month),
                    xlim = c(0.5, 12.5),
                    key = key,
                    main = main,
                    par.settings = simpleTheme(col = myColors, pch = 16),
                    scales = list(x = list(at = 1:12, labels = month.name)),
                    panel =  panel.superpose,...,
                    panel.groups = function(x, y, col.line, type, group.number, subscripts,...) {
                        if (group.number == 1) {
                            panel.grid(-1, 0)
                            panel.abline(v = 1:12, col = "grey85")
                        }
                        panel.xyplot(x, y, type = "p", col.point = myColors[group.number],...)
                        panel.xyplot(x, y, type = "l", col.line = myColors[group.number],...)

                        if (ci) {panel.rect(x - 0.3, data.month$Lower[subscripts], x + 0.3,
                                            data.month$Upper[subscripts],
                                            fill = myColors[group.number],
                                            border = NA, alpha = alpha)}
                    })
    ## #######################################################################################

    print(day.hour, position = c(0, 0.5, 1, 1), more = TRUE)
    print(hour, position = c(0, 0, 0.33, 0.55), more = TRUE)
    print(month, position = c(0.33, 0, 0.66, 0.55), more = TRUE)
    print(day, position = c(0.66, 0, 1, 0.55))

    ## use grid to add an overall title
    grid.text(overall.main, 0.5, 0.975, gp = gpar(fontsize = 14))

    invisible(list(data.day.hour, data.hour, data.weekday, data.month, day.hour,
                   hour, day, month))
}

calc.wd <- function(mydata, vars = "day.hour", pollutant){

    summary.values <- function(mydata, vars, FUN) {
        if (vars == "hour")  {mydata <- with(mydata, aggregate(value, list(variable = variable, hour = hour),
            FUN))}
        if (vars == "day.hour")  {mydata <- with(mydata, aggregate(value, list(variable = variable,
            weekday = weekday, hour = hour), FUN))}
        if (vars == "weekday")  {mydata <- with(mydata, aggregate(value, list(variable = variable,
            weekday = weekday), FUN))}
        if (vars == "month")  {mydata <- with(mydata, aggregate(value, list(variable = variable, month = month),
            FUN))}
     
        mydata
        
    }

    ## function to calculate statistics dealing with wd properly
    if (any(pollutant %in% "wd" == FALSE)) {
        data1 <-  subset(mydata, variable != "wd")
        data1 <-  summary.values(data1, vars, errorInMean)
        data1 <- data.frame(subset(data1, select = -x), data1$x)
       
              
    }

    if ("wd" %in% pollutant) {
        data2 <-  subset(mydata, variable == "wd")
        data2 <-  summary.values(data2, vars, errorInMean)
        data2 <- data.frame(subset(data2, select = -x), data2$x)
    }

    if (length(pollutant) > 1 & "wd" %in% pollutant) data2 <- rbind.fill(data1, data2)

    if ("wd" %in% pollutant == FALSE) data2 <-data1

    if (length(pollutant) == 1 & "wd" %in% pollutant) data2 <- data2

    data2
}

wd.smean.normal <- function(wd) {
    ## function to calculate mean and 95% CI of the mean for wd

    u <- mean(sin(pi * wd / 180), na.rm = TRUE)
    v <- mean(cos(pi * wd / 180), na.rm = TRUE)
    Mean <- as.vector(atan2(u, v) * 360 / 2 / pi)
    ids <- which(Mean < 0)  ## ids where wd < 0
    Mean[ids] <- Mean[ids] + 360

    ## to calculate SD and conf int, need to know how much the angle changes from one point
    ## to the next. Also cannot be more than 180 degrees. Example change from 350 to 10 is not
    ## 340 but 20.
    wd.diff <- diff(wd)
    ids <- which(wd.diff < 0)
    wd.diff[ids] <- wd.diff[ids] + 360
    ids <- which(wd.diff > 180)
    wd.diff[ids] <- abs(wd.diff[ids] - 360)

    conf.int <- errorInMean(wd.diff)
    Lower <- conf.int[2]
    names(Lower) <- NULL

    Upper <- conf.int[3]
    names(Upper) <- NULL
    diff.wd <- (Upper - Lower) / 2

    c(Mean = Mean, Lower = Mean - diff.wd, Upper = Mean + diff.wd)
}




