timeVariation <- function(mydata,
                          pollutant = "nox",
                          local.time = FALSE,
                          normalise = FALSE,
                          ylab = pollutant,
                          xlab = c("hour", "hour", "month", "weekday"),
                          ylim = NA,
                          name.pol = pollutant,
                          type = "default",
                          group = NULL,
                          ci = TRUE,
                          cols = "hue",
                          main = "",
                          key = NULL,
                          key.columns = 1,
                          auto.text = TRUE,
                          alpha = 0.4, ...)   {


    #greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        #strip only
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
    }
   
    vars <- c("date", pollutant)
    
    if (!missing(group) & length(pollutant) > 1) {
        stop("Can only have one pollutant with a grouping variable, or several pollutants and no grouping variable.")}

    ## only one type for now
    if (length(type) > 1) stop("Can only have one type for timeVariation.")

    if (type %in% pollutant) stop("Cannot have type the same as a pollutant name.")

    if (!missing(group)) {
        if (group %in% pollutant) stop("Cannot have group the same as a pollutant name.")
    }

    ## check to see if type = "variable" (word used in code, so change)
    if (type == "variable") {
        mydata <- rename(mydata, c(variable = "tempVar"))
        type <- "tempVar"
    }

    ## if group is present, need to add that list of variables
    if (!missing(group)){
        
        if (group %in%  dateTypes) {
            vars <- unique(c(vars, "date"))
        } else {
            vars <- unique(c(vars, group))
        }
    }   
    
    ## data checks
    mydata <- checkPrep(mydata, vars, type)
    if (!missing(group))  mydata <- cutData(mydata, group, ...) 
    mydata <- cutData(mydata, type, ...)

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
    
    if (missing(group)) {
        mydata <- melt(mydata, measure.vars = pollutant)
        mydata$variable <- factor(mydata$variable)  ## drop unused factor levels
        
    } else {
        names(mydata)[2:3] <- c("value", "variable")
        mydata$variable <- factor(mydata$variable)  ## drop unused factor levels
        mylab <-  sapply(levels(mydata[ , "variable"]), function(x) quickText(x, auto.text))
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
        weekday <- ordered(weekday, levels = format(ISOdate(2000, 1, 3:9), "%A"))
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

    

    ## hour ############################################################################

    data.hour <- calc.wd(mydata, vars = "hour", pollutant, type)
    if (normalise) data.hour <-  ddply(data.hour, .(variable), divide.by.mean)

    if (is.null(xlab[2]) | is.na(xlab[2])) xlab[2] <- "hour"   
    
    ## proper names of labelling ##############################################################################
    if (type != "default") {
        stripName <- sapply(levels(mydata[ , type]), function(x) quickText(x, auto.text))
        strip <- strip.custom(factor.levels =  stripName)
    } else {
        strip <- FALSE
    }
    ## ########################################################################################################
    
    temp <- paste(type, collapse = "+")
    myform <- formula(paste("Mean ~ hour | ", temp, sep = ""))

    if (missing(ylim)) ylim.hour <- rng(data.hour) else ylim.hour <- ylim
    
    hour <- xyplot(myform,  data = data.hour, groups = variable,
                   as.table = TRUE,
                   main = main,
                   ylab = quickText(ylab, auto.text),
                   xlab = xlab[2],
                   xlim = c(0, 23),
                   ylim = ylim.hour,
                   strip = strip,
                   par.strip.text = list(cex = 0.8),
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

    data.weekday <- calc.wd(mydata, vars = "weekday", pollutant, type)
    if (normalise) data.weekday <-  ddply(data.weekday, .(variable), divide.by.mean)

    data.weekday$weekday <- ordered(data.weekday$weekday, levels = format(ISOdate(2000, 1, 3:9), "%A"))

    data.weekday$weekday <- as.numeric(as.factor(data.weekday$weekday))

                                        #note: 4 not 3

    if (is.null(xlab[4]) | is.na(xlab[4])) xlab[4] <- "weekday"

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("Mean ~ weekday | ", temp, sep = ""))

    if (missing(ylim)) ylim.weekday <- rng(data.weekday) else ylim.weekday <- ylim 
    
    day <- xyplot(myform,  data = data.weekday, groups = variable,
                  as.table = TRUE,
                  par.settings = simpleTheme(col = myColors, pch = 16),
                  scales = list(x = list(at = 1:7, labels = format(ISOdate(2000, 1, 3:9), "%a"))),
                  ylab = quickText(ylab, auto.text),
                  xlab = xlab[4],
                  ylim = ylim.weekday,
                  strip = strip,
                  par.strip.text = list(cex = 0.8),
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
                  
    data.month <- calc.wd(mydata, vars = "month", pollutant, type)
    if (normalise) data.month <-  ddply(data.month, .(variable), divide.by.mean)

                                        #note: 3 not 4
    if (is.null(xlab[3]) | is.na(xlab[3])) xlab[3] <- "month"

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("Mean ~ month | ", temp, sep = ""))

    if (missing(ylim)) ylim.month <-  rng(data.month) else ylim.month <- ylim 
    
    month <- xyplot(myform,  data = data.month, groups = variable,
                    as.table = TRUE,
                    ylab = quickText(ylab, auto.text),
                    xlab = xlab[3],
                    ylim = ylim.month,
                    xlim = c(0.5, 12.5),
                    key = key,
                    main = main,
                    strip = strip,
                    par.strip.text = list(cex = 0.8), 
                    par.settings = simpleTheme(col = myColors, pch = 16),
                    scales = list(x = list(at = 1:12, labels = substr(format(seq(as.Date("2000-01-01"),
                                                      as.Date("2000-12-31"), "month"), "%B"), 1, 1))),
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

    ## day and hour ############################################################################
    data.day.hour <- calc.wd(mydata, vars = "day.hour", pollutant, type)
    
    if (normalise) data.day.hour <-  ddply(data.day.hour, .(variable), divide.by.mean)

    ids <- which(is.na(data.day.hour$Lower)) ## missing Lower ci, set to mean
    data.day.hour$Lower[ids] <-  data.day.hour$Mean
    ids <- which(is.na(data.day.hour$Upper)) ## missing Upper ci, set to mean
    data.day.hour$Upper[ids] <-  data.day.hour$Mean

    if (is.null(xlab[1]) | is.na(xlab[1])) xlab[1] <- "hour"

    ## proper names of labelling ##############################################################################   

    strip <- strip.custom(par.strip.text = list(cex = 0.8))

    
    if (type == "default") {     
        strip.left <- FALSE
        layout <- c(7, 1)
        
    } else { ## two conditioning variables        
        stripName <- sapply(levels(mydata[ , type]), function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels =  stripName)
        layout <- NULL
    }
    ## ########################################################################################################
    
    temp <- paste(type, collapse = "+")
    if (type == "default") {
        myform <- formula("Mean ~ hour | weekday")
    } else {
        myform <- formula(paste("Mean ~ hour | weekday *", temp, sep = ""))
    }

    if (missing(ylim)) ylim.day.hour <-  rng(data.day.hour) else ylim.day.hour <- ylim 
    
    day.hour <- xyplot(myform ,  data = data.day.hour, groups = variable,
                       as.table = TRUE,
                       main = main,
                       xlim = c(0, 23),
                       ylim = ylim.day.hour,
                       ylab = quickText(ylab, auto.text),
                       xlab = xlab[1],
                       layout = layout,
                       par.settings = simpleTheme(col = myColors),
                       scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                       key = key,                      
                       strip = strip,
                       strip.left = strip.left,
                       par.strip.text = list(cex = 0.8),
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

    subsets = c("day.hour", "hour", "day", "month")

    main.plot <- function(...) {
        if (type == "default") {
            print(update(day.hour, key = list(rectangles = list(col = myColors[1:npol], border = NA),
                                   text = list(lab = mylab), space = "bottom", columns = key.columns,
                                   title = "", lines.title = 1)
                         ), position = c(0, 0.5, 1, 1), more = TRUE)
        } else {
            print(update(useOuterStrips(day.hour, strip = strip, strip.left = strip.left),
                         key = list(rectangles = list(col = myColors[1:npol], border = NA),
                         text = list(lab = mylab), space = "bottom", columns = key.columns,
                         title = "", lines.title = 1)
                         ), position = c(0, 0.5, 1, 1), more = TRUE)
        }
        print(hour, position = c(0, 0, 0.33, 0.53), more = TRUE)
        print(month, position = c(0.33, 0, 0.66, 0.53), more = TRUE)
        print(day, position = c(0.66, 0, 1, 0.53))
        ## use grid to add an overall title
        grid.text(overall.main, 0.5, 0.975, gp = gpar(fontsize = 14))
    }
    ind.plot = function(x){
        update(x, key = list(
                  rectangles = list(col = myColors[1:npol], border = NA),
                  text = list(lab = mylab), space = "top", columns = key.columns)
               )
    }

    main.plot()
    output <- (list(plot = list(day.hour, hour, day, month, subsets = subsets),
                    data = list(data.day.hour, data.hour, data.weekday, data.month, subsets = subsets),
                    call = match.call(),
                    main.plot = main.plot, ind.plot = ind.plot
                    ))
    names(output$data)[1:4] <- subsets
    names(output$plot)[1:4] <- subsets
    class(output) <- "openair"

    #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale") 
        trellis.par.set("strip.background", current.strip)

    invisible(output)
}

calc.wd <- function(mydata, vars = "day.hour", pollutant, type) {

    summary.values <- function(mydata, vars, FUN, type) {
        
        if (vars == "hour")  myform <- formula(paste("value ~ variable + hour +", type))           
        
        if (vars == "day.hour")  myform <- formula(paste("value ~ variable + weekday + hour +", type))
        
        if (vars == "weekday") myform <- formula(paste("value ~ variable + weekday +", type))
        
        if (vars == "month") myform <- formula(paste("value ~ variable + month +", type))                   
        
        mydata <- aggregate(myform, data = mydata, FUN)
        mydata        
    }

    ## function to calculate statistics dealing with wd properly
    if (any(!pollutant %in% "wd")) {
        data1 <- subset(mydata, variable != "wd")
        data1 <-  summary.values(data1, vars, errorInMean, type)
        data1 <- data.frame(subset(data1, select = -value), data1$value)                
    }

    if ("wd" %in% pollutant) {
        data2 <-  subset(mydata, variable == "wd")
        data2 <-  summary.values(data2, vars, wd.smean.normal, type)
        data2 <- data.frame(subset(data2, select = -value), data2$value)
    }

    if (length(pollutant) > 1 & "wd" %in% pollutant) data2 <- rbind.fill(data1, data2)

    if (!"wd" %in% pollutant) data2 <- data1

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




