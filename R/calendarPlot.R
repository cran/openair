## development of a calendar function
## David Carslaw November 2009
## modifications for internation Karl Ropkins 2010
## drawOpenKey add-in Karl Ropkins 2010

## calendarPlot shows air quality or other data in a conventional calendar format.
## This makes it easy to gain a feel about variations in concentrations and other parameters
## By default ir shows the day of the month for each day, but can also show the wind vector,
## which may also be normalised to wind speed.

calendarPlot <- function(mydata,
                          pollutant = "nox",
                          year = 2003,
                          type = "default",
                          annotate = "date",
                          statistic = "mean",
                          cols = "heat",
                          limits = c(0, 100),
                          main = quickText((paste(pollutant, "in", year))),
                          key.header = "", key.footer = "",
                          key.position = "right", key = NULL, 
                          auto.text = TRUE,
                          ...) {

    ##international keyboard
    ##first letter and ordered Sun to Sat
    weekday.abb <- substr(make.weekday.abbs(), 1, 1)[c(7, 1:6)]    

    ## extract variables of interest
    if (annotate == "date") vars <- c("date", pollutant)
    if (annotate == "wd") vars <- c("wd", "date", pollutant)
    if (annotate == "ws") vars <- c("wd", "ws", "date", pollutant)

    ## select year first, then check variables
    mydata <- selectByDate(mydata, year = year)
    if (nrow(mydata) == 0 ) stop("No data to plot - check year chosen")
    mydata <- checkPrep(mydata, vars, "default")

    main <- main

    ## themes for calendarPlot
    def.theme  <- list(strip.background = list(col = "#ffe5cc"),
                     strip.border = list(col = "black"),
                     axis.line = list(col = "black"),
                     par.strip.text = list(cex =1))

    cal.theme <- list(strip.background = list(col = "grey90"),
                     strip.border = list(col = "transparent"),
                     axis.line = list(col = "transparent"),
                     par.strip.text = list(cex = 0.8))

    lattice.options(default.theme = cal.theme)

    ## all the days in the year
    all.dates <- seq(as.Date(paste(year, "-01-01", sep = "")),
                             as.Date(paste(year, "-12-31", sep = "")), by = "day")

    prepare.grid <- function(mydata, pollutant) {

        ## make sure all days in month are present
        all.days <-  all.dates[format(all.dates, "%B") %in% format(mydata$date[1], "%B")]
        all.days <- data.frame(date = all.days)
        mydata <- merge(mydata, all.days, all = TRUE)

        firstDay <- format(mydata$date[1], "%A")
        lastDay <- as.numeric(format(mydata$date[length(mydata$date)], "%d"))

        ## number of blank cells at beginning to get calendar format
        pad.start <- as.numeric(format(mydata$date[1], "%w"))

        ## need to do in reverse to plot easily
        conc <- rev(mydata[, pollutant])

        ## day of the month
        theDates <- as.numeric(format(mydata$date, "%d"))
        theDates <- rev(theDates)

        daysAtEnd <- 42 - pad.start - nrow(mydata) ## 7x6 regular grid
        conc <- c(rep(NA, daysAtEnd), conc)

        ## get relevant days in previous and next month, like a real calendar
        endDates <- mydata$date[nrow(mydata)] + (1 :daysAtEnd)
        endDates <- rev(as.numeric(format(endDates, "%d")))

        theDates <- c(endDates, theDates)

        beginDates <-  -1 * (1 : pad.start) + mydata$date[1]
        beginDates <- as.numeric(format(beginDates, "%d"))

        conc <- c(conc, rep(NA, pad.start))

        if (pad.start != 0) theDates <- c(theDates, beginDates)

        ## colurs for dates
        dateColour <- c(rep("grey70", daysAtEnd), rep("black", nrow(mydata)),
                        rep("grey70", pad.start))

        ## convert to matrix
        conc.mat <- matrix(conc, ncol = 7, byrow = TRUE)
        date.mat <- matrix(theDates, ncol = 7, byrow = TRUE)
        colour.mat <- matrix(dateColour, ncol = 7, byrow = TRUE)

        ## need to reverse each row for calendar format
        conc.mat <-  as.vector(apply(conc.mat, 1, rev))
        date.mat <-  as.vector(apply(date.mat, 1, rev))
        colour.mat <- as.vector(apply(colour.mat, 1, rev))

        grid <- data.frame(expand.grid(x = 1:7, y = 1:6))
        results <- suppressWarnings(data.frame(x = grid$x, y = grid$y, conc.mat,
                              month = format(mydata$date[1], "%B"),
                              date.mat = date.mat, dateColour = colour.mat))

        results
    }

    ## calculate daily means
    if (class(mydata$date)[1] == "POSIXt") {
        mydata <- timeAverage(mydata, "day")
        mydata$date <- as.Date(mydata$date)
    }

    mydata <- cutData(mydata, type = "month")
    baseData <- mydata

    mydata <- ddply(mydata, .(cond), function(x) prepare.grid(x, pollutant))

    if (annotate == "wd") {
        baseData$wd <- baseData$wd * 2 * pi / 360
        wd <- ddply(baseData, .(cond), function(x) prepare.grid(x, "wd"))
    }

    if (annotate == "ws") {
        baseData$wd <- baseData$wd * 2 * pi / 360
        wd <- ddply(baseData, .(cond), function(x) prepare.grid(x, "wd"))
        ws <- ddply(baseData, .(cond), function(x) prepare.grid(x, "ws"))
        ## normalise wind speeds to highest daily mean
        ws$conc.mat <- ws$conc.mat / max(ws$conc.mat, na.rm = TRUE)
    }

    ## set up scales
    nlev <- 200
     if(missing(limits)) breaks <- pretty(mydata$conc.mat, n = nlev) else breaks <- pretty(limits,n = nlev)
    nlev2 <- length(breaks)
    col <- openColours(cols, (nlev2 - 1))
    col.scale <- breaks

    #################
    #scale key setup
    #################
    legend <- list(col = col, at = col.scale, space = key.position, 
         auto.text = auto.text, footer = key.footer, header = key.header, 
         height = 1, width = 1.5, fit = "all")
    if (!is.null(key)) 
         if (is.list(key)) 
             legend[names(key)] <- key
         else warning("In calendarPlot(...):\n  non-list key not exported/applied\n  [see ?drawOpenKey for key structure/options]", 
             call. = FALSE)
    legend <- list(temp = list(fun = drawOpenKey, args = list(key = legend, 
         draw = FALSE)))
    names(legend)[1] <- if(is.null(key$space)) key.position else key$space

    print(levelplot(conc.mat ~ x * y | month, data = mydata,
              par.settings = cal.theme,
              main = main,
              at = col.scale,
              col.regions = col,
              as.table = TRUE,
              scales = list(y = list(draw = FALSE),
              x = list(at = 1:7, labels = weekday.abb, tck = 0),
              alternating = 1, relation = "free"),
              xlab = "",
              aspect = 6/7,
              ylab = "",
              between = list(x = 1),
              colorkey = FALSE, legend = legend,
              panel = function(x, y, subscripts,...) {
                  panel.levelplot(x, y,subscripts,...)
                  panel.abline(v=c(0.5: 7.5), col = "grey90")
                  panel.abline(h=c(0.5: 7.5), col = "grey90")

                  if (annotate == "date") {
                      ltext(x, y, labels = mydata$date.mat[subscripts], cex = 0.6,
                        col = as.character(mydata$dateColour[subscripts]))
                  }

                  if (annotate == "wd") {
                       larrows(x + 0.5 * sin(wd$conc.mat[subscripts]),
                              y +  0.5 * cos(wd$conc.mat[subscripts]),
                              x +  -0.5 * sin(wd$conc.mat[subscripts]),
                              y +  -0.5 * cos(wd$conc.mat[subscripts]),
                              angle = 20, length = 0.07, lwd = 0.5)
                  }

                  if (annotate == "ws") {
                      larrows(x + (0.5 * sin(wd$conc.mat[subscripts]) *
                                   ws$conc.mat[subscripts]),
                              y +  (0.5 * cos(wd$conc.mat[subscripts]) *
                                    ws$conc.mat[subscripts]) ,
                              x +  (-0.5 * sin(wd$conc.mat[subscripts]) *
                                    ws$conc.mat[subscripts]) ,
                              y +  (-0.5 * cos(wd$conc.mat[subscripts]) *
                                    ws$conc.mat[subscripts]),
                              angle = 20, length = 0.07, lwd = 0.5)
                  }

              }))

    ## reset theme
    lattice.options(default.theme = def.theme)

    #################
    #output
    #################
    plt <- trellis.last.object()
    newdata <- mydata
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)  

}

