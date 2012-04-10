## development of a calendar function
## David Carslaw November 2009
## modifications for internation Karl Ropkins 2010
## drawOpenKey add-in Karl Ropkins 2010

## calendarPlot shows air quality or other data in a conventional calendar format.
## This makes it easy to gain a feel about variations in concentrations and other parameters
## By default ir shows the day of the month for each day, but can also show the wind vector,
## which may also be normalised to wind speed.



##' Plot time series values in convential calendar format
##'
##' This function will plot one year of data by month laid out in a
##' conventional calendar format. The main purpose is to help rapidly visualise
##' potentially complex data in a familiar way. Users can also choose to show
##' daily mean wind vectors if wind speed and direction are available.
##'
##' \code{calendarPlot} will plot one year of data in a conventional calendar
##' format i.e. by month and day of the week. The main purpose of this function
##' is to make it easy to visualise data in a familiar way. Currently the mean
##' value of a variable is plotted using a colour scale. Further statistics
##' will be added in due course.
##'
##' If wind direction are available it is then possible to plot the wind
##' direction vector on each day. This is very useful for getting a feel for
##' the meteorological conditions that affect pollutant concentrations. Note
##' that if hourly or higher time resolution are supplied, then
##' \code{calendarPlot} will calculate daily averages using
##' \code{\link{timeAverage}}, which ensures that wind directions are
##' vector-averaged.
##'
##' If wind speed is also available, then setting the option \code{annotate =
##' "ws"} will plot the wind vectors whose length is scaled to the wind speed.
##' Thus information on the daily mean wind speed and direction are available.
##'
##' @param mydata A data frame minimally containing \code{date} and at least
##'   one other numeric variable and a year. The date should be in either
##'   \code{Date} format or class \code{POSIXct}.
##' @param pollutant Mandatory. A pollutant name corresponding to a variable in
##'   a data frame should be supplied e.g. \code{pollutant = "nox". }
##' @param year Year to plot e.g. \code{year = 2003}.
##' @param type Not yet implemented.
##' @param annotate This option controls what appears on each day of
##' the calendar. Can be: "date" - shows day of the month; "wd" -
##' shows vector-averaged wind direction, or "ws" - shows
##' vector-averaged wind direction scaled by wind speed. Finally it
##' can be "value" which shows the daily mean value.
##' @param statistic Statistic passed to \code{timeAverage}.
##' @param cols Colours to be used for plotting. Options include "default",
##'   "increment", "heat", "jet" and user defined. For user defined the user
##'   can supply a list of colour names recognised by R (type \code{colours()}
##'   to see the full list). An example would be \code{cols = c("yellow",
##'   "green", "blue")}
##' @param limits Use this option to manually set the colour scale limits. This
##'   is useful in the case when there is a need for two or more plots and a
##'   consistent scale is needed on each. Set the limits to cover the maximimum
##'   range of the data for all plots of interest. For example, if one plot had
##'   data covering 0--60 and another 0--100, then set \code{limits = c(0,
##'   100)}. Note that data will be ignored if outside the limits range.
##' @param lim A threshold value to help differentiate values above
##' and below \code{lim}. It is used when \code{annotate =
##' "value"}. See next few options for control over the labels used.
##' @param col.lim For the annotation of concentration labels on each
##' day. The first sets the colour of the text below \code{lim} and
##' the second sets the colour of the text above \code{lim}.
##' @param font.lim For the annotation of concentration labels on each
##' day. The first sets the font of the text below \code{lim} and the
##' second sets the font of the text above \code{lim}. Note that font
##' = 1 is normal text and font = 2 is bold text.
##' @param cex.lim For the annotation of concentration labels on each
##' day. The first sets the size of the text below \code{lim} and
##' the second sets the size of the text above \code{lim}.
##' @param digits The number of digits used to display concentration
##' values when \code{annotate = "value"}.
##' @param data.thresh Data capture threshold passed to
##' \code{timeAverage}. For example, \code{data.thresh = 75} means
##' that at least 75\% of the data must be available in a day for the
##' value to be calculate, else the data is removed.
##' @param main The plot title; default is pollutant and year.
##' @param key.header Adds additional text/labels to the scale key.
##'   For example, passing \code{calendarPlot(mydata, key.header = "header",
##'   key.footer = "footer")} adds addition text above and below the scale key.
##'   These arguments are passed to \code{drawOpenKey} via \code{quickText},
##'   applying the \code{auto.text} argument, to handle formatting.
##' @param key.footer see \code{key.header}.
##' @param key.position Location where the scale key is to plotted.  Allowed
##'   arguments currently include \code{"top"}, \code{"right"}, \code{"bottom"}
##'   and \code{"left"}.
##' @param key Fine control of the scale key via \code{drawOpenKey}. See
##'   \code{drawOpenKey} for further details.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the `2' in NO2.
##' @param ... Other graphical parameters are passed onto the \code{lattice}
##'   function \code{lattice:levelplot}, with common axis and title labelling
##'   options (such as \code{xlab}, \code{ylab}, \code{main}) being passed to
##'   via \code{quickText} to handle routine formatting.
##' @export
##' @import grid
##' @return As well as generating the plot itself, \code{calendarPlot} also
##'   returns an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- calendarPlot(mydata, "nox")}, this output can be used to
##'   recover the data, reproduce or rework the original plot or undertake
##'   further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}. See
##'   \code{\link{openair.generics}} for further details.
##' @author David Carslaw
##' @seealso \code{\link{timePlot}}, \code{\link{timeVariation}}
##' @keywords methods
##' @examples
##'
##'
##' # load example data from package
##' data(mydata)
##'
##' # basic plot
##' calendarPlot(mydata, pollutant = "o3", year = 2003)
##'
##' # show wind vectors
##' calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "wd")
##'
##' # show wind vectors scaled by wind speed and different colours
##' calendarPlot(mydata, pollutant = "o3", year = 2003, annotate = "ws",
##' cols = "heat")
##'
##' # show only specific months with selectByDate
##' calendarPlot(selectByDate(mydata, month = c(3,6,10), year = 2003),
##' pollutant = "o3", year = 2003, annotate = "ws", cols = "heat")
##'
##'
##'
calendarPlot <- function(mydata,
                         pollutant = "nox",
                         year = 2003,
                         type = "default",
                         annotate = "date",
                         statistic = "mean",
                         cols = "heat",
                         limits = c(0, 100),
                         lim = NULL,
                         col.lim = c("grey30", "black"),
                         font.lim = c(1, 2),
                         cex.lim = c(0.6, 1),
                         digits = 0,
                         data.thresh = 0,
                         main = paste(pollutant, "in", year),
                         key.header = "", key.footer = "",
                         key.position = "right", key = TRUE,
                         auto.text = TRUE,
                         ...) {

    ##international keyboard
    ##first letter and ordered Sun to Sat
                                        #weekday.abb <- substr(make.weekday.abbs(), 1, 1)[c(7, 1:6)]
    weekday.abb <- substr(format(ISOdate(2000, 1, 2:8), "%A"), 1, 1)[c(7, 1:6)]

    ##extra args
    extra.args <- list(...)

                                        #label controls
                                        #(main currently handled in formals)
    extra.args$xlab <- if("xlab" %in% names(extra.args))
        quickText(extra.args$xlab, auto.text) else quickText("", auto.text)
    extra.args$ylab <- if("ylab" %in% names(extra.args))
        quickText(extra.args$ylab, auto.text) else quickText("", auto.text)

    ## extract variables of interest
    if (annotate %in% c("date", "value")) vars <- c("date", pollutant)
    if (annotate == "wd") vars <- c("wd", "ws", "date", pollutant)
    if (annotate == "ws") vars <- c("wd", "ws", "date", pollutant)

    ## select year first, then check variables
    mydata <- selectByDate(mydata, year = year)
    if (nrow(mydata) == 0 ) stop("No data to plot - check year chosen")
    mydata <- openair:::checkPrep(mydata, vars, "default", remove.calm = FALSE)

    main <- quickText(main, auto.text)

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
        pad.start <- as.numeric(format(mydata$date[1], "%w")) + 1

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
    if ("POSIXt" %in% class(mydata$date)) {
        mydata <- timeAverage(mydata, "day", statistic= statistic, data.thresh = data.thresh)
        mydata$date <- as.Date(mydata$date)
    }

    ## type not yet used, set to month
    type <- "month"
    mydata <- cutData(mydata, type = type, ...)
    baseData <- mydata

    mydata <- ddply(mydata, type, function(x) prepare.grid(x, pollutant))


    if (annotate == "wd") {
        baseData$wd <- baseData$wd * 2 * pi / 360
        wd <- ddply(baseData, type, function(x) prepare.grid(x, "wd"))
    }

    if (annotate == "ws") {
        baseData$wd <- baseData$wd * 2 * pi / 360
        wd <- ddply(baseData, type, function(x) prepare.grid(x, "wd"))
        ws <- ddply(baseData, type, function(x) prepare.grid(x, "ws"))
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
    legend <- openair:::makeOpenKeyLegend(key, legend, "calendarPlot")

    levelplot.args <- list(x = conc.mat ~ x * y | month, data = mydata,
                           par.settings = cal.theme,
                           main = main,
                           at = col.scale,
                           col.regions = col,
                           as.table = TRUE,
                           scales = list(y = list(draw = FALSE),
                           x = list(at = 1:7, labels = weekday.abb, tck = 0),
                           par.strip.text = list(cex = 0.8),
                           alternating = 1, relation = "free"),
                           aspect = 6/7,
                           between = list(x = 1),
                           colorkey = FALSE, legend = legend,
                           panel = function(x, y, subscripts,...) {
                               panel.levelplot(x, y, subscripts,...)
                               panel.abline(v=c(0.5: 7.5), col = "grey90")
                               panel.abline(h=c(0.5: 7.5), col = "grey90")

                               if (annotate == "date") {
                                   ltext(x, y, labels = mydata$date.mat[subscripts], cex = 0.6,
                                         col = as.character(mydata$dateColour[subscripts]))
                               }

                               if (annotate == "value") {
                                   ## add some dates for navigation
                                   date.col <- as.character(mydata$dateColour[subscripts])
                                   ids <- which(date.col == "black")
                                   date.col[ids] <- "transparent"
                                   ltext(x, y, labels = mydata$date.mat[subscripts], cex = 0.6,
                                         col = date.col)

                                   concs <- mydata$conc.mat[subscripts]

                                   ## deal with values above/below threshold
                                   ids <- seq_along(concs)
                                   the.cols <- rep(col.lim[1], length(ids))
                                   the.font <- rep(font.lim[1], length(ids))
                                   the.cex <- rep(cex.lim[1], length(ids))
                                   if (!is.null(lim)) {
                                       ## ids where conc is >= lim
                                       ids <- which(concs >= lim)
                                       the.cols[ids] <- col.lim[2]
                                       the.font[ids] <- font.lim[2]
                                       the.cex[ids] <- cex.lim[2]
                                   }

                                   the.labs <- round(concs, digits = digits)
                                   id <- which(is.na(the.labs))
                                   if (length(id) > 0) {
                                       the.labs <- as.character(the.labs)
                                       the.labs[id] <- ""
                                   }
                                   ltext(x, y, labels = the.labs, cex = the.cex,
                                         font = the.font, col = the.cols)
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

                           })

                                        #reset for extra.args
    levelplot.args <- openair:::listUpdate(levelplot.args, extra.args)

                                        #plot
    print(do.call(levelplot, levelplot.args))

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

