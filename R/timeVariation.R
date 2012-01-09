##' Diurnal, day of the week and monthly variation
##'
##' Plots the diurnal, day of the week and monthly variation for
##' different variables, typically pollutant concentrations. Four
##' separate plots are produced.
##'
##' The variation of pollutant concentrations by hour of the day and
##' day of the week etc. can reveal many interesting features that
##' relate to source types and meteorology. For traffic sources, there
##' are often important differences in the way vehicles vary by
##' vehicles type e.g. less heavy vehicles at weekends.
##'
##' The \code{timeVariation} function makes it easy to see how
##' concentrations (and many other variable types) vary by hour of the
##' day and day of the week.
##'
##' The plots also show the 95\% confidence intervals in the mean. The
##' 95\% confidence intervals in the mean are calculated through
##' bootstrap simulations, which will provide more robust estimates of
##' the confidence intervals (particularly when there are relatively
##' few data).
##'
##' The function can handle multiple pollutants and uses the flexible
##' \code{type} option to provide separate panels for each 'type' ---
##' see \code{cutData} for more details. \code{timeVariation} can also
##' accept a \code{group} option which is useful if data are
##' stacked. This will work in a similar way to having multiple
##' pollutants in separate columns.
##'
##' The option \code{difference} will calculate the difference in
##' means of two pollutants together with bootstrap estimates of the
##' 95\% confidence intervals in the difference in the mean. This works
##' in two ways: either two pollutants are supplied in separate
##' columns e.g. \code{pollutant = c("no2", "o3")}, or there are two
##' unique values of \code{group}. The difference is calculated as the
##' second pollutant minus the first and is labelled as
##' such. Considering differences in this way can provide many useful
##' insights and is particularly useful for model evaluation when
##' information is needed about where a model differs from
##' observations by many different time scales. The manual contains
##' various examples of using \code{difference = TRUE}.
##'
##' Note also that the \code{timeVariation} function works well on a
##' subset of data and in conjunction with other plots. For example, a
##' \code{\link{polarPlot}} may highlight an interesting feature for a
##' particular wind speed/direction range. By filtering for those
##' conditions \code{timeVariation} can help determine whether the
##' temporal variation of that feature differs from other features ---
##' and help with source identification.
##'
##' In addition, \code{timeVariation} will work well with other variables if
##' available. Examples include meteorological and traffic flow data.
##'
##' @param mydata A data frame of hourly (or higher temporal resolution data).
##'   Must include a \code{date} field and at least one variable to plot.
##' @param pollutant Name of variable to plot. Two or more pollutants can be
##'   plotted, in which case a form like \code{pollutant = c("nox", "co")}
##'   should be used.
##' @param local.time Should the results be calculated in local time? The
##'   default is \code{FALSE}. Emissions activity tends to occur at local time
##'   e.g. rush hour is at 8 am every day. When the clocks go forward in
##'   spring, the emissions are effectively released into the atmosphere at BST
##'   - 1 hour during the summer. When plotting diurnal profiles, this has the
##'   effect of "smearing-out" the concentrations. Sometimes, a better approach
##'   is to express time as local time, which here is defined as BST (British
##'   Summer Time). This correction tends to produce better-defined diurnal
##'   profiles of concentration (or other variables) and allows a better
##'   comparison to be made with emissions/activity data. If set to
##'   \code{FALSE} then GMT is used.
##' @param normalise Should variables be normalised? The default is
##'   \code{FALSE}. If \code{TRUE} then the variable(s) are divided by their
##'   mean values. This helps to compare the shape of the diurnal trends for
##'   variables on very different scales.
##' @param xlab x-axis label; one for each sub-plot.
##' @param name.pol Names to be given to the pollutant(s). This is useful if
##'   you want to give a fuller description of the variables, maybe also
##'   including subscripts etc.
##' @param type \code{type} determines how the data are split i.e. conditioned,
##'   and then plotted. The default is will produce a single plot using the
##'   entire data. Type can be one of the built-in types as detailed in
##'   \code{cutData} e.g. "season", "year", "weekday" and so on. For example,
##'   \code{type = "season"} will produce four plots --- one for each season.
##'
##' It is also possible to choose \code{type} as another variable in the data
##'   frame. If that variable is numeric, then the data will be split into four
##'   quantiles (if possible) and labelled accordingly. If type is an existing
##'   character or factor variable, then those categories/levels will be used
##'   directly. This offers great flexibility for understanding the variation
##'   of different variables and how they depend on one another.
##'
##' Only one \code{type} is allowed in\code{timeVariation}.
##' @param group This sets the grouping variable to be used. For example, if a
##'   data frame had a column \code{site} setting \code{group = "site"} will
##'   plot all sites together in each panel. See examples below.
##' @param difference If two pollutants are chosen then setting
##' \code{difference = TRUE} will also plot the difference in means
##' between the two variables as \code{pollutant[2] -
##' pollutant[1]}. Bootstrap 95\% confidence intervals of the
##' difference in means are also calculated. A horizontal dashed line
##' is shown at y = 0.
##' @param B Number of bootstrap replicates to use. Can be useful to
##' reduce this value when there are a large number of observations
##' available to increase the speed of the calculations without
##' affecting the 95% confidence interval calculations by much.
##' @param ci Should confidence intervals be shown? The default is \code{TRUE}.
##'   Setting this to \code{FALSE} can be useful if multiple pollutants are
##'   chosen where over-lapping confidence intervals can over complicate plots.
##' @param cols Colours to be used for plotting. Options include "default",
##'   "increment", "heat", "spectral", "hue" (default) and user defined (see
##'   manual for more details).
##' @param key By default \code{timeVariation} produces four plots on one page.
##'   While it is useful to see these plots together, it is sometimes necessary
##'   just to use one for a report. If \code{key} is \code{TRUE}, a key is
##'   added to all plots allowing the extraction of a single plot \emph{with}
##'   key. See below for an example.
##' @param key.columns Number of columns to be used in the key. With many
##'   pollutants a single column can make to key too wide. The user can thus
##'   choose to use several columns by setting \code{columns} to be less than
##'   the number of pollutants.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the `2' in NO2.
##' @param alpha The alpha transparency used for plotting confidence intervals.
##'   0 is fully transparent and 1 is opaque. The default is 0.4
##' @param ... Other graphical parameters passed onto \code{lattice:xyplot}
##'   and \code{cutData}. For example, in the case of \code{cutData} the option
##'   \code{hemisphere = "southern"}.
##' @export
##' @return As well as generating the plot itself, \code{timeVariation} also
##'   returns an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data used to make the four components of the plot (or
##'   subplots); and \code{plot}, the associated subplots.  If retained, e.g.
##'   using \code{output <- timeVariation(mydata, "nox")}, this output can be
##'   used to recover the data, reproduce or rework the original plot or
##'   undertake further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}. See
##'   \code{\link{openair.generics}} for further details.
##'
##' The four components of timeVariation are: \code{day.hour}, \code{hour},
##'   \code{day} and \code{month}. Associated data.frames can be extracted
##'   directly using the \code{subset} option, e.g. as in \code{plot(object,
##'   subset = "day.hour")}, \code{summary(output, subset = "hour")}, etc, for
##'   \code{output <- timeVariation(mydata, "nox")}
##' @author David Carslaw
##' @seealso \code{\link{polarPlot}}, \code{\link{linearRelation}}
##' @keywords methods
##' @examples
##'
##'
##' # basic use
##' timeVariation(mydata, pollutant = "nox")
##'
##' # for a subset of conditions
##' timeVariation(subset(mydata, ws > 3 & wd > 100 & wd < 270),
##' pollutant = "pm10", ylab = "pm10 (ug/m3)")
##'
##' # multiple pollutants with concentrations normalised
##' timeVariation(mydata, pollutant = c("nox", "co"), normalise = TRUE)
##'
##' # show BST/GMT variation (see ?cutData for more details)
##' # the NOx plot shows the profiles are very similar when expressed in
##' # local time, showing that the profile is dominated by a local source
##' # that varies by local time and not by GMT i.e. road vehicle emissions
##'
##' timeVariation(mydata, pollutant = "nox", type = "gmtbst")
##'
##' ## In this case it is better to group the results for clarity:
##' timeVariation(mydata, pollutant = "nox", group = "gmtbst")
##'
##' # By contrast, a variable such as wind speed shows a clear shift when
##' #  expressed in local time. These two plots can help show whether the
##' #  variation is dominated by man-made influences or natural processes
##'
##' \dontrun{timeVariation(mydata, pollutant = "ws", group = "gmtbst")}
##'
##' ## It is also possible to plot several variables and set type. For
##' ## example, consider the NOx and NO2 split by levels of O3:
##'
##' \dontrun{timeVariation(mydata, pollutant = c("nox", "no2"), type = "o3", normalise = TRUE)}
##'
##' ## difference in concentrations
##' timeVariation(mydata, poll= c("pm25", "pm10"), difference = TRUE)
##'
##' # It is also useful to consider how concentrations vary by
##' # considering two different periods e.g. in intervention
##' # analysis. In the following plot NO2 has clearly increased but much
##' # less so at weekends - perhaps suggesting vehicles other than cars
##' # are important because flows of cars are approximately invariant by
##' # day of the week
##'
##' mydata <- splitByDate(mydata, dates= "1/1/2003", labels = c("before Jan. 2003", "After Jan. 2003"))
##' timeVariation(mydata, pollutant = "no2", group = "split.by", difference = TRUE)
##'
##' ## sub plots can be extracted from the openair object
##' myplot <- timeVariation(mydata, pollutant = "no2")
##' plot(myplot, subset = "day.hour") # top weekday and plot
##'
##' ## individual plots
##' ## plot(myplot, subset="day.hour") for the weekday and hours subplot (top)
##' ## plot(myplot, subset="hour") for the diurnal plot
##' ## plot(myplot, subset="day") for the weekday plot
##' ## plot(myplot, subset="month") for the monthly plot
##'
##' ## numerical results (mean, lower/upper uncertainties)
##' ## results(myplot, subset = "day.hour") # the weekday and hour data set
##' ## summary(myplot, subset = "hour") #summary of hour data set
##' ## head(myplot, subset = "day") #head/top of day data set
##' ## tail(myplot, subset = "month") #tail/top of month data set
##'
##'
timeVariation <- function(mydata, pollutant = "nox", local.time = FALSE, normalise = FALSE,
                          xlab = c("hour", "hour", "month", "weekday"),
                          name.pol = pollutant, type = "default", group = NULL,
                          difference = FALSE, B = 500, ci = TRUE, cols = "hue",
                          key = NULL, key.columns = 1, auto.text = TRUE,
                          alpha = 0.4, ...)   {


    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
                                        #strip only
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
    }


    ## extra.args setup
    extra.args <- list(...)

    ## label controls
    ## xlab handled in formals and code because unique
    extra.args$ylab <- if("ylab" %in% names(extra.args))
        quickText(extra.args$ylab, auto.text) else
    quickText(paste(pollutant, collapse=", "), auto.text)

    extra.args$main <- if("main" %in% names(extra.args))
        quickText(extra.args$main, auto.text) else quickText("", auto.text)

    extra.args$lwd <- if("lwd" %in% names(extra.args)) extra.args$lwd else 2

    ylim.handler <- if("ylim" %in% names(extra.args))
        FALSE else TRUE

    vars <- c("date", pollutant)

    ##  various check to make sure all teh data are available ######################################
    if (!missing(group) & length(pollutant) > 1) {
        stop("Can only have one pollutant with a grouping variable, or several pollutants and no grouping variable.")}

    ## only one type for now
    if (length(type) > 1) stop("Can only have one type for timeVariation.")

    if (type %in% pollutant) stop("Cannot have type the same as a pollutant name.")

    if (!missing(group)) {
        if (group %in% pollutant) stop("Cannot have group the same as a pollutant name.")
    }

    ## if differences between two pollutants are calculated
    if (difference) {
        if (missing(group)) {

            if (length(pollutant) != 2)
                stop("Need to specify two pollutants to calculate their difference.")
        }

        if (!missing(group)) {
            if (length(unique(mydata[ , group])) != 2)
                stop("Need to specify two pollutants to calculate their difference.")
        }
    }

    ## #############################################################################################

    ## check to see if type = "variable" (word used in code, so change)
    if (type == "variable") {
        mydata <- rename(mydata, c(variable = "tempVar"))
        type <- "tempVar"
    }

    ## if group is present, need to add that list of variables
    if (!missing(group)){

        if (group %in%  openair:::dateTypes) {
            vars <- unique(c(vars, "date"))
        } else {
            vars <- unique(c(vars, group))
        }
    }

    ## data checks
    mydata <- openair:::checkPrep(mydata, vars, type, remove.calm = FALSE)
    if (!missing(group))  mydata <- cutData(mydata, group, ...)
    mydata <- cutData(mydata, type, ...)

    mydata <- na.omit(mydata)

    ## title for overall and individual plots
    overall.main <- extra.args$main
    extra.args$main <- ""

    if (local.time) attr(mydata$date, "tzone") <- "Europe/London"

    ## labels for pollutants, can be user-defined, special handling when difference = TRUE
    poll.orig <- pollutant
    if (difference && missing(group)) pollutant <- c(pollutant, paste(pollutant[2], "-", pollutant[1]))

    if (missing(name.pol)) mylab <- sapply(seq_along(pollutant), function(x)
                                           quickText(pollutant[x], auto.text))


  #  if (difference && !missing(name.pol)) name.pol <- c(name.pol, paste(pollutant[2], "-", pollutant[1]))

    if (!missing(name.pol)) mylab <- sapply(seq_along(name.pol), function(x)
                                            quickText(name.pol[x], auto.text))

    if (missing(group)) {
        mydata <- melt(mydata, measure.vars = poll.orig)
        mydata$variable <- factor(mydata$variable)  ## drop unused factor levels

    } else {
        names(mydata)[2:3] <- c("value", "variable")
        mydata$variable <- factor(mydata$variable)  ## drop unused factor levels
        the.names <- levels(mydata[ , "variable"])
        if (difference) the.names <- c(the.names, paste(levels(mydata$variable)[2], "-",
                                                        levels(mydata$variable)[1]))
        mylab <-  sapply(the.names, function(x) quickText(x, auto.text))
    }


    ## function to normalise
    divide.by.mean <- function(x) {
        Mean <- mean(x$Mean, na.rm = TRUE)
        x$Mean <- x$Mean / Mean
        x$Lower <- x$Lower / Mean
        x$Upper <- x$Upper / Mean
        x
    }

    if (normalise) extra.args$ylab <- "normalised level"

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

        ## if no CI information, just return
        if (all(is.na(x[, c("Lower", "Upper")]))) {
            lims <- NULL
            return(lims)
        }

        if (ci) {
            lims <- range(c(x$Lower, x$Upper), na.rm = TRUE)
            inc <- 0.04 * abs(lims[2] - lims[1])
            lims <- c(lims[1] - inc, lims[2] + inc)
        } else {
            lims <- range(c(x$Mean, x$Mean), na.rm = TRUE)
            inc <- 0.04 * abs(lims[2] - lims[1])
            lims <- c(lims[1] - inc, lims[2] + inc)
        }
        lims
    }

    npol <- length(unique(mydata$variable)) ## number of pollutants

    if (difference) {
        npol <- 3 ## 3 pollutants if difference considered
        if (missing(group)) poll1 <- pollutant[1]; poll2 <- pollutant[2]
        if (!missing(group)) poll1 <- levels(mydata$variable)[1]; poll2 <- levels(mydata$variable)[2]
    }

    ## number of columns for key
    if (missing(key.columns)) key.columns <- npol

    myColors <- openColours(cols, npol)

    ## for individual plot keys - useful if only one of the plots is extracted after printing
    if (!is.null(key)) {
        key <- list(rectangles = list(col = myColors[1:npol], border = NA),
                    text = list(lab = mylab),  space = "bottom", columns = key.columns)

        extra.args$main <- overall.main
    }

    ## hour ############################################################################

    if (difference) {
        data.hour <- errorDiff(mydata, vars = "hour", type = type, poll1 = poll1,
                               poll2 = poll2, B = B)
    } else {
        data.hour <- calc.wd(mydata, vars = "hour", pollutant, type, B = B)
    }


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

    ## ylim hander
    if(ylim.handler)
        extra.args$ylim <- rng(data.hour)

    ## plot
    xyplot.args <- list(x = myform,  data = data.hour, groups = data.hour$variable,
                        as.table = TRUE,
                        xlab = xlab[2],
                        xlim = c(0, 23),
                        strip = strip,
                        par.strip.text = list(cex = 0.8),
                        key = key,
                        scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                        par.settings = simpleTheme(col = myColors),
                        panel =  panel.superpose,
                        panel.groups = function(x, y, col.line, type, group.number, subscripts,...) {
                            if (group.number == 1) {
                                panel.grid(-1, 0)
                                panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                            }

                            if (difference) panel.abline(h = 0, lty = 5)

                            panel.xyplot(x, y, type = "l", col.line = myColors[group.number],...)

                            if (ci) {poly.na(x, data.hour$Lower[subscripts], x,
                                             data.hour$Upper[subscripts], group.number)}

                        })

    ## reset for extra.args
    xyplot.args <- openair:::listUpdate(xyplot.args, extra.args)

    ## plot
    hour <- do.call(xyplot, xyplot.args)

    ## weekday ############################################################################

    if (difference) {
        data.weekday <- errorDiff(mydata, vars = "weekday", type = type, poll1 =poll1,
                                  poll2 = poll2, B = B)
    } else {
        data.weekday <- calc.wd(mydata, vars = "weekday", pollutant, type, B = B)
    }

    if (normalise) data.weekday <-  ddply(data.weekday, .(variable), divide.by.mean)

    data.weekday$weekday <- ordered(data.weekday$weekday, levels = format(ISOdate(2000, 1, 3:9), "%A"))

    data.weekday$weekday <- as.numeric(as.factor(data.weekday$weekday))

    if (is.null(xlab[4]) | is.na(xlab[4])) xlab[4] <- "weekday"

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("Mean ~ weekday | ", temp, sep = ""))

    ## ylim hander
    if (ylim.handler)
        extra.args$ylim <- rng(data.weekday)

    ## plot
    xyplot.args <- list(x = myform,  data = data.weekday, groups = data.weekday$variable,
                        as.table = TRUE,
                        par.settings = simpleTheme(col = myColors, pch = 16),
                        scales = list(x = list(at = 1:7, labels = format(ISOdate(2000, 1, 3:9), "%a"))),
                        xlab = xlab[4],
                        strip = strip,
                        par.strip.text = list(cex = 0.8),
                        key = key,
                        panel =  panel.superpose,
                        panel.groups = function(x, y, col.line, type, group.number, subscripts,...) {
                            if (group.number == 1) {
                                panel.grid(-1, 0)
                                panel.abline(v = 1:7, col = "grey85")
                            }

                            if (difference) panel.abline(h = 0, lty = 5)

                            panel.xyplot(x, y, type = "l", col.line = myColors[group.number],...)
                            panel.xyplot(x, y, type = "p", col.point = myColors[group.number],...)

                            if (ci) {panel.rect(x - 0.3, data.weekday$Lower[subscripts], x + 0.3,
                                                data.weekday$Upper[subscripts],
                                                fill = myColors[group.number],
                                                border = NA, alpha = alpha)}
                        })

    ## reset for extra.args
    xyplot.args <- openair:::listUpdate(xyplot.args, extra.args)

    ## plot
    day <- do.call(xyplot, xyplot.args)

    ## month ############################################################################

    if (difference) {
        data.month <- errorDiff(mydata, vars = "month", type = type, poll1 = poll1,
                                poll2 = poll2, B = B)
    } else {
        data.month <- calc.wd(mydata, vars = "month", pollutant, type, B = B)
    }

    if (normalise) data.month <-  ddply(data.month, .(variable), divide.by.mean)

                                        #note: 3 not 4
    if (is.null(xlab[3]) | is.na(xlab[3])) xlab[3] <- "month"

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("Mean ~ month | ", temp, sep = ""))

    ## ylim hander
    if (ylim.handler)
        extra.args$ylim <- rng(data.month)

    ## plot
    xyplot.args <- list(x = myform,  data = data.month, groups = data.month$variable,
                        as.table = TRUE,
                        xlab = xlab[3],
                        xlim = c(0.5, 12.5),
                        key = key,
                        strip = strip,
                        par.strip.text = list(cex = 0.8),
                        par.settings = simpleTheme(col = myColors, pch = 16),
                        scales = list(x = list(at = 1:12, labels = substr(format(seq(as.Date("2000-01-01"),
                                                          as.Date("2000-12-31"), "month"), "%B"), 1, 1))),
                        panel =  panel.superpose,
                        panel.groups = function(x, y, col.line, type, group.number, subscripts,...) {
                            if (group.number == 1) {
                                panel.grid(-1, 0)
                                panel.abline(v = 1:12, col = "grey85")
                            }
                            if (difference) panel.abline(h = 0, lty = 5)

                            panel.xyplot(x, y, type = "p", col.point = myColors[group.number],...)

                            panel.xyplot(x, y, type = "l", col.line = myColors[group.number],...)


                            if (ci) {panel.rect(x - 0.3, data.month$Lower[subscripts], x + 0.3,
                                                data.month$Upper[subscripts],
                                                fill = myColors[group.number],
                                                border = NA, alpha = alpha)}
                        })

    ## reset for extra.args
    xyplot.args <- openair:::listUpdate(xyplot.args, extra.args)

    ## plot
    month <- do.call(xyplot, xyplot.args)

    ## day and hour ############################################################################

    if (difference) {
        data.day.hour <- errorDiff(mydata, vars = "day.hour", type = type, poll1 = poll1,
                                   poll2 = poll2, B = B)
    } else {
        data.day.hour <- calc.wd(mydata, vars = "day.hour", pollutant, type, B = B)
    }

    if (normalise) data.day.hour <-  ddply(data.day.hour, .(variable), divide.by.mean)

    ids <- which(is.na(data.day.hour$Lower)) ## missing Lower ci, set to mean

    data.day.hour$Lower[ids] <-  data.day.hour$Mean[ids]
    ids <- which(is.na(data.day.hour$Upper)) ## missing Upper ci, set to mean
    data.day.hour$Upper[ids] <-  data.day.hour$Mean[ids]

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

    ## ylim hander
    if(ylim.handler)
        extra.args$ylim <- rng(data.day.hour)

    ## plot
    xyplot.args <- list(x = myform,  data = data.day.hour, groups = data.day.hour$variable,
                        as.table = TRUE,
                        xlim = c(0, 23),
                        xlab = xlab[1],
                        layout = layout,
                        par.settings = simpleTheme(col = myColors),
                        scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                        key = key,
                        strip = strip,
                        strip.left = strip.left,
                        par.strip.text = list(cex = 0.8),
                        panel =  panel.superpose,
                        panel.groups = function(x, y, col.line, type, group.number,
                        subscripts,...) {
                            ## add grid lines once (otherwise they overwrite the data)
                            if (group.number == 1) {
                                panel.grid(-1, 0)
                                panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                            }

                            if (difference) panel.abline(h = 0, lty = 5)

                            panel.xyplot(x, y, type = "l", col.line = myColors[group.number],...)

                            if (ci) {poly.na(x, data.day.hour$Lower[subscripts], x,
                                             data.day.hour$Upper[subscripts], group.number)}
                        })

    ## reset for extra.args
    xyplot.args <- openair:::listUpdate(xyplot.args, extra.args)

    ## plot
    day.hour <- do.call(xyplot, xyplot.args)

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

    ind.plot = function(x, ...){
        plot(update(x, key = list(
                       rectangles = list(col = myColors[1:npol], border = NA),
                       text = list(lab = mylab), space = "top", columns = key.columns)
                    ), ...)
    }

    main.plot()
    output <- list(plot = list(day.hour, hour, day, month, subsets = subsets),
                   data = list(data.day.hour, data.hour, data.weekday, data.month, subsets = subsets),
                   call = match.call(),
                   main.plot = main.plot, ind.plot = ind.plot
                   )
    names(output$data)[1:4] <- subsets
    names(output$plot)[1:4] <- subsets
    class(output) <- "openair"

                                        #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)

    invisible(output)
}

calc.wd <- function(mydata, vars = "day.hour", pollutant, type, B = B) {

    summary.values <- function(mydata, vars, FUN, type, B = B) {

        if (vars == "hour")  myform <- formula(paste("value ~ variable + hour +", type))

        if (vars == "day.hour")  myform <- formula(paste("value ~ variable + weekday + hour +", type))

        if (vars == "weekday") myform <- formula(paste("value ~ variable + weekday +", type))

        if (vars == "month") myform <- formula(paste("value ~ variable + month +", type))

        mydata <- aggregate(myform, data = mydata, FUN, B = B)
        mydata
    }

    ## function to calculate statistics dealing with wd properly
    if (any(!pollutant %in% "wd")) {
        data1 <- subset(mydata, variable != "wd")
        data1 <-  summary.values(data1, vars, openair:::bootMean, type, B = B)
        data1 <- data.frame(subset(data1, select = -value), data1$value)
    }

    if ("wd" %in% pollutant) {
        data2 <-  subset(mydata, variable == "wd")
        data2 <-  summary.values(data2, vars, openair:::bootMean, type, B = B)
        data2 <- data.frame(subset(data2, select = -value), data2$value)
    }

    if (length(pollutant) > 1 & "wd" %in% pollutant) data2 <- rbind.fill(data1, data2)

    if (!"wd" %in% pollutant) data2 <- data1

    if (length(pollutant) == 1 & "wd" %in% pollutant) data2 <- data2

    data2
}

wd.smean.normal <- function(wd, B = B) {
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

    conf.int <- openair:::bootMean(wd.diff, B = B)
    Lower <- conf.int[2]
    names(Lower) <- NULL

    Upper <- conf.int[3]
    names(Upper) <- NULL
    diff.wd <- (Upper - Lower) / 2

    c(Mean = Mean, Lower = Mean - diff.wd, Upper = Mean + diff.wd)
}

errorDiff <- function(mydata, vars = "day.hour", poll1, poll2, type, B = B)
{
    ## bootstrap mean difference confidence intervals
    ## rearrange data
    mydata <- dcast(mydata, ... ~ variable)
    if (vars == "hour") splits <- c("hour", type)
    if (vars == "day.hour") splits <- c("hour", "weekday", type)
    if (vars == "weekday") splits <- c("weekday", type)
    if (vars == "month") splits <- c("month", type)

    res <- ddply(mydata, splits, openair:::bootMeanDiff, x = poll1, y = poll2, B = B)

    res
}




