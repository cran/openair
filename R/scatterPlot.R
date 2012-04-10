##' Flexible scatter plots
##'
##' Scatter plots with conditioning and three main approaches: conventional
##' scatterPlot, hexagonal binning and kernel density estimates. The former
##' also has options for fitting smooth fits and linear models with
##' uncertainties shown.
##'
##' The \code{scatterPlot} is the basic function for plotting scatterPlots in
##' flexible ways in \code{openair}. It is flexible enough to consider lots of
##' conditioning variables and takes care of fitting smooth or linear
##' relationships to the data.
##'
##' There are four main ways of plotting the relationship between two
##' variables, which are set using the \code{method} option. The default
##' \code{"scatter"} will plot a conventional scatterPlot. In cases where there
##' are lots of data and over-plotting becomes a problem, then \code{method =
##' "hexbin"} or \code{method = "density"} can be useful. The former requires
##' the \code{hexbin} package to be installed.
##'
##' There is also a \code{method = "level"} which will bin the \code{x} and
##' \code{y} data according to the intervals set for \code{x.inc} and
##' \code{y.inc} and colour the bins according to levels of a third variable,
##' \code{z}. Sometimes however, a far better understanding of the relationship
##' between three variables (\code{x}, \code{y} and \code{z}) is gained by
##' fitting a smooth surface through the data. See examples below.
##'
##' A smooth fit is shown if \code{smooth = TRUE} which can help show the
##' overall form of the data e.g. whether the relationship appears to be linear
##' or not. Also, a linear fit can be shown using \code{linear = TRUE} as an
##' option.
##'
##' The user has fine control over the choice of colours and symbol type used.
##'
##' Another way of reducing the number of points used in the plots which can
##' sometimes be useful is to aggregate the data. For example, hourly data can
##' be aggregated to daily data. See \code{timePlot} for examples here.
##'
##' By default plots are shown with a colour key at the bottom and in the case
##' of conditioning, strips on the top of each plot. Sometimes this may be
##' overkill and the user can opt to remove the key and/or the strip by setting
##' \code{key} and/or \code{strip} to \code{FALSE}. One reason to do this is to
##' maximise the plotting area and therefore the information shown.
##' @param mydata A data frame containing at least two numeric variables to
##' plot.
##' @param x Name of the x-variable to plot. Note that x can be a date field or
##'   a factor. For example, \code{x} can be one of the \code{openair} built in
##'   types such as \code{"year"} or \code{"season"}.
##' @param y Name of the numeric y-variable to plot.
##' @param z Name of the numeric z-variable to plot for \code{method =
##'   "scatter"} or \code{method = "level"}. Note that for \code{method =
##'   "scatter"} points will be coloured according to a continuous colour
##'   scale, whereas for \code{method = "level"} the surface is coloured.
##' @param method Methods include \code{"scatter"} (conventional scatter plot),
##'   \code{"hexbin"} (hexagonal binning using the \code{hexbin} package).
##'   \code{level} for a binned or smooth surface plot and \code{"density"} (2D
##'   kernel density estimates).
##' @param group The grouping variable to use, if any. Setting this to a
##'   variable in the data frame has the effect of plotting several series in
##'   the same panel using different symbols/colours etc. If set to a variable
##'   that is a character or factor, those categories or factor levels will be
##'   used directly. If set to a numeric variable, it will split that variable
##'   in to quantiles.
##' @param avg.time This defines the time period to average to. Can be "sec",
##'   "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year". For
##'   much increased flexibility a number can precede these options followed by
##'   a space. For example, a timeAverage of 2 months would be \code{period =
##'   "2 month"}. See function \code{timeAverage} for further details on this.
##'   This option se useful as one method by which the number of points plotted
##'   is reduced i.e. by choosing a longer averaging time.
##' @param data.thresh The data capture threshold to use (%) when aggregating
##'   the data using \code{avg.time}. A value of zero means that all available
##'   data will be used in a particular period regardless if of the number of
##'   values available. Conversely, a value of 100 will mean that all data will
##'   need to be present for the average to be calculated, else it is recorded
##'   as \code{NA}. Not used if \code{avg.time = "default"}.
##' @param statistic The statistic to apply when aggregating the data; default
##'   is the mean. Can be one of "mean", "max", "min", "median", "frequency",
##'   "sd", "percentile". Note that "sd" is the standard deviation and
##'   "frequency" is the number (frequency) of valid records in the period.
##'   "percentile" is the percentile level (%) between 0-100, which can be set
##'   using the "percentile" option - see below. Not used if \code{avg.time =
##'   "default"}.
##' @param percentile The percentile level in % used when \code{statistic =
##'   "percentile"} and when aggregating the data with \code{avg.time}. The
##'   default is 95. Not used if \code{avg.time = "default"}.
##' @param type \code{type} determines how the data are split
##' i.e. conditioned, and then plotted. The default is will produce a
##' single plot using the entire data. Type can be one of the built-in
##' types as detailed in \code{cutData} e.g. "season", "year",
##' "weekday" and so on. For example, \code{type = "season"} will
##' produce four plots --- one for each season. \cr\cr It is also
##' possible to choose \code{type} as another variable in the data
##' frame. If that variable is numeric, then the data will be split
##' into four quantiles (if possible) and labelled accordingly. If
##' type is an existing character or factor variable, then those
##' categories/levels will be used directly. This offers great
##' flexibility for understanding the variation of different variables
##' and how they depend on one another.  \cr\cr Type can be up length
##' two e.g. \code{type = c("season", "weekday")} will produce a 2x2
##' plot split by season and day of the week. Note, when two types are
##' provided the first forms the columns and the second the rows.
##' @param smooth A smooth line is fitted to the data if \code{TRUE};
##'   optionally with 95% confidence intervals shown. For \code{method =
##'   "level"} a smooth surface will be fitted to binned data.
##' @param spline A smooth spline is fitted to the data if \code{TRUE}. This is
##'   particularly useful when there are fewer data points or when a connection
##'   line between a sequence of points is required.
##' @param linear A linear model is fitted to the data if \code{TRUE};
##'   optionally with 95% confidence intervals shown. The equation of the line
##'   and R2 value is also shown.
##' @param ci Should the confidence intervals for the smooth/linear fit be
##'   shown?
##' @param mod.line If \code{TRUE} three lines are added to the scatter plot to
##'   help inform model evaluation. The 1:1 line is solid and the 1:0.5 and 1:2
##'   lines are dashed. Together these lines help show how close a group of
##'   points are to a 1:1 relationship and also show the points that are within
##'   a factor of two (FAC2). Also, for \code{method = "scatter"} (the default)
##'   the scales are made to be isometric. In time, more comprehensive model
##'   evaluation statistics will be considered.
##' @param cols Colours to be used for plotting. Options include "default",
##'   "increment", "heat", "spectral", "hue", "brewer1" and user defined (see
##'   manual for more details). The same line colour can be set for all
##'   pollutant e.g. \code{cols = "black"}.
##' @param plot.type \code{lattice} plot type. Can be "p" (points --- default),
##'   "l" (lines) or "b" (lines and points).
##' @param key Should a key be drawn? The default is \code{TRUE}.
##' @param key.title The title of the key (if used).
##' @param key.columns Number of columns to be used in the key. With many
##'   pollutants a single column can make to key too wide. The user can thus
##'   choose to use several columns by setting \code{columns} to be less than
##'   the number of pollutants.
##' @param key.position  Location where the scale key is to plotted.  Allowed
##'   arguments currently include \code{"top"}, \code{"right"}, \code{"bottom"}
##'   and \code{"left"}
##' @param strip Should a strip be drawn? The default is \code{TRUE}.
##' @param log.x Should the x-axis appear on a log scale? The default is
##'   \code{FALSE}. If \code{TRUE} a well-formatted log10 scale is used. This
##'   can be useful for checking linearity once logged.
##' @param log.y Should the y-axis appear on a log scale? The default is
##'   \code{FALSE}. If \code{TRUE} a well-formatted log10 scale is used. This
##'   can be useful for checking linearity once logged.
##' @param x.inc The x-interval to be used for binning data when \code{method =
##'   "level"}.
##' @param y.inc The y-interval to be used for binning data when \code{method =
##'   "level"}.
##' @param y.relation This determines how the y-axis scale is plotted. "same"
##'   ensures all panels use the same scale and "free" will use panel-specfic
##'   scales. The latter is a useful setting when plotting data with very
##'   different values.
##' @param x.relation This determines how the y-axis scale is plotted. "same"
##'   ensures all panels use the same scale and "free" will use panel-specfic
##'   scales. The latter is a useful setting when plotting data with very
##'   different values.
##' @param ref.x Add a vertical dashed reference line at this value.
##' @param ref.y Add a horizontal dashed reference line at this value.
##' @param k Smoothing parameter supplied to \code{gam} for fitting a smooth
##'   surface when \code{method = "level"}.
##' @param trans \code{trans} is used when \code{continuous = TRUE}. Often for
##'   a good colour scale with skewed data it is a good idea to "compress" the
##'   scale. If \code{TRUE} a square root transform is used, if \code{FALSE} a
##'   linear scale is used.
##' @param map Should a base map be drawn? This option is under development.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the `2' in NO2.
##' @param \dots Other graphical parameters are passed onto \code{cutData} and
##'   an appropriate \code{lattice} plot function (\code{xyplot}, \code{levelplot}
##'   or \code{hexbinplot} depending on \code{method}). For example,
##'   \code{scatterPlot} passes the option \code{hemisphere = "southern"} on to
##'   \code{cutData} to provide southern (rather than default northern) hemisphere
##'   handling of \code{type = "season"}. Similarly, for the default case
##'   \code{method = "scatter"} common axis and title labelling options (such
##'   as \code{xlab}, \code{ylab}, \code{main}) are passed to \code{xyplot} via
##'   \code{quickText} to handle routine formatting. Other common graphical
##'   parameters, e.g. \code{layout} for panel arrangement,
##'   \code{pch} for plot symbol and \code{lwd} and \code{lty}
##'   for line width and type, as also available (see examples below).
##' @export
##' @return As well as generating the plot itself, \code{scatterPlot} also
##'   returns an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- scatterPlot(mydata, "nox", "no2")}, this output can be
##'   used to recover the data, reproduce or rework the original plot or
##'   undertake further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}. See
##'   \code{\link{openair.generics}} for further details.
##' @author David Carslaw
##' @seealso \code{\link{linearRelation}}, \code{\link{timePlot}} and
##'   \code{\link{timeAverage}} for details on selecting averaging times and
##'   other statistics in a flexible way
##' @keywords methods
##' @examples
##'
##' # load openair data if not loaded already
##' data(mydata)
##'
##' # basic use, single pollutant
##'
##' scatterPlot(mydata, x = "nox", y = "no2")
##'
##' # scatterPlot by year
##' scatterPlot(mydata, x = "nox", y = "no2", type = "year")
##'
##' # scatterPlot by day of the week, removing key at bottom
##' scatterPlot(mydata, x = "nox", y = "no2", type = "weekday", key =
##' FALSE)
##'
##' # example of the use of continuous where colour is used to show
##' # different levels of a third (numeric) variable
##' # plot daily averages and choose a filled plot symbol (pch = 16)
##' # select only 2004
##' \dontrun{dat2004 <- selectByDate(mydata, year = 2004)
##' scatterPlot(dat2004, x = "nox", y = "no2", z = "co", avg.time = "day", pch = 16)}
##'
##' # show linear fit, by year
##' \dontrun{scatterPlot(mydata, x = "nox", y = "no2", type = "year", smooth =
##' FALSE, linear = TRUE)}
##'
##' # do the same, but for daily means...
##' \dontrun{scatterPlot(mydata, x = "nox", y = "no2", type = "year", smooth =
##' FALSE, linear = TRUE, avg.time = "day")}
##'
##' # log scales
##' \dontrun{scatterPlot(mydata, x = "nox", y = "no2", type = "year", smooth =
##' FALSE, linear = TRUE, avg.time = "day", log.x = TRUE, log.y = TRUE)}
##'
##' # also works with the x-axis in date format (alternative to timePlot)
##' \dontrun{scatterPlot(mydata, x = "date", y = "no2", avg.time = "month",
##' key = FALSE)}
##'
##' ## multiple types and grouping variable and continuous colour scale
##' \dontrun{scatterPlot(mydata, x = "nox", y = "no2", z = "o3", type = c("season", "weekend"))}
##'
##' # use hexagonal binning
##' \dontrun{
##' library(hexbin)
##' # basic use, single pollutant
##' scatterPlot(mydata, x = "nox", y = "no2", method = "hexbin")
##'
##' # scatterPlot by year
##' scatterPlot(mydata, x = "nox", y = "no2", type = "year", method =
##' "hexbin")
##'
##' ## bin data and plot it - can see how for high NO2, O3 is also high
##' \dontrun{
##' scatterPlot(mydata, x = "nox", y = "no2", z = "o3", method = "level", x.inc = 10, y.inc = 2)
##' }
##'
##' ## fit surface for clearer view of relationship - clear effect of
##' ## increased O3
##' \dontrun{
##' scatterPlot(mydata, x = "nox", y = "no2", z = "o3", method = "level",
##' x.inc = 10, y.inc = 2, smooth = TRUE)
##' }
##' }
##'
##'
scatterPlot <- function(mydata,
                        x = "nox",
                        y = "no2",
                        z = NA,
                        method = "scatter",
                        group = NA,
                        avg.time = "default",
                        data.thresh = 0,
                        statistic = "mean",
                        percentile = NA,
                        type = "default",
                        smooth = FALSE,
                        spline = FALSE,
                        linear = FALSE,
                        ci = TRUE,
                        mod.line = FALSE,
                        cols = "hue",
                        plot.type = "p",
                        key = TRUE,
                        key.title = group,
                        key.columns = 1,
                        key.position = "right",
                        strip = TRUE,
                        log.x = FALSE,
                        log.y = FALSE,
                        x.inc = 10,
                        y.inc = 10,
                        y.relation = "same",
                        x.relation = "same",
                        ref.x = NULL,
                        ref.y = NULL,
                        k = 100,
                        trans = TRUE,
                        map = FALSE,
                        auto.text = TRUE, ...)   {

    ## basic function to plot single/multiple time series in flexible waysproduce scatterPlot
    ## Author: David Carslaw 27 Jan. 10
    ## method = scatter/hexbin/kernel

    x.nam <- x ## names of pollutants for linear model equation
    y.nam <- y
    thekey <- key

    ## reset strip color on exit
    current.strip <- trellis.par.get("strip.background")
    on.exit(trellis.par.set("strip.background", current.strip))

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
        ## other local colours
        method.col <- "greyscale"
    } else {
        method.col <- "default"
    }


    ##extra.args setup
    extra.args <- list(...)

    #label controls
    ##(main handled deferred to plot
    ##because of unique by-method handling)
    ##(pch handled specific at different points in code)

#note why is this different for pch, lty, lwd?

    extra.args$xlab <- if("xlab" %in% names(extra.args))
                           quickText(extra.args$xlab, auto.text) else quickText(x, auto.text)
    extra.args$ylab <- if("ylab" %in% names(extra.args))
                           quickText(extra.args$ylab, auto.text) else quickText(y, auto.text)
    extra.args$key.footer <- if("key.footer" %in% names(extra.args))
                             extra.args$key.footer else NULL
    if(!"lwd" %in% names(extra.args))
         extra.args$lwd <- 1
    if(!"lty" %in% names(extra.args))
         extra.args$lty <- 1
    if(!"layout" %in% names(extra.args))
         extra.args$layout <- NULL



    ## For Log scaling (adapted from lattice book ####################################
    if(log.x) nlog.x <- 10 else nlog.x <- FALSE
    if(log.y) nlog.y <- 10 else nlog.y <- FALSE
    yscale.components.log10 <- function(lim, ...) {
        ## adpated from lattice book
        ans <- yscale.components.default(lim = lim, ...)
        if(!log.y) return(ans)
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

    xscale.components.log10 <- function(lim, ...) {
        ## adpated from lattice book
        ans <- xscale.components.default(lim = lim, ...)
        if(!log.x) return(ans)
        tick.at <- logTicks(10^lim, loc = 1:9)
        tick.at.major <- logTicks(10^lim, loc = 1)
        major <- tick.at %in% tick.at.major
        ans$bottom$ticks$at <- log(tick.at, 10)
        ans$bottom$ticks$tck <- ifelse(major, 1.5, 0.75)
        ans$bottom$labels$at <- log(tick.at, 10)
        ans$bottom$labels$labels <- as.character(tick.at)
        ans$bottom$labels$labels[!major] <- ""
        ans$bottom$labels$check.overlap <- FALSE
        ans
    }


    logTicks <- function (lim, loc = c(1, 5)) {
        ii <- floor(log10(range(lim))) + c(-1, 2)
        main <- 10^(ii[1]:ii[2])
        r <- as.numeric(outer(loc, main, "*"))
        r[lim[1] <= r & r <= lim[2]]
    }


    ## average the data if necessary (default does nothing)
    ## note - need to average before cutting data up etc

    if (!is.na(group)) types <- c(type, group) else types <- type
    if (avg.time != "default")  {

        ## can't have a type or group that is date-based
        if (group %in% openair:::dateTypes | type  %in% openair:::dateTypes) stop ("Can't have an averging period set and a time-based 'type' or 'group'.")
        if ("default" %in% types) mydata$default <- 0 ## FIX ME

        mydata <- ddply(mydata, types, timeAverage, avg.time = avg.time,
                        statistic = statistic, percentile = percentile,
                        data.thresh = data.thresh)
    }

    ## the following makes sure all variables are present, which depends on 'group'
    ## and 'type'
    if (is.na(z) & method == "level") stop("Need to specify 'z' when using method = 'level'")

###########################################################################################
    if (any(type %in%  openair:::dateTypes) | !missing(avg.time)) {

        vars <- c("date", x, y)

    } else {

        vars <- c(x, y)
    }

    ## if group is present, need to add that list of variables unless it is a
    ## pre-defined date-based one
    if (!is.na(group)){

        if (group %in%  openair:::dateTypes | !missing(avg.time) |
            any(type %in% openair:::dateTypes)) {
            if (group %in%  openair:::dateTypes) {
                vars <- unique(c(vars, "date")) ## don't need group because it is
                ## defined by date
            } else {
                vars <- unique(c(vars, "date", group))
            }

        } else {
            vars <- unique(c(vars, group))
        }
    }

    if (!is.na(group)) if (group %in% type) stop ("Can't have 'group' also in 'type'.")

    ## decide if a trajectory plot is being drawn AND if a line is required rather
    ## than points
    traj <- FALSE
    if (all(c("date", "lat", "lon", "height", "pressure") %in% names(mydata)) &
        plot.type == "l") traj <- TRUE

    ## will need date so that trajectory groups can be coloured
    if (traj)  vars <- c(vars, "date")

    ## data checks

    if (!is.na(z)) vars <- c(vars, z)
    mydata <- openair:::checkPrep(mydata, vars, type)

    ## remove missing data except for time series where we want to show gaps
    ## this also removes missing factors
    if (class(mydata[ , x])[1] != "Date" | !"POSIXt" %in% class(mydata[ , x])) {
        mydata <- na.omit(mydata)
    }

    ## if x is a factor/character, then rotate axis labels for clearer labels
    x.rot <- 0
    if ("factor" %in% class(mydata[, x]) | "character"  %in% class(mydata[, x])) {
        x.rot <- 90
        mydata[, x] <- factor(mydata[, x])
    }

    ## continuous colors ##########################################################################################

    if (!is.na(z) & method == "scatter") {
        if (z %in% openair:::dateTypes) stop("Colour coding requires 'z' to be continuous numeric variable'")

        ## check to see if type is numeric/integer
        if (class(mydata[, z]) %in% c("integer", "numeric") == FALSE) stop(paste("Continuous colour coding requires ", z , " to be numeric", sep = ""))

        ## don't need a key with this
        key <- NULL

        ## colour scale transform
        if (trans) thePower <- 2 else thePower <- 1

        mydata <- cutData(mydata, type, ...)

        ## use square root transform for key to help highlight typical dustributions
        mydata[ , z] <- mydata[ , z] ^ (1 / thePower)

        if (missing(cols)) cols <- "default" ## better default colours for this
        thecol <- openColours(cols, 100)[cut(mydata[, z], 100, label = FALSE)]
        mydata$col <- thecol

        breaks <- unique(c(0, pretty(mydata[ , z], 100)))
        br <- pretty((mydata[ , z] ^ thePower), n = 10)  ## breaks for scale

        min.col <- min(mydata[, z], na.rm = TRUE)
        max.col <- max(mydata[, z], na.rm = TRUE)

        ## don't need to group by all levels - want one smooth etc
        group <- "NewGroupVar"
        mydata$NewGroupVar <- "NewGroupVar"

        if (!"pch" %in% names(extra.args)) extra.args$pch <- 16

        if (thekey) {
            legend <- list(right = list(fun = draw.colorkey, args =
                           list(key = list(col = openColours(cols, length(breaks)),
                                at = breaks, labels = list(at = br ^ (1 / thePower),
                                             labels = br)),
                                draw = FALSE)))
        } else {
            legend <- NULL
        }

    } else {

        mydata <- cutData(mydata, type, ...)

        if (!is.na(group))  mydata <- cutData(mydata, group, ...)

        legend <- NULL
    }


    ## if no group to plot, then add a dummy one to make xyplot work
    if (is.na(group)) {mydata$MyGroupVar <- factor("MyGroupVar"); group <-  "MyGroupVar"}

    ## number of groups
    npol <- length(levels(mydata[ ,group]))

    if (!"pch" %in% names(extra.args)) extra.args$pch <- seq(npol)

    ## set up colours
    myColors <- openColours(cols, npol)

    ## basic function for lattice call + defaults
    temp <- paste(type, collapse = "+")
    myform <- formula(paste(y, "~", x, "|", temp, sep = ""))

    scales <- list(x = list(log = nlog.x, rot = x.rot, relation = x.relation),
                   y = list(log = nlog.y, relation = y.relation, rot = 0))

    ## if logs are chosen, ensure data >0 for line fitting etc
    if (log.x)  mydata <- mydata[mydata[ , x] > 0, ]
    if (log.y)  mydata <- mydata[mydata[ , y] > 0, ]

    pol.name <- sapply(levels(mydata[ , group]), function(x) quickText(x, auto.text))

    if (is.na(z)) { ## non-continuous key

        if (key & npol > 1) {
            if (plot.type == "p") {
                key <- list(points = list(col = myColors[1:npol]),
                            pch = if("pch" %in% names(extra.args)) extra.args$pch else 1,
                            text = list(lab = pol.name, cex = 0.8),
                            space = key.position, columns = key.columns,
                            title = quickText(key.title, auto.text), cex.title = 1,
                            border = "grey")
            }

            if (plot.type == "l") {
                key <- list(lines = list(col = myColors[1:npol], lty = extra.args$lty, lwd = extra.args$lwd),
                            text = list(lab = pol.name, cex = 0.8),  space = key.position,
                            columns = key.columns,
                            title = quickText(key.title, auto.text), cex.title = 1,
                            border = "grey")
            }

            if (plot.type == "b") {
                key <- list(points = list(col = myColors[1:npol]),
                            pch = if("pch" %in% names(extra.args)) extra.args$pch else 1,
                            lines = list(col = myColors[1:npol],
                            lty = extra.args$lty, lwd = extra.args$lwd),
                            text = list(lab = pol.name, cex = 0.8),  space = key.position,
                            columns = key.columns,
                            title = quickText(key.title, auto.text), cex.title = 1,
                            border = "grey")
            }


        } else {

            key <- NULL
        }
    }

    ## special wd layout
    if (length(type) == 1 & type[1] == "wd" & is.null(extra.args$layout)) {
        ## re-order to make sensible layout
        ## starting point code as of ManKendall
        wds <-  c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
        mydata$wd <- ordered(mydata$wd, levels = wds)
        wd.ok <- sapply(wds, function (x) {if (x %in% unique(mydata$wd)) FALSE else TRUE })
        skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
        mydata$wd <- factor(mydata$wd)
        extra.args$layout <- c(3, 3)
        if(!"skip" %in% names(extra.args))
            extra.args$skip <- skip
    }
    if(!"skip" %in% names(extra.args))
         extra.args$skip <- FALSE

    ## proper names of labelling
    ## ############################################################################

    stripName <- sapply(levels(mydata[ , type[1]]), function(x) quickText(x, auto.text))
    if (strip) strip <- strip.custom(factor.levels = stripName)

    if (length(type) == 1 ) {

        strip.left <- FALSE

    } else { ## two conditioning variables
        stripName <- sapply(levels(mydata[ , type[2]]), function(x)
                            quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels =  stripName)
    }
    ## #############################################################################


    ## no strip needed for single panel
    if (length(type) == 1 & type[1]  == "default") strip <- FALSE

    ## not sure how to evaluate "group" in xyplot, so change to a fixed name
    id <- which(names(mydata) == group)
    names(mydata)[id] <- "MyGroupVar"


    if (method == "scatter") {

        xyplot.args <- list(x = myform,  data = mydata, groups = mydata$MyGroupVar,
                      type = c("p", "g"),
                      as.table = TRUE,
                      scales = scales,
                      key = key,
                      par.strip.text = list(cex = 0.8),
                      strip = strip,
                      strip.left = strip.left,
                      yscale.components = yscale.components.log10,
                      xscale.components = xscale.components.log10,
                      legend = legend,
                      panel =  panel.superpose,...,
                      panel.groups = function(x, y, col.symbol, col, type, col.line,
                      lty, lwd, group.number,
                      subscripts,...)
                  {

                      ## specific treatemt of trajectory lines
                      ## in order to avoid a line back to the origin, need to process
                      ## in batches
                      if (traj) {

                          if (!is.na(z)) {
                              ## colour by z
                              ddply(mydata[subscripts, ], "date", function (x)
                                    llines(x$lon, x$lat, col.line = x$col, lwd = lwd,
                                           lty = lty))
                          } else {
                              ## colour by a grouping variable
                              ddply(mydata[subscripts, ], .(date), function (x)
                                    llines(x$lon, x$lat, col.line = myColors[group.number],
                                           lwd = lwd, lty = lty))
                          }

                      }

                      if (!is.na(z) & !traj) panel.xyplot(x, y,
                                                          col.symbol = thecol[subscripts],
                                                  as.table = TRUE, ...)

                      if (is.na(z) & !traj) panel.xyplot(x, y, type = plot.type,
                                                 col.symbol = myColors[group.number],
                                                 col.line = myColors[group.number],
                                                 lty = lty, lwd = lwd,
                                                 as.table = TRUE,...)

                      if (linear & npol == 1) panel.linear(x, y, col = "black",
                                   myColors[group.number], lwd = 1, lty = 5,
                                   x.nam = x.nam, y.nam = y.nam, se = ci,  ...)


                      if (smooth) panel.gam(x, y, col = "grey20", col.se = "black",
                                            lty = 1, lwd = 1, se = ci, ...)
                      if (spline) panel.smooth.spline(x, y, col = myColors[group.number],
                                                      lwd = lwd, ...)


                      if (map && group.number == 1) {
                          require(maps)
                          mp <- map(database="world", plot = FALSE)
                          llines(mp$x, mp$y, col = "black")
                      }


                      if (mod.line && group.number == 1) {

                          panel.abline(a = c(0, 0.5), lty = 5)
                          panel.abline(a = c(0, 2), lty = 5)
                          panel.abline(a = c(0, 1), lty = 1)
                      }


                      ## add reference lines
                      panel.abline(v = ref.x, lty = 5)
                      panel.abline(h = ref.y, lty = 5)

                  })

        #by default title if z set
        #else none
        default.main <- if(is.na(z)) "" else paste(x, "vs.", y, "by levels of", z)

        extra.args$main <- if("main" %in% names(extra.args))
                               quickText(extra.args$main, auto.text) else
                                   quickText(default.main, auto.text)

        if(!"pch" %in% names(extra.args))
            extra.args$pch <- 1

        #reset for extra.args
        xyplot.args<- openair:::listUpdate(xyplot.args, extra.args)

        #plot
        plt <- do.call(xyplot, xyplot.args)

    }

    if (method == "hexbin") {
        require(hexbin)

        ##plot via ... handler

        hexbinplot.args <- list(x = myform, data = mydata,
                          strip = strip,
                          as.table = TRUE,
                          xbins = 40,
                          par.strip.text = list(cex = 0.8),
                          colorkey = TRUE,
                          aspect = 1,
                          colramp = function(n) {openColours(method.col, n)},
                          trans = function(x) log(x), inv = function(x) exp(x),
                          panel = function(x,...) {
                              panel.grid(-1, -1)
                              panel.hexbinplot(x,...)
                              if (mod.line) {
                                  panel.abline(a = c(0, 0.5), lty = 5)
                                  panel.abline(a = c(0, 2), lty = 5)
                                  panel.abline(a = c(0, 1), lty = 1)
                              }
                              ## add reference lines
                              panel.abline(v = ref.x, lty = 5)
                              panel.abline(h = ref.y, lty = 5)
                          })

        #by default no title ever
        extra.args$main <- if("main" %in% names(extra.args))
                               quickText(extra.args$main, auto.text) else quickText("", auto.text)

        if(!"pch" %in% names(extra.args))
            extra.args$pch <- 1

        #reset for extra.args
        hexbinplot.args<- openair:::listUpdate(hexbinplot.args, extra.args)

        #plot
        plt <- do.call(hexbinplot, hexbinplot.args)

    }


    if (method == "level") {

        ## bin data
        mydata$ygrid <- round_any(mydata[ , y], y.inc)
        mydata$xgrid <- round_any(mydata[ , x], x.inc)

        rhs <- c("xgrid", "ygrid", type)
        rhs <- paste(rhs, collapse = "+")
        myform <- formula(paste(z, "~", rhs))
        mydata <-aggregate(myform, data = mydata, mean, na.rm = TRUE)

        smooth.grid <- function(mydata, z) {

            myform <- formula(paste(z, "~ s(xgrid, ygrid, k = ", k , ")", sep = ""))
            res <- 101
            Mgam <- gam(myform, data = mydata)
            new.data <- expand.grid(xgrid = seq(min(mydata$xgrid),
                                    max(mydata$xgrid), length = res),
                                    ygrid = seq(min(mydata$ygrid),
                                    max(mydata$ygrid), length = res))

            pred <- predict.gam(Mgam, newdata = new.data)
            pred <- as.vector(pred)

            new.data[ , z] <- pred

            ## exlcude too far
            ## exclude predictions too far from data (from mgcv)
            x <- seq(min(mydata$xgrid), max(mydata$xgrid), length = res)
            y <- seq(min(mydata$ygrid), max(mydata$ygrid), length = res)

            wsp <- rep(x, res)
            wdp <- rep(y, rep(res, res))

            ## data with gaps caused by min.bin
            all.data <- na.omit(data.frame(xgrid = mydata$xgrid, ygrid = mydata$ygrid, z))
            ind <- with(all.data, exclude.too.far(wsp, wdp, mydata$xgrid,
                                                  mydata$ygrid, dist = 0.05))

            new.data[ind, z] <- NA

            new.data
        }

        if (smooth) mydata <- ddply(mydata, type, smooth.grid, z)

        ## basic function for lattice call + defaults
        temp <- paste(type, collapse = "+")
        myform <- formula(paste(z, "~ xgrid * ygrid |", temp, sep = ""))

        nlev <- 200

        breaks <- pretty(mydata[ , z], n = nlev)

        nlev2 <- length(breaks)

        if (missing(cols)) cols <- "default"
        col <- openColours(cols, (nlev2 - 1))

        col.scale <- breaks


        #plot byt ... handler

        levelplot.args <- list(x = myform, data = mydata,
                         strip = strip,
                         as.table = TRUE,
                         region = TRUE,
                         col.regions = col,
                         at = col.scale,
                         par.strip.text = list(cex = 0.8),
                         colorkey = TRUE,
                         panel = function(x, y, z, subscripts,...) {
                             panel.grid(h = -1, v = -1)
                             panel.levelplot(x, y, z, subscripts,
                                             interpolate = FALSE,
                                             at = col.scale,
                                             pretty = TRUE,
                                             col.regions = col,
                                             labels = FALSE)

                             if (mod.line) {
                                 panel.abline(a = c(0, 0.5), lty = 5)
                                 panel.abline(a = c(0, 2), lty = 5)
                                 panel.abline(a = c(0, 1), lty = 1)
                             }

                             if (map) {

                                 require(maps)
                                 mp <- map(database="world", plot = FALSE)
                                 llines(mp$x, mp$y, col = "black")
                             }
                             ## add reference lines
                             panel.abline(v = ref.x, lty = 5)
                             panel.abline(h = ref.y, lty = 5)

                         })

        #z must exist to get here
        extra.args$main <- if("main" %in% names(extra.args))
                               quickText(extra.args$main, auto.text) else
                                   quickText(paste(x, "vs.", y, "by levels of", z), auto.text)

        if(!"pch" %in% names(extra.args))
            extra.args$pch <- 1

        #reset for extra.args
        levelplot.args<- openair:::listUpdate(levelplot.args, extra.args)

        #plot
        plt <- do.call(levelplot, levelplot.args)

    }


    ## kernel density


    if (method == "density") {
        prepare.grid <- function(subdata) {
            x <- subdata[, x]
            y <- subdata[, y]
            xy <- xy.coords(x, y, "xlab", "ylab")
            xlab <-  xy$xlab
            ylab <- xy$ylab
            x <- cbind(xy$x, xy$y)[is.finite(xy$x) & is.finite(xy$y),
                                   , drop = FALSE]
            xlim <- range(x[, 1])
            ylim <- range(x[, 2])

            map <- grDevices:::.smoothScatterCalcDensity(x, 256)
            xm <- map$x1
            ym <- map$x2

            dens <- map$fhat

            grid <- expand.grid(x = xm, y = ym)

            results <- data.frame(x = grid$x, y = grid$y, z = as.vector(dens))#, cond = subdata$site[1])
            results
        }

#############################################################################

        results.grid <-  ddply(mydata, type, prepare.grid)

        ## auto-scaling
        nlev <- 200  ## preferred number of intervals
        breaks <- pretty(results.grid$z, n = nlev)

        nlev2 <- length(breaks)

        col <- openColours(method.col, (nlev2 - 1)) #was "default"??
        col <- c("transparent", col) ## add white at bottom
        col.scale <- breaks

        ## basic function for lattice call + defaults
        temp <- paste(type, collapse = "+")
        myform <- formula(paste("z ~ x * y", "|", temp, sep = ""))

        #plot via ... handler
        levelplot.args <- list(x = myform, data = results.grid,
                         as.table = TRUE,
                         col.regions = col,
                         region = TRUE,
                         at = col.scale,
                         colorkey = FALSE,

                         panel = function(x, y, z, subscripts,...) {
                             panel.grid(-1, -1)
                             panel.levelplot(x, y, z,
                                             subscripts,
                                             at = col.scale,
                                             pretty = TRUE,
                                             col.regions = col,
                                             labels = FALSE)
                             if (mod.line) {
                                 panel.abline(a = c(0, 0.5), lty = 5)
                                 panel.abline(a = c(0, 2), lty = 5)
                                 panel.abline(a = c(0, 1), lty = 1)
                             }
                             ## add reference lines
                             panel.abline(v = ref.x, lty = 5)
                             panel.abline(h = ref.y, lty = 5)
                         })

        #by default no title ever
        extra.args$main <- if("main" %in% names(extra.args))
                               quickText(extra.args$main, auto.text) else quickText("", auto.text)

        if(!"pch" %in% names(extra.args))
            extra.args$pch <- 1

        #reset for extra.args
        levelplot.args<- openair:::listUpdate(levelplot.args, extra.args)

        #plot
        plt <- do.call(levelplot, levelplot.args)

    }


    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- mydata
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    invisible(output)

}



panel.linear <- function (x, y, form = y ~ x, method = "loess", x.nam, y.nam, ...,
                          se = TRUE, level = 0.95, n = 100, col = plot.line$col,
                          col.se = col, lty = plot.line$lty, lwd = plot.line$lwd,
                          alpha = plot.line$alpha, alpha.se = 0.25, border = NA,
                          subscripts, group.number, group.value, type, col.line,
                          col.symbol, fill, pch, cex, font, fontface, fontfamily)
{
    ## get rid of R check annoyances
    plot.line = NULL


    thedata <- data.frame(x = x, y = y)
    thedata <- na.omit(thedata)

    tryCatch({mod <- lm(y ~ x, data = thedata)

              lims <- current.panel.limits()

              xrange <- c(max(min(lims$x, na.rm = TRUE), min(x, na.rm = TRUE)),
                          min(max(lims$x, na.rm = TRUE), max(x, na.rm = TRUE)))

              xseq <- seq(xrange[1], xrange[2], length = n)

              pred <- predict(mod, data.frame(x = xseq), interval = "confidence")

              if (se) {
                  ## predicts 95% CI by default
                  panel.polygon(x = c(xseq, rev(xseq)), y = c(pred[, 2], rev(pred[, 3])),
                                col = col.se,  alpha = alpha.se, border = border)
              }

              pred <- pred[, 1]

              panel.lines(xseq, pred, col = col, alpha = alpha, lty = lty,
                          lwd = lwd)

              x <- current.panel.limits()$xlim[1]

              y <- 0.95 * current.panel.limits()$ylim[2]

              r.sq <- summary(mod)$r.squared
              slope <- coef(mod)[2]
              intercept <- coef(mod)[1]

              if (intercept > 0) symb <- "+" else symb <- ""
              panel.text(x, y, quickText(paste(y.nam, "=", format(slope, digits = 2),
                                               "[", x.nam, "]", symb,
                                               format(intercept, digits = 2),
                                               " R2=",  format(r.sq, digits = 2),
                                               sep = "")), cex = 0.7, pos = 4)

          }, error = function(x) return)
}




