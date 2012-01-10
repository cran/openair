##' Function to plot percentiles by wind direction
##'
##' \code{percentileRose} plots percentiles by wind direction with flexible
##' conditioning. The plot can display mutiple percentile lines or filled
##' areas.
##'
##' \code{percentileRose} calculates percentile levels of a pollutant and plots
##' them by wind direction. One or more percentile levels can be calculated and
##' these are displayed as either filled areas or as lines.
##'
##' The levels by wind direction are calculated using a cyclic smooth cubic
##' spline. The wind directions are rounded to the nearest 10 degrees,
##' consistent with surface data from the UK Met Office before a smooth is
##' fitted.
##'
##' The \code{percentileRose} function compliments other similar functions
##' including \code{\link{windRose}}, \code{\link{pollutionRose}},
##' \code{\link{polarFreq}} or \code{\link{polarPlot}}. It is most useful for
##' showing the distribution of concentrations by wind direction and often can
##' reveal different sources e.g. those that only affect high percentile
##' concentrations such as a chimney stack.
##'
##' Similar to other functions, flexible conditioning is available through the
##' \code{type} option. It is easy for example to consider multiple percentile
##' values for a pollutant by season, year and so on. See examples below.
##'
##' \code{percentileRose} also offers great flexibility with the scale used and
##' the user has fine control over both the range, interval and colour.
##'
##' @param mydata A data frame minimally containing \code{wd} and a numeric
##'   field to plot --- \code{pollutant}.
##' @param pollutant Mandatory. A pollutant name corresponding to a variable in
##'   a data frame should be supplied e.g. \code{pollutant = "nox"}. More than
##'   one pollutant can be supplied e.g. \code{pollutant = c("no2", "o3")}
##'   provided there is only one \code{type}.
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
##' Type can be up length two e.g. \code{type = c("season", "weekday")} will
##'   produce a 2x2 plot split by season and day of the week. Note, when two
##'   types are provided the first forms the columns and the second the rows.
##' @param percentile The percentile value(s) to plot. Must be between 0--100.
##' @param cols Colours to be used for plotting. Options include "default",
##'   "increment", "heat", "jet" and user defined. For user defined the user
##'   can supply a list of colour names recognised by R (type \code{colours()}
##'   to see the full list). An example would be \code{cols = c("yellow",
##'   "green", "blue")}.
##' @param fill Should the percentile intervals be filled (default) or should
##'   lines be drawn (\code{fill = FALSE}).
##' @param intervals User-supplied intervals for the scale
##' e.g. \code{intervals = c(0, 10, 30, 50)}
##' @param angle.scale The pollutant scale is by default shown at a 45 degree
##'   angle. Sometimes the placement of the scale may interfere with an
##'   interesting feature. The user can therefore set \code{angle.scale} to
##'   another value (between 0 and 360 degrees) to mitigate such problems. For
##'   example \code{angle.scale = 315} will draw the scale heading in a NW
##'   direction.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the \sQuote{2}
##'   in NO2.
##' @param key.header Adds additional text/labels to the scale key.
##'   For example, passing options \code{key.header = "header", key.footer =
##'   "footer"} adds addition text above and below the scale key. These
##'   arguments are passed to \code{drawOpenKey} via \code{quickText}, applying
##'   the \code{auto.text} argument, to handle formatting.
##' @param key.footer \code{key.header}.
##' @param key.position Location where the scale key is to plotted.  Allowed
##'   arguments currently include \code{"top"}, \code{"right"}, \code{"bottom"}
##'   and \code{"left"}.
##' @param key Fine control of the scale key via \code{drawOpenKey}. See
##'   \code{drawOpenKey} for further details.
##' @param ... Other graphical parameters are passed onto \code{cutData} and
##'   \code{lattice:xyplot}. For example, \code{percentileRose} passes the option
##'   \code{hemisphere = "southern"} on to \code{cutData} to provide southern
##'   (rather than default northern) hemisphere handling of \code{type = "season"}.
##'   Similarly, common graphical arguments, such as \code{xlim} and \code{ylim}
##'   for plotting ranges and \code{lwd} for line thickness when using
##'   \code{fill = FALSE}, are passed on \code{xyplot}, although some local
##'   modifications may be applied by openair. For example, axis and title
##'   labelling options (such as \code{xlab}, \code{ylab} and \code{main})
##'   are passed to \code{xyplot} via \code{quickText} to handle routine formatting.
##' @export
##' @return As well as generating the plot itself, \code{percentileRose} also
##'   returns an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- percentileRose(mydata, "nox")}, this output can be used
##'   to recover the data, reproduce or rework the original plot or undertake
##'   further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}. See
##'   \code{\link{openair.generics}} for further details.
##' @author David Carslaw
##' @seealso See Also as \code{\link{windRose}}, \code{\link{pollutionRose}},
##'   \code{\link{polarFreq}}, \code{\link{polarPlot}}
##' @keywords methods
##' @examples
##'
##'
##' # basic percentile plot
##' percentileRose(mydata, pollutant = "o3")
##'
##' # 50/95th percentiles of ozone, with different colours
##' percentileRose(mydata, pollutant = "o3", percentile = c(50, 95), col = "brewer1")
##'
##' # percentiles of ozone by year, with different colours
##' percentileRose(mydata, type = "year", pollutant = "o3", col = "brewer1")
##'
##' # percentile concentrations by season and day/nighttime..
##' percentileRose(mydata, type = c("season", "daylight"), pollutant = "o3", col = "brewer1")
##'
##'
##'
percentileRose <- function (mydata, pollutant = "nox", type = "default",
                            percentile = c(25, 50, 75, 90, 95), cols = "default",
                            fill = TRUE,
                            intervals = NULL,
                            angle.scale = 45,
                            auto.text = TRUE,  key.header = NULL,
                            key.footer = "percentile", key.position = "bottom",
                            key = TRUE,  ...)

{

    vars <- c("wd", pollutant)
    if (any(type %in%  openair:::dateTypes)) vars <- c(vars, "date")

    mydata <- openair:::checkPrep(mydata, vars, type, remove.calm = FALSE)
    ## round wd
    mydata$wd <- 10 * ceiling(mydata$wd / 10 - 0.5)

    ## need lowest value if shading
    if (fill) percentile <- unique(c(0, percentile))

    ## if more than one pollutant, need to stack the data and set type = "variable"
    ## this case is most relevent for model-measurement compasrions where data are in columns
    ## Can also do more than one pollutant and a single type that is not "default", in which
    ## case pollutant becomes a conditioning variable
    if (length(pollutant) > 1) {

        if (length(type) > 1) {
            warning(paste("Only type = '", type[1], "' will be used", sep = ""))
            type <- type[1]
        }
        ## use pollutants as conditioning variables
        mydata <- melt(mydata, measure.vars = pollutant)
        ## now set pollutant to "value"
        pollutant <- "value"
        type <- c(type, "variable")
    }


    ##extra.args setup
    extra.args <- list(...)

    #label controls
    extra.args$xlab <- if("xlab" %in% names(extra.args))
                           quickText(extra.args$xlab, auto.text) else quickText("", auto.text)
    extra.args$ylab <- if("ylab" %in% names(extra.args))
                           quickText(extra.args$ylab, auto.text) else quickText("", auto.text)
    extra.args$main <- if("main" %in% names(extra.args))
                           quickText(extra.args$main, auto.text) else quickText("", auto.text)

    #lwd handling
    if(!"lwd" %in% names(extra.args))
        extra.args$lwd <- 2

    mydata <- na.omit(mydata)

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        #strip only
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
    }

    if (!fill) { ## labels depend on whether line or area are used
        theLabels <- percentile
    } else {
        values <- cbind(percentile[-length(percentile)], percentile[-1])
        theLabels <- paste(values[ , 1], "-", values[ , 2], sep = "")
    }


    prepare.grid <- function(mydata) {
        ## add zero wind angle = same as 360 for cyclic spline
        ids <- which(mydata$wd == 360)

        if (length(ids) > 0) {
            zero.wd <- mydata[ids, ]
            zero.wd$wd <- 0
            mydata <- rbind.fill(mydata, zero.wd)
        }

        ## calculate percentiles
        percentiles <- ddply(mydata, .(wd), numcolwise(function (x) quantile(x, probs = percentile /
                                                                             100, na.rm = TRUE)))
        percentiles$percentile <- percentile

        mod.percentiles <- function(i, mydata) {
            ## need to work out how many knots to use in smooth
            thedata <- subset(percentiles, percentile == i)
            min.dat <- min(thedata)

            ## fit a spline through the data; making sure it goes through each wd value
            spline.res <- spline(x = thedata[ , "wd"], y = thedata[, pollutant], n = 361,
                                 method = "natural")

            pred <- data.frame(percentile = i, wd = 0:360, pollutant = spline.res$y)

            ## don't let interpolated percentile be lower than data
            pred$pollutant[pred$pollutant < min.dat] <- min.dat

            ## only plot where there are valid wd
            wd <- unique(percentiles$wd)
            ids <- lapply(wd, function(x) seq(from = x - 5, to = x + 5))
            ids <- unique(do.call(c, ids))
            ids[ids < 0] <- ids[ids < 0] + 360
            pred$pollutant[-ids] <- min(c(0, min(percentiles[ , pollutant], na.rm = TRUE)))
            pred
        }

        results <- ldply(percentile, mod.percentiles)
        results
    }


    mydata <- cutData(mydata, type, ...)
    results.grid <- ddply(mydata, type, prepare.grid)

    ## proper names of labelling ##############################################################################
    pol.name <- sapply(levels(results.grid[ , type[1]]), function(x) quickText(x, auto.text))
    strip <- strip.custom(factor.levels = pol.name)

    if (length(type) == 1 ) {

        strip.left <- FALSE

    } else { ## two conditioning variables

        pol.name <- sapply(levels(results.grid[ , type[2]]), function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels = pol.name)
    }
    if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
###############################################################################

    col <- openColours(cols, length(theLabels))

    legend <- list(col = col, space = key.position, auto.text = auto.text,
                   labels = theLabels, footer = key.footer, header = key.header,
                   height = 0.60, width = 1.5, fit = "scale",
                   plot.style =  "other")
    legend <- openair:::makeOpenKeyLegend(key, legend, "percentileRose")

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("y ~ x | ", temp, sep = ""))

    ## keep unstransformed copy in case data are negative
    results <- results.grid

    results.grid <- transform(results.grid, x = pollutant * sin(wd * pi / 180),
                              y = pollutant * cos(wd * pi / 180))

    min.res <- min(results.grid$pollutant)

    newdata <- results.grid ## data to return

    ## nice intervals for pollutant concentrations
    if (missing(intervals)) intervals <- pretty(results.grid$pollutant)

    labs <- intervals ## the labels

    ## if negative data, add to make all postive to plot properly
    min.int <- min(intervals)
    zero <- NA

    if (min.int < 0 ) {
        zero <- which(intervals == 0) ## the zero line
        intervals <- intervals + -1 * min.int
        results$pollutant <- results$pollutant + -1 * min.int
        results.grid <- transform(results, x = pollutant * sin(wd * pi / 180),
                              y = pollutant * cos(wd * pi / 180))
    }

    xyplot.args <- list(x = myform,
                  xlim = c(max(intervals) * -1, max(intervals) * 1),
                  ylim = c(max(intervals) * -1, max(intervals) * 1),
                  data = results.grid,
                  type = "n",
                  strip = strip,
                  strip.left = strip.left,
                  as.table = TRUE,
                  aspect = 1,
                  par.strip.text = list(cex = 0.8),
                  scales = list(draw = FALSE),

                  panel = function(x, y, subscripts, ...) {

                      if (fill) { ## filled polygons

                          for (i in rev(seq_along(percentile))) {
                              value <- percentile[i]

                              if (i == 1) {
                                  subdata <- subset(results.grid[subscripts, ], percentile == value)
                                  lpolygon(subdata$x, subdata$y, col = "white", border = NA)

                             } else {

                                  subdata1 <- subset(results.grid[subscripts, ], percentile == value)
                                  value2 <- percentile[i - 1]
                                  subdata2 <- subset(results.grid[subscripts, ],
                                                     percentile == value2)
                                  lpolygon(c(subdata1$x, subdata2$x),  c(subdata1$y, subdata2$y),
                                           col = col[i - 1], border = NA)

                              }
                          }
                      }

                      angles <- seq(0, 2 * pi, length = 360)
                      sapply(intervals, function(x) llines(x * sin(angles), x * cos(angles),
                                                           col = "grey85", lty = 5))

                      ## zero line if needed
                      if (!is.na(zero)) llines(intervals[zero] * sin(angles),
                                               intervals[zero] * cos(angles), col = "grey85")


                      ## add axis lines
                      larrows(max(intervals) * -1, 0, max(intervals), 0, code = 3, length = 0.1)
                      larrows(0, max(intervals) * -1, 0, max(intervals), code = 3, length = 0.1)


                      ltext(0.7 * sin(pi * (angle.scale + 5) / 180) * max(intervals),
                            0.7 * cos(pi * (angle.scale + 5) / 180) * max(intervals),
                            quickText(pollutant, auto.text), srt = 0, cex = 0.8, pos = 4)


                      ltext(max(intervals) * -1 * 0.95, 0.07 * max(intervals), "W", cex = 0.7)
                      ltext(0.07 * max(intervals), max(intervals) * -1 * 0.95, "S", cex = 0.7)
                      ltext(0.07 * max(intervals), max(intervals) * 0.95, "N", cex = 0.7)
                      ltext(max(intervals) * 0.95, 0.07 * max(intervals), "E", cex = 0.7)

                      ## draw lines if fill = FALSE
                      if (!fill) {
                          for (i in seq_along(percentile)) {
                              value <- percentile[i]
                              subdata <- subset(results.grid[subscripts, ], percentile == value)
                              llines(subdata$x, subdata$y, col = col[i], lwd = extra.args$lwd)
                          }

                      }

                      ltext(intervals * sin(pi * angle.scale / 180),
                            intervals * cos(pi * angle.scale / 180),
                            paste(labs, c("", "", rep("", 7))), cex = 0.7)


                  }, legend = legend)

    #reset for extra.args
    xyplot.args<- listUpdate(xyplot.args, extra.args)

    #plot
    plt <- do.call(xyplot, xyplot.args)


    ## output ####################################################################################

    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))

    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)

    invisible(output)

}
