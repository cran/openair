##' Bivariate polar plot with smoothing
##'
##' Function for plotting pollutant concentration in polar coordinates showing
##' concentration by wind speed and direction. Mean concentrations are
##' calculated for wind speed-direction \sQuote{bins} (e.g. 0-1, 1-2 m/s,...
##' and 0-10, 10-20 degrees etc.).  To aid interpretation, gam smoothing is
##' carried out using \code{mgcv}.
##'
##' The bivariate polar plot is a useful diagnostic tool for quickly gaining an
##' idea of potential sources. Wind speed is one of the most useful variables
##' to use to separate source types (see references). For example, ground-level
##' concentrations resulting from buoyant plumes from chimney stacks tend to
##' peak under higher wind speed conditions. Conversely, ground-level,
##' non-buoyant plumes such as from road traffic, tend to have highest
##' concentrations under low wind speed conditions. Other sources such as from
##' aircraft engines also show differing characteristics by wind speed.
##'
##' The plots can vary considerably depending on how much smoothing is done.
##' The approach adopted here is based on the very flexible and capable
##' \code{mgcv} package that uses \emph{Generalized Additive Models}. While
##' methods do exist to find an optimum level of smoothness, they are not
##' necessarily useful. The principal aim of \code{polarPlot} is as a graphical
##' analysis rather than for quantitative purposes. In this respect the
##' smoothing aims to strike a balance between interesting (real) features and
##' overly noisy data. The defaults used in \code{polarPlot} are based on the
##' analysis of data from many different sources. More advanced users may wish
##' to modify the code and adopt other smoothing approaches.
##'
##' These plots often show interesting features at higher wind speeds (see
##' references below). For these conditions there can be very few measurements
##' and therefore greater uncertainty in the calculation of the surface. There
##' are several ways in which this issue can be tackled. First, it is possible
##' to avoid smoothing altogether and use \code{\link{polarFreq}}. Second, the
##' effect of setting a minimum number of measurements in each wind
##' speed-direction bin can be examined through \code{min.bin}. It is possible
##' that a single point at high wind speed conditions can strongly affect the
##' surface prediction. Therefore, setting \code{min.bin = 3}, for example,
##' will remove all wind speed-direction bins with fewer than 3 measurements
##' \emph{before} fitting the surface. Third, consider setting
##' \code{uncertainty = TRUE}. This option will show the predicted surface
##' together with upper and lower 95% confidence intervals, which take account
##' of the frequency of measurements.
##'
##' Variants on \code{polarPlot} include \code{polarAnnulus} and
##' \code{polarFreq}.
##'
##' @param mydata A data frame minimally containing \code{ws}, \code{wd} and a
##'   pollutant. Can also contain \code{date} if plots by time period are
##'   required.
##' @param pollutant Mandatory. A pollutant name corresponding to a variable in
##'   a data frame should be supplied e.g. \code{pollutant = "nox"}. There can
##'   also be more than one pollutant specified e.g. \code{pollutant = c("nox",
##'   "no2")}. The main use of using two or more pollutants is for model
##'   evaluation where two species would be expected to have similar
##'   concentrations. This saves the user stacking the data and it is possible
##'   to work with columns of data directly. A typical use would be
##'   \code{pollutant = c("obs", "mod")} to compare two columns "obs" (the
##'   observations) and "mod" (modelled values).
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
##' @param statistic The statistic that should be applied to each wind
##'   speed/direction bin. Can be "mean" (default), "median", "max" (maximum),
##'   "frequency". "stdev" (standard deviation) or "weighted.mean". Because of
##'   the smoothing involved, the colour scale for some of these statistics is
##'   only to provide an indication of overall pattern and should not be
##'   interpreted in concentration units e.g. for \code{statistic =
##'   "weighted.mean"} where the bin mean is multiplied by the bin frequency
##'   and divided by the total frequency. In many cases using \code{polarFreq}
##'   will be better. Setting \code{statistic = "weighted.mean"} can be useful
##'   because it provides an indication of the concentration * frequency of
##'   occurrence and will highlight the wind speed/direction conditions that
##'   dominate the overall mean.
##' @param resolution Two plot resolutions can be set: "normal" (the default)
##'   and "fine", for a smoother plot. It should be noted that plots with a
##'   "fine" resolution can take longer to render and the default option should
##'   be sufficient or most circumstances.
##' @param limits The function does its best to choose sensible limits
##'   automatically. However, there are circumstances when the user will wish
##'   to set different ones. An example would be a series of plots showing each
##'   year of data separately. The limits are set in the form \code{c(lower,
##'   upper)}, so \code{limits = c(0, 100)} would force the plot limits to span
##'   0-100.
##' @param exclude.missing Setting this option to \code{TRUE} (the default)
##'   removes points from the plot that are too far from the original data. The
##'   smoothing routines will produce predictions at points where no data exist
##'   i.e. they predict. By removing the points too far from the original data
##'   produces a plot where it is clear where the original data lie. If set to
##'   \code{FALSE} missing data will be interpolated.
##' @param uncertainty Should the uncertainty in the calculated surface be
##'   shown? If \code{TRUE} three plots are produced on the same scale showing
##'   the predicted surface together with the estimated lower and upper
##'   uncertainties at the 95% confidence interval. Calculating the
##'   uncertainties is useful to understand whether features are real or not.
##'   For example, at high wind speeds where there are few data there is
##'   greater uncertainty over the predicted values. The uncertainties are
##'   calculated using the GAM and weighting is done by the frequency of
##'   measurements in each wind speed-direction bin. Note that if uncertainties
##'   are calculated then the type is set to "default".
##' @param cols Colours to be used for plotting. Options include "default",
##'   "increment", "heat", "jet" and user defined. For user defined the user
##'   can supply a list of colour names recognised by R (type \code{colours()}
##'   to see the full list). An example would be \code{cols = c("yellow",
##'   "green", "blue")}
##' @param min.bin The minimum number of points allowed in a wind speed/wind
##'   direction bin.  The default is 1. A value of two requires at least 2
##'   valid records in each bin an so on; bins with less than 2 valid records
##'   are set to NA. Care should be taken when using a value > 1 because of the
##'   risk of removing real data points. It is recommended to consider your
##'   data with care. Also, the \code{polarFreq} function can be of use in such
##'   circumstances.
##' @param upper This sets the upper limit wind speed to be used. Often there
##'   are only a relatively few data points at very high wind speeds and
##'   plotting all of them can reduce the useful information in the plot.
##' @param ws.int This sets the (dashed) circular grid line spacing. The
##'   default is 5. It can be useful, for example, to set \code{ws.int} to a
##'   lower value when the wind speeds are low to get a sensible grid spacing.
##' @param angle.scale The wind speed scale is by default shown at a 45 degree
##'   angle. Sometimes the placement of the scale may interfere with an
##'   interesting feature. The user can therefore set \code{angle.scale} to
##'   another value (between 0 and 360 degrees) to mitigate such problems. For
##'   example \code{angle.scale = 315} will draw the scale heading in a NW
##'   direction.
##' @param units The units shown on the wind speed scale. These are only shown
##'   on the third highest \code{ws.int} interval to reduce chart clutter.
##' @param force.positive The default is \code{TRUE}. Sometimes if smoothing
##'   data with streep gradients it is possible for predicted values to be
##'   negative. \code{force.positive = TRUE} ensures that predictions remain
##'   postive. This is useful for several reasons. First, with lots of missing
##'   data more interpolation is needed and this can result in artefacts
##'   because the predictions are too far from the original data. Second, if it
##'   is known beforehand that the data are all postive, then this option
##'   carries that assumption through to the prediction. The only likley time
##'   where setting \code{force.positive = FALSE} would be if background
##'   concentrations were first subtracted resulting in data that is
##'   legitimately negative. For the vast majority of situations it is expected
##'   that the user will not need to alter the default option.
##' @param k This is the smoothing parameter that is set if auto.smooth is set
##'   to \code{FALSE}. Typically, value of around 100 (the default) seems to be
##'   suitable and will resolve more features in the plot.
##' @param normalise If \code{TRUE} concentrations are normalised by dividing
##'   by their mean value. This is done \emph{after} fitting the smooth
##'   surface. This option is particularly useful if one is interested in the
##'   patterns of concentrations for several pollutants on different scales
##'   e.g. NOx and CO. Often useful if more than one \code{pollutant} is
##'   chosen.
##' @param key.header,key.footer Adds additional text/labels to the scale key.
##'   For example, passing the options \code{key.header = "header", key.footer
##'   = "footer1"} adds addition text above and below the scale key. These
##'   arguments are passed to \code{drawOpenKey} via \code{quickText}, applying
##'   the \code{auto.text} argument, to handle formatting.
##' @param key.position Location where the scale key is to plotted.  Allowed
##'   arguments currently include \code{"top"}, \code{"right"}, \code{"bottom"}
##'   and \code{"left"}.
##' @param key Fine control of the scale key via \code{drawOpenKey}. See
##'   \code{drawOpenKey} for further details.
##' @param auto.text Either \code{TRUE} (default) or \code{FALSE}. If
##'   \code{TRUE} titles and axis labels will automatically try and format
##'   pollutant names and units properly e.g.  by subscripting the `2' in NO2.
##' @param \dots Other graphical parameters passed onto \code{lattice:levelplot}
##'   and \code{cutData}. For example, \code{polarPlot} passes the option 
##'   \code{hemisphere = "southern"} on to \code{cutData} to provide southern 
##'   (rather than default northern) hemisphere handling of \code{type = "season"}.
##'   Similarly, common axis and title labelling options (such as \code{xlab}, 
##'   \code{ylab}, \code{main}) are passed to \code{levelplot} via \code{quickText} 
##'   to handle routine formatting.
##' @export
##' @return As well as generating the plot itself, \code{polarPlot} also
##'   returns an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- polarPlot(mydata, "nox")}, this output can be used to
##'   recover the data, reproduce or rework the original plot or undertake
##'   further analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}. See
##'   \code{\link{openair.generics}} for further details.
##'
##' \code{polarPlot} surface data can also be extracted directly using the
##'   \code{results}, e.g.  \code{results(object)} for \code{output <-
##'   polarPlot(mydata, "nox")}. This returns a data frame with four set
##'   columns: \code{cond}, conditioning based on \code{type}; \code{u} and
##'   \code{v}, the translational vectors based on \code{ws} and \code{wd}; and
##'   the local \code{pollutant} estimate.
##' @author David Carslaw
##' @seealso \code{\link{polarAnnulus}}, \code{\link{polarFreq}}
##' @references
##'
##' Carslaw, D.C., Beevers, S.D, Ropkins, K and M.C. Bell (2006).  Detecting
##'   and quantifying aircraft and other on-airport contributions to ambient
##'   nitrogen oxides in the vicinity of a large international airport.
##'   Atmospheric Environment. 40/28 pp 5424-5434.
##'
##' Henry, R.C., Chang, Y.S., Spiegelman, C.H., 2002. Locating nearby sources
##'   of air pollution by nonparametric regression of atmospheric
##'   concentrations on wind direction. Atmospheric Environment 36 (13),
##'   2237-2244.
##'
##' Westmoreland, E.J., N. Carslaw, D.C. Carslaw, A. Gillah and E. Bates
##'   (2007).  Analysis of air quality within a street canyon using statistical
##'   and dispersion modelling techniques.  Atmospheric Environment. Vol.
##'   41(39), pp. 9195-9205.
##'
##' Yu, K.N., Cheung, Y.P., Cheung, T., Henry, R.C., 2004.  Identifying the
##'   impact of large urban airports on local air quality by nonparametric
##'   regression. Atmospheric Environment 38 (27), 4501-4507.
##' @keywords methods
##' @examples
##'
##'
##' # load example data from package
##' data(mydata)
##'
##' # basic plot
##' polarPlot(mydata, pollutant = "nox")
##'
##' # polarPlots by year on same scale
##' \dontrun{polarPlot(mydata, pollutant = "so2", type = "year", main = "polarPlot of so2")}
##'
##' # set minimum number of bins to be used to see if pattern remains similar
##' polarPlot(mydata, pollutant = "nox", min.bin = 3)
##'
##' # plot by day of the week
##'
##' \dontrun{polarPlot(mydata, pollutant = "pm10", type = "weekday")}
##'
##' # show the 95% confidence intervals in the surface fitting
##' \dontrun{polarPlot(mydata, pollutant = "so2", uncertainty = TRUE)}
##'
##'
polarPlot <- function(mydata,
                      pollutant = "nox",
                      type = "default",
                      statistic = "mean",
                      resolution = "normal",
                      limits = NA,
                      exclude.missing = TRUE,
                      uncertainty = FALSE,
                      cols = "default",
                      min.bin = 1,
                      upper = NA,
                      ws.int = 5,
                      angle.scale = 45,
                      units = "(m/s)",
                      force.positive = TRUE,
                      k = 100,
                      normalise = FALSE,
                      key.header = "",
                      key.footer = pollutant,
                      key.position = "right",
                      key = TRUE,
                      auto.text = TRUE, ...) {


    ## initial checks ##########################################################################################
    if (length(type) > 2) {stop("Maximum number of types is 2.")}

    if (uncertainty) type <- "default" ## can't have conditioning here

    if (uncertainty & length(pollutant) > 1) stop("Can only have one pollutant when uncertainty = TRUE")

    if (!statistic %in% c("mean", "median", "frequency", "max", "stdev", "weighted.mean")) {
        stop (paste("statistic '", statistic, "' not recognised", sep = ""))
    }

    if (missing(key.header)) key.header <- statistic
    if (key.header == "weighted.mean") key.header <- c("weighted", "mean")

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        ## strip only
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
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

    #layout default
    if(!"layout" %in% names(extra.args))
         extra.args$layout <- NULL

    ## extract variables of interest

    vars <- c("wd", "ws", pollutant)
    if (any(type %in%  dateTypes)) vars <- c(vars, "date")

    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

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

    ## ##########################################################################################################

    mydata <- na.omit(mydata)
    ## cutData depending on type
    mydata <- cutData(mydata, type, ...)


    ## if upper ws not set, set it to the max to display all information
    max.ws <- ceiling(max(mydata$ws, na.rm = TRUE))
    if(missing(upper)) upper <- max.ws

    ## for resolution of grid plotting (default = 101; fine =201)
    if (resolution == "normal") int <- 101
    if (resolution == "fine") int <- 201
    if (resolution == "ultra.fine") int <- 401  ## very large files!

    ## binning wd data properly
    ws <- seq(0, max.ws, length = 30)
    wd <- seq(from = 10, to = 360, by = 10) ## wind directions from 10 to 360
    ws.wd <- expand.grid(ws = ws, wd = wd)

    u <- with(ws.wd, ws * sin(pi * wd / 180))  ## convert to polar coords
    v <- with(ws.wd, ws * cos(pi * wd / 180))

    ## data to predict over
    input.data <- expand.grid(u = seq(-upper, upper, length = int),
                              v = seq(-upper, upper, length = int))

    prepare.grid <- function(mydata) {
        ## identify which ws and wd bins the data belong
        wd <- cut(mydata$wd, breaks = seq(0, 360, 10), include.lowest = TRUE)
        ws <- cut(mydata$ws, breaks = seq(0, max.ws, length = 31), include.lowest = TRUE)

        binned <- switch(statistic,
                         frequency = tapply(mydata[ , pollutant], list(wd, ws), function(x)
                         length(na.omit(x))),
                         mean =  tapply(mydata[, pollutant], list(wd, ws), function(x)
                         mean(x, na.rm = TRUE)),
                         median = tapply(mydata[, pollutant], list(wd, ws), function(x)
                         median(x, na.rm = TRUE)),
                         max = tapply(mydata[, pollutant], list(wd, ws), function(x)
                         max(x, na.rm = TRUE)),
                         stdev = tapply(mydata[, pollutant], list(wd, ws), function(x)
                         sd(x, na.rm = TRUE)),
                         weighted.mean = tapply(mydata[, pollutant], list(wd, ws),
                         function(x) (mean(x) * length(x) / nrow(mydata)))
                         )

        binned <- as.vector(t(binned))

        ## frequency - remove points with freq < min.bin
        bin.len <- tapply(mydata[, pollutant], list(ws, wd), length)
        binned.len <- as.vector(bin.len)

        ids <- which(binned.len < min.bin)
        binned[ids] <- NA
######################Smoothing#################################################
        if (force.positive) n <- 0.5 else n <- 1

        ## no uncertainty to calculate
        if (!uncertainty) {
            Mgam <- gam(binned ^ n ~ s(u, v, k = k))
            pred <- predict.gam(Mgam, input.data)
            pred <- pred ^ (1 / n)
            pred <- as.vector(pred)
            results <- data.frame(u = input.data$u, v = input.data$v, z = pred)

        } else {

            ## uncertainties calculated, weighted by number of points in each bin
            Mgam <- gam(binned ^ n ~ s(u, v, k = k), weights = binned.len)
            pred <- predict.gam(Mgam, input.data, se = TRUE)
            uncer <- 2 * as.vector(pred[[2]]) ## for approx 95% CI
            pred <- as.vector(pred[[1]]) ^ (1 / n)


            ## do not weight for central prediction
            Mgam <- gam(binned ^ n ~ s(u, v, k = k))
            pred <- predict.gam(Mgam, input.data)
            pred <- as.vector(pred)
            Lower <- (pred - uncer) ^ (1 / n)
            Upper <- (pred + uncer) ^ (1 / n)
            pred <- pred ^ (1 / n)

            n <- length(pred)
            results <-  data.frame(u = rep(input.data$u, 3), v = rep(input.data$v, 3),
                                   z = c(pred, Lower, Upper),
                                   default = rep(c("prediction", "lower uncertainty",
                                   "upper uncertainty"), each = n))
        }

#############################################################################
        ## function to remove points too far from original data
        exclude <- function(results) {

            ## exclude predictions too far from data (from mgcv)
            x <- seq(-upper, upper, length = int)
            y <- x
            res <- int
            wsp <- rep(x, res)
            wdp <- rep(y, rep(res, res))

            ## data with gaps caused by min.bin
            all.data <- na.omit(data.frame(u, v, binned))
            ind <- with(all.data, exclude.too.far(wsp, wdp, u, v, dist = 0.05))

            results$z[ind] <- NA
            results
        }

        if (exclude.missing) results <- exclude(results)

        results
    }

    ## ########################################################################################################

    results.grid <- ddply(mydata, type, prepare.grid)

    ## remove wind speeds > upper to make a circle
    results.grid$z[(results.grid$u ^ 2 + results.grid$v ^ 2) ^ 0.5 > upper] <- NA

    ## proper names of labelling ##############################################################################
    pol.name <- sapply(levels(results.grid[ , type[1]]), function(x) quickText(x, auto.text))
    strip <- strip.custom(factor.levels = pol.name)

    if (length(type) == 1 ) {

        strip.left <- FALSE

    } else { ## two conditioning variables

        pol.name <- sapply(levels(results.grid[ , type[2]]), function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels = pol.name)
    }
    ## ########################################################################################################

    if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
    if (uncertainty) strip <- TRUE

    ## normalise by divining by mean conditioning value if needed
    if (normalise){
        results.grid <- ddply(results.grid, type, transform, z = z / mean(z, na.rm = TRUE))
        if (missing(key.footer)) key.footer <- "normalised \nlevel"
    }


    ## auto-scaling
    nlev <- 200  ## preferred number of intervals

    ## handle missing breaks arguments
    if(missing(limits)) breaks <- pretty(results.grid$z, n = nlev) else breaks <-
        pretty(limits, n = nlev)

    nlev2 = length(breaks)

    col <- openColours(cols, (nlev2 - 1))

    col.scale = breaks

    #special handling of layout for uncertainty
    if (uncertainty & is.null(extra.args$layout)) {
        extra.args$layout <- c(3, 1)
    }


    ## scale key setup ######################################################################################

    legend <- list(col = col, at = col.scale, space = key.position,
                   auto.text = auto.text, footer = key.footer, header = key.header,
                   height = 1, width = 1.5, fit = "all")
    legend <- makeOpenKeyLegend(key, legend, "polarPlot")

########################################################################################################

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("z ~ u * v | ", temp, sep = ""))

    levelplot.args <- list(x = myform, results.grid, axes = FALSE,
                     as.table = TRUE,
                     strip = strip,
                     strip.left = strip.left,
                     col.regions = col,
                     region = TRUE,
                     aspect = 1,
                     at = col.scale,
                     par.strip.text = list(cex = 0.8),
                     scales = list(draw = FALSE),
                     xlim = c(-upper * 1, upper * 1),
                     ylim = c(-upper * 1, upper * 1),
                     colorkey = FALSE, legend = legend,

                     panel = function(x, y, z,subscripts,...) {
                         panel.levelplot(x, y, z,
                                         subscripts,
                                         at = col.scale,
                                         pretty = TRUE,
                                         col.regions = col,
                                         labels = FALSE)

                         angles <- seq(0, 2 * pi, length = 360)

                         sapply(seq(ws.int, 10 * ws.int, ws.int), function(x)
                                llines(x * sin(angles), x * cos(angles), col = "grey", lty = 5))

                         ltext(seq(ws.int, 10 * ws.int, by = ws.int) * sin(pi * angle.scale / 180),
                               seq(ws.int, 10 * ws.int, by = ws.int) * cos(pi * angle.scale / 180),
                               paste(seq(ws.int, 10 * ws.int, by = ws.int), c("", "",
                                                              units, rep("", 7))), cex = 0.7)

                         ## add axis line to central polarPlot
                         larrows(-upper, 0, upper, 0, code = 3, length = 0.1)
                         larrows(0, -upper, 0, upper, code = 3, length = 0.1)

                         ltext(upper * -1 * 0.95, 0.07 * upper, "W", cex = 0.7)
                         ltext(0.07 * upper, upper * -1 * 0.95, "S", cex = 0.7)
                         ltext(0.07 * upper, upper * 0.95, "N", cex = 0.7)
                         ltext(upper * 0.95, 0.07 *upper, "E", cex = 0.7)

                     })

    #reset for extra.args
    levelplot.args<- listUpdate(levelplot.args, extra.args)

    #plot
    plt <- do.call(levelplot, levelplot.args)

    ## output ##############################################################################################

    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))

    newdata <- results.grid
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    ## reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)

    invisible(output)

}


