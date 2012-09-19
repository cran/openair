##' Trajectory plots with conditioning
##'
##' This function plots back trajectories. There are two related
##' functions: \code{trajPlot} and \code{trajLevel}. These functions
##' require that data are imported using the \code{importTraj}
##' function.
##'
##' Several types of trajectory plot are available. \code{trajPlot} by
##' default will plot each lat/lon location showing the origin of
##' each trajectory, if no \code{pollutant} is supplied.
##'
##' If a pollutant is given, by merging the trajectory data with
##' concentration data (see example below) the trajectories are colour-coded by the
##' concentration of \code{pollutant}. With a long time series there
##' can be lots of overplotting making it difficult to gauge the
##' overall concentration pattern. In these cases setting \code{alpha}
##' to a low value e.g. 0.1 can help.
##'
##' For the investigation of a few days it can be useful to use
##' \code{plot.type = "l"}, which shows the back trajectories as
##' continuous lines rather than individual points. Note that points
##' help to show the duration an air mass spend in a particular
##' location, whereas lines do not.
##'
##' An alternative way of showing the trajectories is to bin the
##' points into latitude/longitude intervals and average the
##' corresponding concentrations. For these purposes \code{trajLevel}
##' should be used. A further useful refinement is to smooth the
##' resulting surface, which is possible by setting \code{smooth =
##' TRUE}.
##'
##' @note This function is under active development and is likely to change
##'
##' @rdname trajPlot
##' @param mydata Data frame, the result of importing a trajectory
##' file using \code{importTraj}
##' @param lon Column containing the longitude, as a decimal.
##' @param lat Column containing the latitude, as a decimal.
##' @param pollutant Pollutant to be plotted.
##' @param type \code{type} determines how the data are split
##' i.e. conditioned, and then plotted. The default is will produce a
##' single plot using the entire data. Type can be one of the built-in
##' types as detailed in \code{cutData} e.g. "season", "year",
##' "weekday" and so on. For example, \code{type = "season"} will
##' produce four plots --- one for each season.
##'
##' It is also possible to choose \code{type} as another variable in
##' the data frame. If that variable is numeric, then the data will be
##' split into four quantiles (if possible) and labelled
##' accordingly. If type is an existing character or factor variable,
##' then those categories/levels will be used directly. This offers
##' great flexibility for understanding the variation of different
##' variables and how they depend on one another.
##'
##' \code{type} can be up length two e.g. \code{type = c("season",
##' "weekday")} will produce a 2x2 plot split by season and day of the
##' week. Note, when two types are provided the first forms the
##' columns and the second the rows.
##'
##' @param smooth Should the trajectory surface be smoothed?
##' @param statistic For \code{trajLevel}. By default the function
##' will plot the mean concentration of a pollutant. If
##' \code{statistic = "frequency"}, a plot will be shown for gridded
##' trajectory frequencies. This is useful to understand where air
##' masses tend to orginate.
##'
##' It is also possible to set \code{statistic = "difference"}. In
##' this case trajectories where the associated concentration is
##' greater than \code{percentile} are compared with the the full set
##' of trajectories to understand the differences in freqeuncies of
##' the origin of air masses. The comparsion is made by comparing the
##' percentage change in gridded frequencies. For example, such a plot
##' could show that the top 10\% of concentrations of PM10 tend to
##' orginate from air-mass origins to the east.
##' @param percentile For \code{trajLevel}. The percentile
##' concentration of \code{pollutant} against which the all
##' trajectories are compared.
##' @param map Should a base map be drawn? If \code{TRUE} the world
##' base map from the \code{maps} package is used.
##' @param lon.inc The longitude-interval to be used for binning data
##' for \code{trajLevel}.
##' @param lat.inc The latitude-interval to be used for binning data
##' when \code{trajLevel}.
##' @param min.bin For \code{trajLevel} the minimum number of unique
##' \emph{trajectories} in a grid cell. Counts below \code{min.bin} are set as
##' missing. For \code{statistic = "frequency"} or \code{statistic =
##' "frequency"}
##' @param ... other arguments are passed to \code{cutData} and
##' \code{scatterPlot}. This provides access to arguments used in both
##' these functions and functions that they in turn pass arguments on
##' to. For example, \code{plotTraj} passes the argument \code{cex} on
##' to \code{scatterPlot} which in turn passes it on to the
##' \code{lattice} function \code{xyplot} where it is applied to set
##' the plot symbol size.
##' @export
##' @return NULL
##' @seealso \code{\link{importTraj}} to import trajectory data from the King's
##' College server.
##' @author David Carslaw
##' @examples
##'
##' # show a simple case with no pollutant i.e. just the trajectories
##' # let's check to see where the trajectories were coming from when
##' # Heathrow Airport was closed due to the Icelandic volcanic eruption
##' # 15--21 April 2010.
##' # import trajectories for London and plot
##' \dontrun{
##' lond <- importTraj("london", 2010)
##' # well, HYSPLIT seems to think there certainly were conditions where trajectories
##' # orginated from Iceland...
##' trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"), plot.type = "l")}
##'
##' # plot by day, need a column that makes a date
##' \dontrun{
##' lond$day <- as.Date(lond$date)
##' trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"), plot.type = "l",
##' type = "day")
##' }
##'
##' # or show each day grouped by colour, with some other options set
##' \dontrun{
##'  trajPlot(selectByDate(lond, start = "15/4/2010", end = "21/4/2010"), plot.type = "l",
##' group = "day", col = "jet", lwd = 2, key.pos = "right", key.col = 1)
##' }
##' # more examples to follow linking with concentration measurements...
##'
##' # import some measurements from KC1 - London
##' \dontrun{
##' kc1 <- importAURN("kc1", year = 2010)
##' # now merge with trajectory data by 'date'
##' lond <- merge(lond, kc1, by = "date")
##'
##' # trajectory plot, no smoothing - and limit lat/lon area of interest
##' trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20), pollutant = "pm10")
##'
##' # can smooth surface:
##' trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20), pollutant = "pm2.5",
##' smooth = TRUE)
##'
##' # plot by season:
##' trajLevel(subset(lond, lat > 40 & lat < 70 & lon >-20 & lon <20), pollutant = "pm2.5",
##' smooth = TRUE, type = "season")
##' }
trajLevel <- function(mydata, lon = "lon", lat = "lat",
                      pollutant = "pm10", type = "default", smooth = FALSE,
                      statistic = "mean", percentile = 90,
                      map = TRUE, lon.inc = 1.5, lat.inc = 1.5, min.bin = 1, ...)  {

    ## mydata can be a list of several trajectory files; in which case combine them
    ## before averaging

    method <- "level"
    if (is.list(mydata)) mydata <- rbind.fill(mydata)

    mydata <- cutData(mydata, type, ...)

    ## bin data
    mydata$ygrid <- round_any(mydata[ , lat], lat.inc)
    mydata$xgrid <- round_any(mydata[ , lon], lon.inc)

    rhs <- c("xgrid", "ygrid", type)
    rhs <- paste(rhs, collapse = "+")
    mydata <- mydata[ , c("date", "xgrid", "ygrid", type, pollutant)]
    ids <- which(names(mydata) %in% c("xgrid", "ygrid", type))

    ## plot mean concentration
    if (statistic == "mean") {
        counts <-  aggregate(mydata[ , -ids], mydata[ , ids],
                             function (x)  length(unique(x)))
        mydata <- aggregate(mydata[ , -ids], mydata[ , ids], mean, na.rm = TRUE)
        mydata$count <- counts$date
        mydata <- subset(mydata, count >= min.bin)
        attr(mydata$date, "tzone") <- "GMT"  ## avoid warning messages about TZ
    }

    ## plot trajectory frequecies
    if (statistic == "frequency") {
        ## count % of times a cell contains a trajectory
        n <- length(unique(mydata$date))
        ## number in each bin
        counts <-  aggregate(mydata[ , -ids], mydata[ , ids],
                             function (x)  length(unique(x)))

        ## need dates for later processing e.g. for type = "season"
        dates <- aggregate(mydata[ , -ids], mydata[ , ids], mean, na.rm = TRUE)
        dates <- dates$date

        mydata <- aggregate(mydata[ , -ids], mydata[ , ids],
                            function (x) 100 * length(unique(x)) / n)


        mydata[, pollutant] <- mydata[, "date"]
        mydata$count <- counts$date
        mydata$date <- dates
        attr(mydata$date, "tzone") <- "GMT"  ## avoid warning messages about TZ
        mydata <- subset(mydata, count >= min.bin)
    }

    ## plot trajectory frequecy differences e.g. top 10% concs cf. mean
    if (statistic == "difference") {
        ## count % of times a cell contains a trajectory
        n1 <- length(unique(mydata$date))
        dat1 <- aggregate(mydata[ , -ids], mydata[ , ids],
                          function (x) 100 * length(unique(x)) / n1)
        dat1[, pollutant] <- dat1[, "date"]
        dat1 <- subset(dat1, select = -date)

        ## select top X percent
        Q90 <- quantile(mydata[, pollutant], probs = percentile / 100, na.rm = TRUE)

        ## now select trajectories with conc > percentile
        dat2 <- subset(mydata, get(pollutant) > Q90)
        n2 <- length(unique(dat2$date))
        ## number in each bin
        counts <-  aggregate(dat2[ , -ids], dat2[ , ids],
                             function (x)  length(unique(x)))

        ## need dates for later processing e.g. for type = "season"
        dates <- aggregate(dat2[ , -ids], dat2[ , ids], mean, na.rm = TRUE)
        dates <- dates$date

        dat2 <- aggregate(dat2[ , -ids], dat2[ , ids],
                          function (x) 100 * length(unique(x)) / n2)
        dat2[, pollutant] <- dat2[, "date"]
        dat2$count <- counts$date
        dat2$date <- dates
        attr(dat2$date, "tzone") <- "GMT"  ## avoid warning messages about TZ
        dat2 <- subset(dat2, count >= min.bin)

        ## differences
        mydata <- merge(dat1, dat2, by = c("xgrid", "ygrid", type))
        pol1 <- paste(pollutant, ".x", sep = "")
        pol2 <- paste(pollutant, ".y", sep = "")
        mydata[, pollutant] <-  mydata[, pol2] - mydata[, pol1]

    }



    ## change x/y names to gridded values
    lon <- "xgrid"
    lat <- "ygrid"

    ## extra.args
    extra.args <- list(...)

    ## aspect
 #   if(!"aspect" %in% names(extra.args))
   #     extra.args$aspect <- 1

    if(!"ylab" %in% names(extra.args))
        extra.args$ylab <- "latitude"

    if(!"xlab" %in% names(extra.args))
        extra.args$xlab <- "longitude"

     if(!"main" %in% names(extra.args))
        extra.args$main <- ""

    if(!"key.header" %in% names(extra.args)) {
        if (statistic == "frequency") extra.args$key.header <- "% trajectories"
        if (statistic == "difference") extra.args$key.header <- quickText(paste("gridded differences", "\n(", percentile, "th percentile)", sep = ""))
    }

     if(!"key.footer" %in% names(extra.args))
         extra.args$key.footer <- ""

    extra.args$trajStat <- statistic

    ## the plot
    scatterPlot.args <- list(mydata, x = lon, y = lat, z = pollutant, type = type,
                             method = method, smooth = smooth, map = map,
                             x.inc = lon.inc, y.inc = lat.inc)

    ## reset for extra.args
    scatterPlot.args <- openair:::listUpdate(scatterPlot.args, extra.args)

    ## plot
    do.call(scatterPlot, scatterPlot.args)

}


##' @rdname trajPlot
##' @param group For \code{trajPlot} it is sometimes useful to group
##' and colour trajectories according to a grouping variable. See example below.
##' @export
trajPlot <- function(mydata, lon = "lon", lat = "lat", pollutant = "pm10", type = "default",
                     smooth = FALSE, statistic = "mean", percentile = 90, map = TRUE, lon.inc = 1.5,
                     lat.inc = 1.5, min.bin = 1, group = NA, ...)
{

    ##extra.args
    extra.args <- list(...)
    method <- "scatter"

    #aspect, cex
    if(!"aspect" %in% names(extra.args))
        extra.args$aspect <- 1
    if(!"cex" %in% names(extra.args))
        extra.args$cex <- 0.1

    if(!"ylab" %in% names(extra.args))
        extra.args$ylab <- "latitude"

    if(!"xlab" %in% names(extra.args))
        extra.args$xlab <- "longitude"


    if (missing(pollutant)) { ## don't need key

        if (is.na(group)) key <- FALSE else key <- TRUE

        if(!"main" %in% names(extra.args))
             extra.args$main <- NULL

        scatterPlot.args <- list(mydata, x = lon, y = lat, z = NA, type = type, method = method,
                                 smooth = smooth, map = map, x.inc = lon.inc, y.inc = lat.inc,
                                 key = key, group = group)

    } else {
         if(!"main" %in% names(extra.args))
             extra.args$main <- pollutant

        scatterPlot.args <- list(mydata, x = lon, y = lat, z = pollutant, type = type, method = method,
                                 smooth = smooth, map = map, x.inc = lon.inc, y.inc = lat.inc,
                                 group = group)
    }

    #reset for extra.args
    scatterPlot.args<- listUpdate(scatterPlot.args, extra.args)

    #plot
    do.call(scatterPlot, scatterPlot.args)



}
