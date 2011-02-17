
polarAnnulus <- function(polar,
                          pollutant = "nox",
                          resolution = "fine",
                          local.time = FALSE,
                          period = "hour",
                          type = "default",
                          limits = c(0, 100),
                          cols = "default",
                          width = "normal",
                          exclude.missing = TRUE,
                          date.pad = FALSE,
                          force.positive = TRUE,
                          k = 15,
                         normalise = FALSE,
                          main = "",
                          key.header = "",
                          key.footer = pollutant,
                          key.position = "right",
                          key = NULL,
                          auto.text = TRUE,...) {


    ## extract variables of interest
    vars <- c("wd", "date", pollutant)

    if (any(c("hour", "weekday", "season", "trend") %in% type)) stop ("Cannot have same type and period.")
    if (length(type) > 2) stop("Cannot have more than two types.")

    ## check data
    polar <- checkPrep(polar, vars, type)

    ## if more than one pollutant, need to stack the data and set type = "variable"
    ## this case is most relevent for model-measurement compasrions where data are in columns
    if (length(pollutant) > 1) {
        polar <- melt(polar, measure.vars = pollutant)
        ## now set pollutant to "value"
        pollutant <- "value"
        type <- "variable"       
    }

    if (period == "trend" && missing(k)) k <- 20 ## less smoothing for the trend component

    d <- 10      ## d + upper = 1/2 width of annulus; do not need to adjust

    if (width == "normal") upper <- 10
    if (width == "thin") upper <- 15
    if (width == "fat") upper <- 5

    ## add extra wds - reduces discontinuity at 0/360
    zero.wd <- subset(polar, wd == 360)
    zero.wd$wd <- 0
    polar <- rbind(polar, zero.wd)

    ## remove NAs
    polar <- na.omit(polar)
    polar <- cutData(polar, type, ...)

    ## convert to local time
    if (local.time) polar$date <- as.POSIXct(format(polar$date, tz = "Europe/London"))

    ## for resolution of grid plotting (default = 0.2; fine = 0.1)
    if (resolution == "normal") int <- 0.2
    if (resolution == "fine") int <- 0.1
    if (resolution == "ultra.fine") int <- 0.05  # very large files!

    len.int <- 20 / int + 1 ## number of x and y points to make up surfacexb

    prepare.grid <- function(polar) {

        ## for padding to beginning of first year, end of last year
        if (date.pad) {

            min.year <- as.numeric(format(min(polar$date, na.rm = TRUE), "%Y"))
            max.year <- as.numeric(format(max(polar$date, na.rm = TRUE), "%Y"))

            all.dates <- data.frame(date = seq(ISOdate(min.year, 1, 1, 0, 0, 0, tz = "GMT"),
                                    ISOdate(max.year, 12, 31, 23, 0, 0, tz = "GMT"),
                                    by = "hour"))

            all.dates <- data.frame(date = all.dates)
        }

        ## if period = trend but less than 1 year of data then force season
       # if (polar$date[nrow(polar)] - polar$date[1] < 366 & period == "trend") period = "season"

        ## different date components, others available
        if (period == "trend")
        {
            if (date.pad) {
                ## for new limits with padding
                day <- as.numeric(format(all.dates$date, "%j"))
                year <- as.numeric(format(all.dates$date, "%Y"))
                trend2 <- year + day / 366
                min.trend <- min(trend2, na.rm = TRUE)
                max.trend <- max(trend2, na.rm = TRUE)

                ## actual data
                day <- as.numeric(format(polar$date, "%j"))
                year <- as.numeric(format(polar$date, "%Y"))
                trend <- year + day / 366

            } else {

                year <- as.numeric(format(polar$date, "%Y"))
                day <- as.numeric(format(polar$date, "%j"))
                trend <- year + day / 366
                min.trend <- min(trend, na.rm = TRUE)
                max.trend <- max(trend, na.rm = TRUE)
            }
        }

        if (period == "weekday")
        {
            hour <- as.numeric(format(polar$date, "%H"))
            weekday <- as.numeric(format(polar$date, "%w"))
            trend <- weekday + hour / 23
            min.trend = 0
            max.trend = 7
        }

        if (period == "season")
        {
            week <- as.numeric(format(polar$date, "%W"))
            trend <- week
            min.trend = 0
            max.trend = 53
        }

        if (period == "hour")
        {
            hour <- as.numeric(format(polar$date, "%H"))
            trend <- hour
            min.trend = 0
            max.trend = 23
        }

        trend <- 10 * (trend - min.trend) / (max.trend - min.trend)

        polar <- cbind(polar, trend)

        time.seq <- seq(0, 10, length = 24)

        wd <- seq(from = 0, to = 360, 10) #wind directions from 10 to 360
        ws.wd <- expand.grid(time.seq = time.seq, wd = wd)


        ## identify which ws and wd bins the data belong
        ## wd.cut <- cut(polar$wd, seq(0, 360, 10))
        wd.cut <- cut(polar$wd, seq(0, 360, length = 38), include.lowest = TRUE)

        ## divide-up the data for the annulus
        time.cut <- cut(polar$trend, seq(0, 10, length = 25), include.lowest = TRUE)

        binned <- tapply(polar[, pollutant], list(time.cut, wd.cut), mean, na.rm = TRUE)
        binned <- as.vector(binned)

        ## data to predict over
        time.seq <- ws.wd$time.seq
        wd <- ws.wd$wd

        input.data <- expand.grid(time.seq = seq(0, 10, length = len.int),
                                  wd = seq(0, 360, length = len.int))
######################Smoothing#################################################

        ## run GAM to make a smooth surface
        if (force.positive) n <- 0.5 else n <- 1

        ## note use of cyclic smooth for the wind direction component
        Mgam <- gam(binned ^ n ~ te(time.seq, wd, k = k, bs = c("tp", "cc")))


        pred <- predict.gam(Mgam, input.data)
        pred <- pred ^ (1 / n)

        input.data <- cbind(input.data, pred)

        if (exclude.missing) {

            ## exclude predictions too far from data (from mgcv)
            x <- seq(0, 10, length = len.int)
            y <- seq(0, 360, length = len.int)
            res <- len.int
            wsp <- rep(x, res)
            wdp <- rep(y, rep(res, res))

            ind <- exclude.too.far(wsp, wdp, polar$trend, polar$wd, dist = 0.03)

            input.data$pred[ind] <- NA
            pred <- input.data$pred
        }

#############################################################################
        ## transform to new coords - probably more efficient way of doing this
        ## need to transform to/from annulus to a cartesian coords

        ## new grid for plotting (the annulus)
        new.data <- expand.grid(u = seq(-upper - d, upper + d, by = int),
                                v = seq(-upper - d, upper + d, by = int), z = NA, wd = NA,
                                time.val = NA)

        new.data$id <- seq(nrow(new.data))

        ## calculate wd and time.val in on annulus in original units (helps with debugging)
        ## essentially start with final grid (annulus) and work backwards to find original data point in
        ## orginal coordinates
        new.data <- within(new.data, time.val <- (u ^ 2 + v ^ 2) ^ 0.5  - upper)
        new.data <- within(new.data, wd <- 180 * atan2(u , v) / pi)
        new.data <- within(new.data, wd[wd < 0] <- wd[wd < 0] + 360)  ## correct negative wds

        ## remove ids outside range
        ids <- with(new.data, which((u ^ 2 + v ^ 2) ^ 0.5 > d + upper | (u ^ 2 + v ^ 2) ^ 0.5
                                    < upper))
        new.data$wd[ids] <- NA
        new.data$time.val[ids] <- NA

        ## ids where there are data
        ids <- new.data$id[!is.na(new.data$wd)]

        ## indexes in orginal (cartesian) coordinates
        id.time <- round((2 * d / int) * new.data$time.val[ids] / 10) + 1
        id.wd <- round((2 * d / int) * new.data$wd[ids] / 360) + 1

        ## predicted values as a matrix
        pred <- matrix(pred, nrow = len.int, ncol = len.int)

        ## match the ids with predictions
        new.data$z[ids] <- sapply(seq_along(ids), function(x) pred[id.time[x], id.wd[x]])
        new.data
    }

    ## more compact way?  Need to test
     results.grid <- ddply(polar, type, prepare.grid)

    ## normalise by divining by mean conditioning value if needed
    if (normalise){
        results.grid <- ddply(results.grid, type, transform, z = z / mean(z, na.rm = TRUE))
        if (missing(key.footer)) key.footer <- "normalised \nlevel"
    }

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

    
    ## auto-scaling
    nlev = 200  #preferred number of intervals
    ## handle missing breaks arguments
    if(missing(limits)) breaks <- pretty(results.grid$z, n = nlev) else breaks <- pretty(limits,
                                                         n = nlev)

    nlev2 = length(breaks)

    col <- openColours(cols, (nlev2 - 1))
    col.scale = breaks

   

    #################
    ## scale key setup
    #################
    legend <- list(col = col, at = col.scale, space = key.position, 
         auto.text = auto.text, footer = key.footer, header = key.header, 
         height = 1, width = 1.5, fit = "all")
    if (!is.null(key)) 
         if (is.list(key)) 
             legend[names(key)] <- key
         else warning("In polarAnnulus(...):\n  non-list key not exported/applied\n  [see ?drawOpenKey for key structure/options]", 
             call. = FALSE)
    legend <- list(temp = list(fun = drawOpenKey, args = list(key = legend, 
         draw = FALSE)))
    names(legend)[1] <- if(is.null(key$space)) key.position else key$space

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("z ~ u * v | ", temp, sep = ""))
    
    plt <- levelplot(myform, results.grid, axes = FALSE,
              as.table = TRUE,
              aspect = 1,
              xlab = "",
              ylab = "",
              main = quickText(main, auto.text),
              colorkey = FALSE, legend = legend,
              at = col.scale, col.regions = col,
              par.strip.text = list(cex = 0.8),
              scales = list(draw = FALSE),
              strip = strip,

              len <- upper + d + 3,
              xlim = c(-len, len), ylim = c(-len, len),...,

              panel = function(x, y, z,subscripts,...) {
                  panel.levelplot(x, y, z, subscripts, at = col.scale,
                                  lwd = 1, col.regions = col, labels = FALSE)

                  ## add axis line to central polarPlot
                  llines(c(upper, upper + d), c(0, 0), col = "grey20")
                  llines(c(0, 0), c(-upper, -upper - d), col = "grey20")

                  ## add axis line to central polarPlot
                  llines(c(0, 0), c(upper, upper + d), col = "grey20")
                  llines(c(-upper, -upper - d), c(0, 0), col = "grey20")

                  add.tick <- function(n, start = 0, end = 0) {
                      ## start is an offset if time series does not begin in January

                      ## left
                      lsegments(seq(-upper - start, -upper - d + end, length = n),
                                rep(-.5, n),
                                seq(-upper - start, -upper - d + end, length = n),
                                rep(.5, n), col = "grey20")
                      ## top
                      lsegments(rep(-.5, n),
                                seq(upper + start, upper + d - end, length = n),
                                rep(.5, n),
                                seq(upper + start, upper + d - end, length = n))
                      ## right
                      lsegments(seq(upper + start, upper + d - end, length = n),
                                rep(-.5, n),
                                seq(upper + start, upper + d - end, length = n),
                                rep(.5, n), col = "grey20")
                      ## bottom
                      lsegments(rep(-.5, n),
                                seq(-upper - start, -upper - d + end, length = n),
                                rep(.5, n),
                                seq(-upper - start, -upper - d + end, length = n))
                  }

                  label.axis <- function(x, lab1, lab2, ticks)
                  {
                      ltext(x, upper, lab1, cex = 0.7, pos = 4)
                      ltext(x, upper + d, lab2, cex = 0.7, pos = 4)
                      ## at bottom
                      ltext(-x, -upper, lab1, cex = 0.7, pos = 2)
                      ltext(-x, -upper - d, lab2, cex = 0.7, pos = 2)
                      add.tick(ticks)
                  }

                  if (period == "trend")
                  {
                      if (date.pad) {
                          date.start <- min(all.dates$date)
                          date.end <- max(all.dates$date)
                      } else {
                          date.start <- polar$date[1]
                          date.end <- polar$date[nrow(polar)]
                      }

                      label.axis(0, format(date.start, "%b-%Y"), format(date.end, "%b-%Y"), 2)

                      ## add ticks at 1-Jan each year (could be missing dates)
                      days <- seq(as.Date(date.start), as.Date(date.end), by = "day")

                      ## find number of Januarys
                      num.jans <- which(format(days, "%j") == "001")
                      ticks <- length(num.jans)
                      start <- 10 * (num.jans[1] - 1) / length(days)
                      end <- 10 - 10 * (num.jans[ticks] - 1) / length(days)
                      add.tick(ticks, start, end)
                  }

                  if (period == "season") label.axis(0, format(ISOdate(2000, 1, 1), "%B"), format(ISOdate(2000, 12, 1), "%B"), 13)

                  if (period == "hour")	label.axis(0, "0", "23", 7)

                  if (period == "weekday") {
                      local.weekdays <- format(ISOdate(2000, 1, 1:14), "%A")[order(format(ISOdate(2000, 1, 1:14), "%w"))]
                      loc.sunday <- local.weekdays[1]
                      loc.saturday <- local.weekdays[length(local.weekdays)]
                      label.axis(0, loc.sunday, loc.saturday, 8)
                  }

                  ## text for directions
                  ltext(-upper -d - 1.5, 0, "W", cex = 0.7)
                  ltext(0, -upper - d - 1.5, "S", cex = 0.7)
                  ltext(0, upper + d + 1.5, "N", cex = 0.7)
                  ltext(upper + d + 1.5, 0, "E", cex = 0.7)

              })

    #################
    #output
    #################
    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- results.grid
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)  

}


