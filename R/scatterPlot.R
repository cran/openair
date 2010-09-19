
scatterPlot <- function(mydata,
                         x = "nox",
                         y = "no2",
                         method = "scatter",
                         group = FALSE,
                         avg.time = "default",
                         data.thresh = 0,
                         statistic = "mean",
                         percentile = 95,
                         type = "default",
                         layout = c(1, 1),
                         smooth = TRUE,
                         linear = FALSE,
                         ci = TRUE,
                         cols = "hue",
                         main = "",
                         ylab = y,
                         xlab = x,
                         pch = 1,
                         lwd = 1,
                         key = TRUE,
                         key.title = type,
                         key.columns = 1,
                         strip = TRUE,
                         log.x = FALSE,
                         log.y = FALSE,
                         nbin = 256,
                         continuous = FALSE,
                         auto.text = TRUE, ...)   {

    ## basic function to plot single/multiple time series in flexible waysproduce scatterPlot
    ## Author: David Carslaw 27 Jan. 10
    ## method = scatter/hexbin/kernel
   
    
    x.nam <- x ## names of pollutants for linear model equation
    y.nam <- y

### For Log scaling (adapted from lattice book) ###############################################
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
################################################################################################
    if (type %in%  c("year", "hour", "month", "season", "weekday", "weekend", "monthyear", "gmtbst", "bstgmt") | !missing(avg.time)) {

        vars <- c("date", x, y)

    } else {

        vars <- c(x, y)
    }

    ## data checks
    mydata <- checkPrep(mydata, vars, type)

    ## average the data if necessary (default does nothing)
    if (avg.time != "default") mydata <- timeAverage(mydata, period = avg.time,
        data.thresh = data.thresh, statistic = statistic, percentile = percentile)

    ## remove missing data
    mydata <- na.omit(mydata)

    ## continuous colors
    if (continuous & method == "scatter") {
        ## check to see if type is numeric/integer
        if (class(mydata[, type]) %in% c("integer", "numeric") == FALSE) stop(paste("Continuous colour coding requires ", type , " to be numeric", sep = ""))

        ## define spectrum of colours
        if (missing(cols)) cols <- "default" ## better default colours for this
        thecol <- openColours(cols, 100)[cut(mydata[, type], 100, label = FALSE)]

        min.col <- min(mydata[, type], na.rm = TRUE)
        max.col <- max(mydata[, type], na.rm = TRUE)
        mydata$cond <- "default"

        if (missing(pch)) pch <- 16

        if (missing(main)) main <- paste(x, "vs.", y, "by levels of", type)
        key <- FALSE
        group <- TRUE
        legend <- list(right = list(fun = draw.colorkey, args =
                       list(key = list(col = openColours(cols, 100),
                            at = seq(min.col, max.col, length = 100)),
                            draw = FALSE)))
    } else {

        mydata <- cutData(mydata, type)
        legend <- NULL

    }

    ## The aim to to get colums "date", "site" then turn to column data using melt
    ## Finally end up with "date", "value", "variable"

    ## don't need type, now a condition
    vars <-  c(vars, "cond")
    vars <- vars[vars != type]
    mydata <- mydata[, vars]
    mydata <- rename(mydata, c(cond = "site")) ## change to name "site"

    
    theStrip <- strip

    ## number of pollutants (or sites for type = "site")
    npol <- length(unique(mydata$site)) ## number of pollutants
    if (missing(pch)) pch <- seq(npol)

    ## layout - stack vertically
    if (missing(layout) & !group) layout <- NULL 

    ## set up colours
    myColors <- openColours(cols, npol)

    ## basic function for lattice call + defaults
    myform <- formula(paste(y, "~", x))

    scales <- list(x = list(log = nlog.x), y = list(log = nlog.y))

    if (x == "date") { ## get proper date scaling
        date.breaks <- 7
        dates <- dateBreaks(mydata$date, date.breaks)$major ## for date scale
        xlim <- range(mydata$date)
        scales = list(x = list(at = dateBreaks(mydata$date, date.breaks)$major, format =
                                                         dateBreaks(mydata$date)$format,
                      relation = "sliced"),
         y = list(log = nlog.y))
    }

    pol.name <- sapply( unique(levels(factor(mydata$site))), function(x) quickText(x, auto.text))

    ## if logs are chosen, ensure data >0 for line fitting etc
    if (log.x)  mydata <- mydata[mydata[ , x] > 0, ]
    if (log.y)  mydata <- mydata[mydata[ , y] > 0, ]

    ## layout changes depening on plot type

    if (!group) { ## sepate panels per pollutant

        ## now need conditioning formula
        myform <- formula(paste(y, "~", x, "| site"))
        ## proper names of labelling
        strip <- strip.custom(par.strip.text = list(cex = 0.8), factor.levels = pol.name)
        scales <- list(y = list(rot = 0, log = nlog.y), x = list(log = nlog.x))
    }

    if (key & type != "default") {
        if (missing(key.columns)) if (npol < 5) key.columns <- npol else key.columns <- 4

        key <- list(points = list(col = myColors[1:npol]), pch = pch,
                    text = list(lab = pol.name),  space = "bottom", columns = key.columns,
                    title = quickText(key.title, auto.text), cex.title = 1.2,
                    border = "grey")
    } else {
        key <- NULL ## either there is a key or there is not
    }

    if (!theStrip) strip <- FALSE

    ## special wd layout
    skip <- FALSE
    if (type == "wd" & !continuous) {
        layout <- c(3, 3)
        skip <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
    }

    if (method == "scatter") {


        pltscatter <- xyplot(myform,  data = mydata, groups = site,
                             as.table = TRUE,
                             pch = pch,
                             main = quickText(main),
                             ylab = quickText(ylab, auto.text),
                             xlab = quickText(xlab, auto.text),
                             scales = scales,
                             key = key,
                             strip = strip,
                             layout = layout,
                             skip = skip,
                             yscale.components = yscale.components.log10,
                             xscale.components = xscale.components.log10,
                             legend = legend,
                             panel =  panel.superpose,...,
                             panel.groups = function(x, y, col.symbol, col, col.line, lwd, lty, type,
                             group.number,
                             subscripts,...) {
                                 if (group.number == 1 & x.nam != "date") panel.grid(-1, -1)
                                 if (group.number == 1 & x.nam == "date") {
                                     panel.abline(v = dates, col = "grey90")
                                      panel.grid(-1, 0)
                                 }
                                 if (!group & x.nam != "date") panel.grid(-1, -1)
                                 if (!group & x.nam == "date") {
                                     panel.abline(v = dates, col = "grey90")
                                      panel.grid(-1, 0)
                                 }

                                 if (continuous) panel.xyplot(x, y, col.symbol =
                                                              thecol[subscripts],
                                                              as.table = TRUE,...)
                                 if (!continuous) panel.xyplot(x, y, col.symbol =
                                                               myColors[group.number],
                                                               as.table = TRUE,...)
                                 if (smooth) panel.gam(x, y, col = myColors[group.number],
                                                       col.se = "black",#myColors[group.number],
                                                       lty = 1, lwd = 1, se = ci, ...)
                                 if (linear) panel.linear(x, y, col = "black",myColors[group.number], lwd = 1,
                                                          lty = 5, x.nam = x.nam, y.nam = y.nam,
                                                          se = ci,  ...)
                             })
    }

    if (method == "hexbin") {
        library(hexbin)
        plthexbin <- hexbinplot(myform, data = mydata,
                                ylab = quickText(ylab, auto.text),
                                xlab = quickText(xlab, auto.text),
                                strip = strip,
                                as.table = TRUE,
                                xbins = 40,
                                colorkey = TRUE,
                                aspect = 1,
                                colramp = function(n) {openColours("default", n)},
                                trans = function(x) log(x), inv = function(x) exp(x),
                                panel = function(x,...) {
                                    panel.grid(-1, -1)
                                    panel.hexbinplot(x,...)
                                })
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

            map <- grDevices:::.smoothScatterCalcDensity(x, nbin)
            xm <- map$x1
            ym <- map$x2

            dens <- map$fhat

            grid <- expand.grid(x = xm, y = ym)

            results <- data.frame(x = grid$x, y = grid$y, z = as.vector(dens), cond = subdata$site[1])
            results
        }

#############################################################################

        results.grid <-  ddply(mydata, .(site), prepare.grid)

        ## auto-scaling
        nlev <- 200  ## preferred number of intervals
        breaks <- pretty(results.grid$z, n = nlev)

        nlev2 <- length(breaks)

        col <- openColours("default", (nlev2 - 1))
        col <- c("transparent", col) ## add white at bottom
        col.scale <- breaks

        pltkernel <- levelplot(z ~ x * y | cond, results.grid,
                               as.table = TRUE,
                               ylab = quickText(ylab, auto.text),
                               xlab = quickText(xlab, auto.text),
                               strip = strip,
                               col.regions = col,
                               region = TRUE,
                               layout = layout,
                               at = col.scale,
                               colorkey = FALSE,
                               ...,

                               panel = function(x, y, z, subscripts,...) {
                                   panel.grid(-1, -1)
                                   panel.levelplot(x, y, z,
                                                   subscripts,
                                                   at = col.scale,
                                                   pretty = TRUE,
                                                   col.regions = col,
                                                   labels = FALSE)
                               })
    }
    if (method == "scatter") print(pltscatter)
    if (method == "hexbin") print(plthexbin)
    if (method == "density") print(pltkernel)
}



panel.linear <- function (x, y, form = y ~ x, method = "loess", x.nam, y.nam, ..., se = TRUE,
                          level = 0.95, n = 100, col = plot.line$col, col.se = col,
                          lty = plot.line$lty, lwd = plot.line$lwd, alpha = plot.line$alpha,
                          alpha.se = 0.25, border = NA, subscripts, group.number, group.value,
                          type, col.line, col.symbol, fill, pch, cex, font, fontface,
                          fontfamily)
{


    thedata <- data.frame(x = x, y = y)
    tryCatch({mod <- lm(y ~ x, data = thedata)

              lims <- current.panel.limits()
              xrange <- c(max(min(lims$x), min(x)), min(max(lims$x), max(x)))
              xseq <- seq(xrange[1], xrange[2], length = n)

              pred <- predict(mod, data.frame(x = xseq), interval = "confidence")

              if (se) {
                  ## predicts 95% CI by default
                  panel.polygon(x = c(xseq, rev(xseq)), y = c(pred[, 2], rev(pred[, 3])), col = col.se,
                                alpha = alpha.se, border = border)
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
              panel.text(x, y, quickText(paste(y.nam, "=", format(slope, digits = 2), "[", x.nam, "]", symb,
                                                format(intercept, digits = 2),
                                                " R2=",  format(r.sq, digits = 2),
                                                sep = "")), cex = 0.7, pos = 4)

          }, error = function(x) return)
}



