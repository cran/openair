scatterPlot <- function(mydata,
                        x = "nox",
                        y = "no2",
                        z = NA,
                        method = "scatter",
                        group = NULL,
                        avg.time = "default",
                        data.thresh = 0,
                        statistic = "mean",
                        percentile = NA,
                        type = "default",
                        layout = NULL,
                        smooth = FALSE,
                        spline = FALSE,
                        linear = FALSE,
                        ci = TRUE,
                        mod.line = FALSE,
                        cols = "hue",
                        main = "",
                        ylab = y,
                        xlab = x,
                        pch = 1,
                        lwd = 1,
                        lty = 1,
                        plot.type = "p",
                        key = TRUE,
                        key.title = group,
                        key.columns = 1,
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
                        auto.text = TRUE, ...)   {

    ## basic function to plot single/multiple time series in flexible waysproduce scatterPlot
    ## Author: David Carslaw 27 Jan. 10
    ## method = scatter/hexbin/kernel

    x.nam <- x ## names of pollutants for linear model equation
    y.nam <- y
    thekey <- key

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        ## strip
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
        ## other local colours
        method.col <- "greyscale"
    } else {
        method.col <- "default"
    }


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


    ## average the data if necessary (default does nothing)
    ## note - need to average before cutting data up etc
    if (avg.time != "default") mydata <- timeAverage(mydata, avg.time = avg.time,
        data.thresh = data.thresh, statistic = statistic, percentile = percentile)

    ## the following makes sure all variables are present, which depends on 'group' and 'type'
    if (is.na(z) & method == "level") stop("Need to specify 'z' when using method = 'level'")

################################################################################################
    if (any(type %in%  openair:::dateTypes) | !missing(avg.time)) {

        vars <- c("date", x, y)

    } else {

        vars <- c(x, y)
    }

    ## if group is present, need to add that list of variables unless it is a pre-defined date-based one
    if (!missing(group)){

        if (group %in%  openair:::dateTypes | !missing(avg.time) | any(type %in% openair:::dateTypes)) {
            if (group %in%  openair:::dateTypes) {
                vars <- unique(c(vars, "date")) ## don't need group because it is defined by date
            } else {
                vars <- unique(c(vars, "date", group))
            }

        } else {
            vars <- unique(c(vars, group))
        }
    }

    if (!missing(group)) if (group %in% type) stop ("Can't have 'group' also in 'type'.")



    ## data checks

    if (!missing(z)) vars <- c(vars, z)
    mydata <- openair:::checkPrep(mydata, vars, type)

    ## remove missing data
    mydata <- na.omit(mydata)

    ## if x is a factor/character, then rotate axis labels for clearer labels
    x.rot <- 0
    if ("factor" %in% class(mydata[, x]) | "character"  %in% class(mydata[, x])) {
        x.rot <- 90
        mydata[, x] <- factor(mydata[, x])
    }



    ## continuous colors ###################################################################################################

    if (!missing(z) & method == "scatter") {
        if (z %in% openair:::dateTypes) stop("Colour coding requires 'group' to be continuous numeric variable'")

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

        breaks <- unique(c(0, pretty(mydata[ , z], 100)))
        br <- pretty((mydata[ , z] ^ thePower), n = 10)  ## breaks for scale

        min.col <- min(mydata[, z], na.rm = TRUE)
        max.col <- max(mydata[, z], na.rm = TRUE)

        if (missing(main)) main <- paste(x, "vs.", y, "by levels of", z)

        ## don't need to group by all levels - want one smooth etc
        group <- "NewGroupVar"
        mydata$NewGroupVar <- "NewGroupVar"

        if (missing(pch)) pch <- 16

        legend <- list(right = list(fun = draw.colorkey, args =
                       list(key = list(col = openColours(cols, length(breaks)),
                            at = breaks, labels = list(at = br ^ (1 / thePower), labels = br)),
                            draw = FALSE)))


    } else {

        mydata <- cutData(mydata, type, ...)
        if (missing(group)) {

            if ((!"group" %in% type) & (!"group" %in% c(x, y))) mydata$group <- factor("group")
            ## don't overwrite a
        } else {  ## means that group is there
            mydata <- cutData(mydata, group, ...)
        }

        legend <- NULL
    }

    ## if no group to plot, then add a dummy one to make xyplot work
    if (is.null(group)) {mydata$MyGroupVar <- factor("MyGroupVar"); group <-  "MyGroupVar"}

    ## number of groups
    npol <- length(levels(mydata[ ,group]))

    if (missing(pch)) pch <- seq(npol)

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

    if (missing(z)) { ## non-continuous key
        if (missing(key.columns)) if (npol < 5) key.columns <- npol else key.columns <- 4

        if (key & npol > 1) {
            if (plot.type == "p") {
                key <- list(points = list(col = myColors[1:npol]), pch = pch,
                            text = list(lab = pol.name, cex = 0.8),  space = "bottom", columns = key.columns,
                            title = quickText(key.title, auto.text), cex.title = 1,
                            border = "grey")
            }

            if (plot.type == "l") {
                key <- list(lines = list(col = myColors[1:npol], lty = lty, lwd = lwd),
                            text = list(lab = pol.name, cex = 0.8),  space = "bottom", columns = key.columns,
                            title = quickText(key.title, auto.text), cex.title = 1,
                            border = "grey")
            }

            if (plot.type == "b") {
                key <- list(points = list(col = myColors[1:npol]), pch = pch,
                            lines = list(col = myColors[1:npol], lty = lty, lwd = lwd),
                            text = list(lab = pol.name, cex = 0.8),  space = "bottom", columns = key.columns,
                            title = quickText(key.title, auto.text), cex.title = 1,
                            border = "grey")
            }


        } else {

            key <- NULL
        }
    }

    ## special wd layout
    skip <- FALSE
    if (length(type) == 1 & type[1] == "wd" ) {
        ## re-order to make sensible layout
        wds <-  c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
        mydata$wd <- ordered(mydata$wd, levels = wds)

        ## see if wd is actually there or not
        wd.ok <- sapply(wds, function (x) {if (x %in% unique(mydata$wd)) FALSE else TRUE })
        skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])

        mydata$wd <- factor(mydata$wd)  ## remove empty factor levels

        layout = if (type == "wd") c(3, 3) else NULL
    }

    ## proper names of labelling ##############################################################################

    stripName <- sapply(levels(mydata[ , type[1]]), function(x) quickText(x, auto.text))
    if (strip) strip <- strip.custom(factor.levels = stripName)

    if (length(type) == 1 ) {

        strip.left <- FALSE

    } else { ## two conditioning variables
        stripName <- sapply(levels(mydata[ , type[2]]), function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels =  stripName)
    }
    ## ########################################################################################################


    ## no strip needed for single panel
    if (length(type) == 1 & type[1]  == "default") strip <- FALSE

    ## not sure how to evaluate "group" in xyplot, so change to a fixed name
    id <- which(names(mydata) == group)
    names(mydata)[id] <- "MyGroupVar"

    if (method == "scatter") {
        plt <- xyplot(myform,  data = mydata, groups = MyGroupVar,
                      type = c("p", "g"),
                      as.table = TRUE,
                      pch = pch,
                      lwd = lwd,
                      lty = lty,
                      main = quickText(main, auto.text),
                      ylab = quickText(ylab, auto.text),
                      xlab = quickText(xlab, auto.text),
                      scales = scales,
                      key = key,
                      par.strip.text = list(cex = 0.8),
                      strip = strip,
                      strip.left = strip.left,
                      layout = layout,
                      skip = skip,
                      yscale.components = yscale.components.log10,
                      xscale.components = xscale.components.log10,
                      legend = legend,
                      panel =  panel.superpose,...,
                      panel.groups = function(x, y, col.symbol, col, type, col.line, lty, lwd,
                      group.number,
                      subscripts,...)
                  {


                      if (!is.na(z)) panel.xyplot(x, y, col.symbol = thecol[subscripts],
                                                  as.table = TRUE, ...)

                      if (is.na(z)) panel.xyplot(x, y, type = plot.type,
                                                 col.symbol = myColors[group.number],
                                                 col.line = myColors[group.number], lty = lty, lwd = lwd,
                                                 as.table = TRUE,...)

                      if (linear & npol == 1) panel.linear(x, y, col = "black", myColors[group.number],
                                   lwd = 1, lty = 5, x.nam = x.nam, y.nam = y.nam, se = ci,  ...)

                      if (smooth) panel.gam(x, y, col = "grey20", col.se = "black",
                                            lty = 1, lwd = 1, se = ci, ...)
                      if (spline) panel.smooth.spline(x, y, col = myColors[group.number], lwd = lwd, ...)

                      if (mod.line) {
                          panel.abline(a = c(0, 0.5), lty = 5)
                          panel.abline(a = c(0, 2), lty = 5)
                          panel.abline(a = c(0, 1), lty = 1)
                      }


                      ## add reference lines
                      panel.abline(v = ref.x, lty = 5)
                      panel.abline(h = ref.y, lty = 5)


                  })
    }

    if (method == "hexbin") {
        library(hexbin)
        plt <- hexbinplot(myform, data = mydata,
                          ylab = quickText(ylab, auto.text),
                          xlab = quickText(xlab, auto.text),
                          strip = strip,
                          as.table = TRUE,
                          main = quickText(main, auto.text),
                          xbins = 40,
                          par.strip.text = list(cex = 0.8),
                          colorkey = TRUE,
                          aspect = 1,
                          colramp = function(n) {openColours(method.col, n)},  #was "default"
                          trans = function(x) log(x), inv = function(x) exp(x),...,
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
            new.data <- expand.grid(xgrid = seq(min(mydata$xgrid), max(mydata$xgrid), length = res),
                                    ygrid = seq(min(mydata$ygrid), max(mydata$ygrid), length = res))

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
            ind <- with(all.data, exclude.too.far(wsp, wdp, mydata$xgrid, mydata$ygrid, dist = 0.05))

            new.data[ind, z] <- NA

            new.data
        }

        if (smooth) mydata <- ddply(mydata, type, smooth.grid, z)

        if (missing(main)) main <- paste(x, "vs.", y, "by levels of", z)

        ## basic function for lattice call + defaults
        temp <- paste(type, collapse = "+")
        myform <- formula(paste(z, "~ xgrid * ygrid |", temp, sep = ""))

        nlev <- 200

        breaks <- pretty(mydata[ , z], n = nlev)

        nlev2 <- length(breaks)

        if (missing(cols)) cols <- "default"
        col <- openColours(cols, (nlev2 - 1))

        col.scale <- breaks


        plt <- levelplot(myform, data = mydata,
                         ylab = quickText(ylab, auto.text),
                         xlab = quickText(xlab, auto.text),
                         strip = strip,
                         as.table = TRUE,
                         layout = layout,
                         skip = skip,
                         region = TRUE,
                         col.regions = col,
                         at = col.scale,
                         main = quickText(main, auto.text),
                         par.strip.text = list(cex = 0.8),
                         colorkey = TRUE,...,
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
                             ## add reference lines
                             panel.abline(v = ref.x, lty = 5)
                             panel.abline(h = ref.y, lty = 5)

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

        plt <- levelplot(myform, results.grid,
                         as.table = TRUE,
                         ylab = quickText(ylab, auto.text),
                         xlab = quickText(xlab, auto.text),
                         main = quickText(main, auto.text),
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
                             if (mod.line) {
                                 panel.abline(a = c(0, 0.5), lty = 5)
                                 panel.abline(a = c(0, 2), lty = 5)
                                 panel.abline(a = c(0, 1), lty = 1)
                             }
                             ## add reference lines
                             panel.abline(v = ref.x, lty = 5)
                             panel.abline(h = ref.y, lty = 5)
                         })
    }


    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- mydata
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

                                        #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)

    invisible(output)

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



