conditionalQuantile <- function(mydata, obs = "obs", mod = "mod",
                                type = "default",
                                bins = 31,
                                min.bin = c(10, 20),
                                xlab = "predicted value",
                                ylab = "observed value",
                                col = brewer.pal(5, "YlOrRd"),
                                key.columns = 2,
                                key.position = "bottom",
                                auto.text = TRUE, ...) {
    ## partly based on from Wilks (2005) and package verification, with many modifications
    require(latticeExtra)

    if (length(type) > 2) stop("Only two types can be used with this function")

    #greyscale handling
    if (length(col) == 1 && col == "greyscale") {
        #strip
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
        #other local colours
        ideal.col <- "black"
        col.1 <- grey(0.75)
        col.2 <- grey(0.5)
        col.5 <- grey(0.25)

    } else {
        ideal.col <- "#0080ff"
        col.1 <- col[1]
        col.2 <- col[2]
        col.5 <- col[5]
    }

    vars <- c(mod, obs)

    if (any(type %in%  dateTypes)) vars <- c("date", vars)

    ## check the data
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
    mydata <- na.omit(mydata)
    mydata <- cutData(mydata, type)


    procData <- function(mydata){
        mydata <- mydata[ , sapply(mydata, class) %in% c("numeric", "integer"), drop = FALSE]

        obs <- mydata[ , obs]
        pred <- mydata[ , mod]
        min.d <- min(mydata)
        max.d <- max(mydata)
        bins <- seq(floor(min.d), ceiling(max.d), length = bins)

        lo <- min(bins)
        hi <- max(bins)
        b <- bins[-length(bins)]
        labs <- b + 0.5 * diff(bins)
        obs.cut <- cut(obs, breaks = bins, include.lowest = TRUE,
                       labels = labs)
        obs.cut[is.na(obs.cut)] <- labs[1]
        obs.cut <- as.numeric(as.character(obs.cut))
        frcst.cut <- cut(pred, breaks = bins, include.lowest = TRUE,
                         labels = labs)
        frcst.cut[is.na(frcst.cut)] <- labs[1]
        frcst.cut <- as.numeric(as.character(frcst.cut))
        n <- length(labs)
        lng <- aggregate(obs, by = list(length = frcst.cut), length)
        med <- aggregate(obs, by = list(med = frcst.cut), median)
        q1 <- aggregate(obs, by = list(q1 = frcst.cut), quantile, 0.25)
        q2 <- aggregate(obs, by = list(q2 = frcst.cut), quantile, 0.75)
        q1$x[lng$x <= min.bin[1]] <- NA
        q2$x[lng$x <= min.bin[1]] <- NA
        q3 <- aggregate(obs, by = list(q3 = frcst.cut), quantile, 0.1)
        q4 <- aggregate(obs, by = list(q4 = frcst.cut), quantile, 0.9)
        q3$x[lng$x <= min.bin[2]] <- NA
        q4$x[lng$x <= min.bin[2]] <- NA

        results <- data.frame(x = med$med, lng = lng$x, med = med$x, q1 = q1$x, q2 = q2$x,
                              q3 = q3$x, q4 = q4$x)
        results.cut <- data.frame(frcst.cut = frcst.cut)

        ## range taken by observations
        results.obs <- data.frame(min = min(obs), max = max(obs))
        results <- list(results, results.cut, results.obs)
        results
    }


    lo <- min(mydata[ , c(mod, obs)])
    hi <- max(mydata[ , c(mod, obs)])
    all.results <- dlply(mydata, type, procData)

    results <- ldply(all.results, function(x) rbind(x[[1]]))
    hist.results <- ldply(all.results, function(x) rbind(x[[2]]))
    obs.results <- ldply(all.results, function(x) rbind(x[[3]]))

    ## proper names of labelling ##############################################################################
    pol.name <- sapply(levels(results[ , type[1]]), function(x) quickText(x, auto.text))
    strip <- strip.custom(factor.levels = pol.name)

    if (length(type) == 1 ) {

        strip.left <- FALSE
        if (type == "default") strip <- FALSE

    } else { ## two conditioning variables

        pol.name <- sapply(levels(results[ , type[2]]), function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels = pol.name)
    }
#######################################################################################


    ## polygon that can deal with missing data
    poly.na <- function(x1, y1, x2, y2, col) {
        for(i in seq(2, length(x1)))
            if (!any(is.na(y2[c(i - 1, i)])))
                lpolygon(c(x1[i - 1], x1[i], x2[i], x2[i - 1]),
                         c(y1[i - 1], y1[i], y2[i], y2[i - 1]),
                         col = col, border = NA, alpha = 1)
    }

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("x ~ med | ", temp, sep = ""))
    if (!missing(xlab)) xlab <- xlab
    if (!missing(ylab)) ylab <- ylab

    scatter <- xyplot(myform, data = results,
                      xlim = c(lo, hi),
                      ylim = c(lo, hi),
                      ylab = ylab,
                      xlab = xlab,
                      as.table = TRUE,
                      aspect = 1,
                      strip = strip,
                      strip.left = strip.left,
                      key = list(lines = list(col = c(col.1, col.2, col.5, ideal.col),
                                 lwd = c(15, 15, 2, 1)),
                      lines.title = 1, title = "", text = list(lab = c("25/75th percentile",
                                                               "10/90th percentile",
                                                               "median",
                                                               "perfect model")),
                      space = key.position,
                      columns = key.columns),
                      par.strip.text = list(cex = 0.8), ...,
                      panel = function(x, subscripts,  ...){
                          panel.grid (-1, -1, col = "grey95")

                          poly.na(results$x[subscripts], results$q3[subscripts],
                                  results$x[subscripts],
                                  results$q4[subscripts], col = col.2)
                          poly.na(results$x[subscripts], results$q1[subscripts],
                                  results$x[subscripts],
                                  results$q2[subscripts], col = col.1)

                          ## match type and get limits for obs
                          theType <- results[subscripts[1], type]

                          if (length(type) == 1) {
                              theSubset <- subset(obs.results, get(type) == theType)
                          } else {

                              theSubset <- obs.results[obs.results[type[1]] ==
                                                       as.character(theType[, 1]) &
                                                       obs.results[type[2]] ==
                                                       as.character(theType[, 2]) , ]

                          }

                          panel.lines(c(theSubset$min, theSubset$max), c(theSubset$min,
                                                                         theSubset$max),
                                      col = ideal.col, lwd = 1)
                          panel.lines(results$x[subscripts], results$med[subscripts],
                                      col = col.5, lwd = 2)

                      })

    temp <- paste(type, collapse = "+")
    myform <- formula(paste(" ~ frcst.cut | ", temp, sep = ""))
    bins <- seq(floor(lo), ceiling(hi), length = bins)

    histo <- histogram(myform, data = hist.results, breaks = bins, type = "count",
                       as.table = TRUE,
                       strip = strip,
                       strip.left = strip.left,
                       col = "black", alpha = 0.1, border = NA,
                       par.strip.text = list(cex = 0.8),
                       ylab = "sample size")

    thePlot <- doubleYScale(scatter, histo, add.ylab2 = TRUE)
    thePlot <- update(thePlot, par.settings = simpleTheme(col = c("black", "black")))

    if (length(type) == 1) plot(thePlot) else plot(useOuterStrips(thePlot, strip = strip,
              strip.left = strip.left))

    #reset if greyscale
    if (length(col) == 1 && col == "greyscale")
        trellis.par.set("strip.background", current.strip)

    invisible(trellis.last.object())

}

