
linearRelation <- function(mydata,
                            x = "nox",
                            y = "no2",
                            period = "monthly",
                            condition = FALSE,
                            n = 20,
                            rsq.thresh = 0,
                            ylim = c(0, 20),
                            ylab = paste("slope from ", y, " = m.", x, " + c", sep = ""),
                            xlab = NULL,
                            auto.text = TRUE,
                            main = "",
                            span = 0.3,...) {
  
    adj <- 1 ## factors for ratios (oxidant is a percentage)

    ## prepare data
    if ("ox" %in% tolower(c(x, y))) {
        vars <- c("date", "nox", "no2", "ox")
        mydata$ox <- mydata$no2 + mydata$o3
        mydata <- subset(mydata, nox > 0 & ox > 0)
        if (missing(ylab)) ylab <- "f-no2 (%) by vol."
        adj <- 100
    } else {
        vars <- c("date", x, y)
    }

    mydata <- checkPrep(mydata, vars, "default")
    mydata <- na.omit(mydata)

    if (!condition) {
        mydata$cond <- paste(format(min(mydata$date), "%d/%/%m/%Y"),
                             " to ", format(max(mydata$date), "%d/%/%m/%Y"))
    } else {   ## condition by year
        mydata$cond <- format(mydata$date, "%Y")
    }

    model <- function(df) { lm(eval(paste(y, "~", x)), data = df) }
    rsq <- function(x) summary(x)$r.squared
    seslope <-  function(x) { if (nrow(summary(x)$coefficients) == 2) {
        2 * summary(x)$coefficients[2, 2]  ## 95 % CI; need two rows in coefs
    } else {
        NA
    }}
    len <-  function(x) nrow(x$model)

    ## y range taking account of expanded uncertainties
    rng <- function(x) {
        lims <- range(c(x$slope - x$seslope, x$slope + x$seslope), na.rm = TRUE)
        inc <- 0.04 * abs(lims[2] - lims[1])
        lims <- c(lims[1] - inc, lims[2] + inc)
        lims
    }
################################################################################################
    if (period == "hour") {

        if(is.null(xlab)) xlab <- "hour"
        models <- dlply(mydata, .(cond, hour = as.numeric(format(date, "%H"))), model)
        results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
        names(results) <- c("cond", "hour", "intercept", "slope", "rsquare", "seslope", "N")
        results$slope <- results$slope * adj
        results$seslope <- results$seslope * adj
        results <- subset(results, rsquare >= rsq.thresh & N >= n)

        eq <- formula(slope ~ hour)
        if (condition) eq <- formula(slope ~ hour | cond)
        if (missing(ylim)) ylim <- rng(results)

        plt <- xyplot(eq, data = results,
                      as.table = TRUE,
                      ylim = ylim,
                      xlab = xlab,
                      main = quickText(main, auto.text),
                      ylab = quickText(ylab, auto.text),
                      scales = list(x = list(at = c(0, 6, 12, 18, 23))),...,
                      panel = function(x, y, subscripts, ...) {
                          panel.grid(-1, 0)
                          panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                          panel.xyplot(x, y, col = "#3366FF", pch = 16,...)
                          panel.segments(x, y - results$seslope[subscripts], x,
                                         y + results$seslope[subscripts], col = "#3366FF",
                                         lwd = 2)
                      })
    }
################################################################################################

    if (period == "monthly" | period == "weekly") {

        if(is.null(xlab)) xlab <- "year"
        if (period == "monthly") {
            models <- dlply(mydata, .(cond, year = as.numeric(format(date, "%Y")),
                                      month = as.numeric(format(date, "%m"))), model)
            results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
            names(results) <- c("cond", "year", "month", "intercept", "slope",
                                "rsquare", "seslope", "N")
            results$slope <- results$slope * adj
            results$seslope <- results$seslope * adj
            results$date <- ISOdate(results$year, results$month, 15)

        } else {
            models <- dlply(mydata, .(cond, year = as.numeric(format(date, "%Y")),
                                      month = as.numeric(format(date, "%U"))), model)
            results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
            names(results) <- c("cond", "year", "week", "intercept", "slope",
                                "rsquare", "seslope", "N")
            results$slope <- results$slope * adj
            results$seslope <- results$seslope * adj

            day <- round(results$week * 7) + 1
            day[day > 366] <- 366
            dates <- paste(results$year, day, sep = "-")
            results$date <- as.POSIXct(strptime(dates, "%Y-%j"), "GMT")
        }

        results <- subset(results, rsquare >= rsq.thresh & N >= n)
        start.year <- as.numeric(format(min(results$date), "%Y"))
        end.year <- as.numeric(format(max(results$date), "%Y"))

        yrs <- seq(ISOdate(start.year, 1, 1), ISOdate(end.year + 1, 1, 1), by = "1 years")
        if (missing(ylim)) ylim <- rng(results)

        plt <- xyplot(slope ~ date, data = results,
                      xlab = xlab,
                      ylim = ylim,
                      main = quickText(main, auto.text),
                      ylab = quickText(ylab, auto.text),...,
                      panel = function(x, y, subscripts, ...) {
                          panel.grid(-1, 0)
                          panel.abline(v = yrs, col = "grey85")
                          panel.xyplot(x, y, col = "#3366FF",...)
                          panel.segments(x, y - results$seslope[subscripts], x,
                                         y + results$seslope[subscripts], col = "#3366FF")
                          panel.loess(x, y, col = "red", lwd = 2, span = span)
                      })
    }
################################################################################################

    if (period == "weekday") {

        if(is.null(xlab)) xlab <- "weekday"
        models <- dlply(mydata, .(cond, weekday = format(date, "%a")), model)
        results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
        names(results) <- c("cond", "weekday", "intercept", "slope", "rsquare", "seslope", "N")
        results <- subset(results, rsquare >= rsq.thresh & N >= n)
        results$slope <- results$slope * adj
        results$seslope <- results$seslope * adj

        results$weekday <- ordered(results$weekday, levels = make.weekday.abbs())
        if (missing(ylim)) ylim <- rng(results)

        plt <- xyplot(slope ~ weekday | cond, data = results,
                      as.table = TRUE,
                      xlab = xlab,
                      main = quickText(main, auto.text),
                      ylim = ylim,
                      ylab = quickText(ylab, auto.text),...,
                      panel = function(x, y, subscripts, ...) {
                          panel.grid(-1, 0)
                          panel.abline(v = 1:7, col = "grey85")
                          panel.xyplot(x, y, col = "#3366FF", pch = 16,...)
                          panel.segments(x, y - results$seslope[subscripts], x,
                                         y + results$seslope[subscripts], col = "#3366FF",
                                         lwd = 2)
                      })
    }
################################################################################################

    if (period == "day.hour") {

        if(is.null(xlab)) xlab <- "hour"
        models <- dlply(mydata, .(cond, weekday = format(date, "%A"),
                                  hour = as.numeric(format(date, "%H"))), model)
        results <- ldply(models, function(x) c(coef(x), rsq(x), seslope(x), len(x)))
        names(results) <- c("cond", "weekday", "hour", "intercept", "slope",
                            "rsquare", "seslope", "N")
        results$slope <- results$slope * adj
        results$seslope <- results$seslope * adj
        results <- subset(results, rsquare >= rsq.thresh & N >= n)
        results$weekday <- ordered(results$weekday, levels = make.weekday.names())

        eq <- formula(slope ~ hour | weekday)
        if (condition) eq <- formula(slope ~ hour | weekday * cond)
        if (missing(ylim)) ylim <- rng(results)

        plt <- xyplot(eq, data = results,
                      as.table = TRUE,
                      layout = c(7, length(unique(results$cond))),
                      xlab = xlab,
                      main = quickText(main, auto.text),
                      ylim = ylim,...,
                      ylab = quickText(ylab, auto.text),
                      scales = list(x = list(at = c(0, 6, 12, 18, 23))),
                      panel = function(x, y, subscripts, ...) {
                          panel.grid(-1, 0)
                          panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                          panel.xyplot(x, y, col = "#3366FF", pch = 16,...)
                          panel.segments(x, y - results$seslope[subscripts], x,
                                         y + results$seslope[subscripts], col = "#3366FF",
                                         lwd = 2)
                      })
    }

    if (condition & period == "day.hour") print(useOuterStrips(plt)) else print(plt)
    invisible(results) ## return data used in plots
}
