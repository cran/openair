
smoothTrend <- function(mydata,
                         pollutant = "nox",
                         deseason = FALSE,
                         type = "default",
                         statistic = "mean",
                         percentile = NA,
                         data.thresh = 0,
                         simulate = FALSE,
                         n = 200, #bootstrap simulations
                         autocor = FALSE,
                         cols = "brewer1",
                         ylab = pollutant,
                         xlab = "year",
                         lty = 1,
                         lwd = 1,
                         pch = 1,
                         cex = 0.6,
                         key.columns = length(percentile),
                         main = "",
                         ci = TRUE,
                         alpha = 0.2,
                         date.breaks = 7,
                         auto.text = TRUE,...)  {

   
    vars <- c("date", pollutant)

    mydata <- checkPrep(mydata, vars, type)

    if (!missing(percentile)) statistic <- "percentile"

    if (length(pollutant) > 1 & length(percentile) > 1) {
        warning(paste("You cannot choose multiple percentiles and pollutants, using percentile =", percentile[1]))
        percentile <- percentile[1]
    }

   
   
    ## for overall data and graph plotting
    start.year <- startYear(mydata$date)
    end.year <-  endYear(mydata$date)
    start.month <- startMonth(mydata$date)
    end.month <-  endMonth(mydata$date)

    ## date formatting for plot
    date.at <- dateBreaks(mydata$date, date.breaks)$major
    date.format <- dateBreaks(mydata$date)$format

    ## cutData depending on type
    mydata <- cutData(mydata, type)

    ## The aim to to get colums "date", "site" then turn to column data using melt
    ## Finally end up with "date", "value", "variable"

    ## don't need type, now a condition
    vars <-  c(vars, "cond")
    vars <- vars[vars != type]
    mydata <- mydata[, vars]
    mydata <- rename(mydata, c(cond = "site")) ## change to name "site"
     
        mydata <- melt(mydata, id.var = c("date", "site"))
        names(mydata)[2:3] <- c("variable", "group")
        mylab <- levels(factor(mydata$variable))
        mydata <- split(mydata, mydata$group)

    ## don't need a list for percentiles
    if (length(percentile) > 1) mydata <- mydata[[1]]

    process.cond <- function(mydata, percentile = percentile) {

        ## sometimes data have long trailing NAs, so start and end at
        ## first and last data
        min.idx <- min(which(!is.na(mydata[, "value"])))
        max.idx <- max(which(!is.na(mydata[, "value"])))
        mydata <- mydata[min.idx:max.idx, ]

        ## these subsets may have different dates to overall
        start.year <- startYear(mydata$date)
        end.year <-  endYear(mydata$date)
        start.month <- startMonth(mydata$date)
        end.month <-  endMonth(mydata$date)

        cond <- mydata$variable[1]
        group <- mydata$group[1]
        mydata <- timeAverage(mydata, period = "month", statistic = statistic, percentile = percentile,
                               data.thresh = data.thresh)

        if (type == "season") { ## special case

            results <- cbind(mydata, cond)
            ## winter stradles 2 years, need to deal with this
            results$month <- as.numeric(format(results$date, "%m"))
            results$year <- as.numeric(format(results$date, "%Y"))
            results$year[results$month == 12] <- results$year[results$month == 12] + 1
            ## remove missing for proper aggregation

            results <- subset(results, select = -cond)
            results <- aggregate(results, list(results$year), mean, na.rm = TRUE)

            class(results$date) <- c("POSIXt", "POSIXct")

            results$cond <- cond
            mydata <- na.omit(results)

        }

        ## can't deseason less than 2 years of data
        if (nrow(mydata) < 24) deseason <- FALSE

        if (deseason) {
            ## interpolate missing data using zoo

            mydata[, "value"] <- na.approx(mydata[, "value"])

            myts <- ts(mydata[, "value"], start = c(start.year, start.month),
                       end = c(end.year, end.month), frequency = 12)

            ssd <- stl(myts, s.window = 35, robust = TRUE, s.degree = 0)

            deseas <- ssd$time.series[, "trend"] + ssd$time.series[, "remainder"]
            deseas <- as.vector(deseas)
           
            results <- data.frame(date = mydata$date, conc = as.vector(deseas), cond = cond)

        } else {

            results <- data.frame(date = mydata$date, conc = mydata[, "value"],
                                  cond = cond)
        }

        results$group <- "NA"
        
        if (statistic == "percentile" & length(pollutant) == 1) { ## need to add "group" and percentile name
            per.name <- paste(percentile, "th", " percentile", sep = "")
            results$group <- per.name
            results$group <- ordered(results$group)

        }

        if (length(pollutant) > 1) { ## need to add "group" and pollutant name
                      
            results$group <- group

        }
        
        results
    }

    if (length(percentile) > 1) {
        calc.res <- function (x) ddply(mydata, .(variable), process.cond, percentile = x)
        split.data <- lapply(percentile, calc.res)
        split.data <- do.call(rbind, split.data)
        
    } else {
        
     split.data <- ldply(mydata,  function (x) ddply(x, .(variable),
                                                     process.cond, percentile = percentile))
   
    }
    
    ## define the levels for plotting

  #  if (type == "wd") layout = c(3, 3)
    layout = if (type == "wd") c(3, 3) else NULL
    
    strip <- TRUE
    skip <- FALSE
    if (type == "default") strip <- FALSE ## remove strip
    if (type == "wd") skip <-  c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
        FALSE, FALSE)

    ## colours according to number of percentiles
    npol <- max(length(percentile), length(pollutant)) ## number of pollutants

    ## set up colours
    myColors <- openColours(cols, npol)

    ## percentile names for key
    if (length(percentile) > 1) key.lab <- levels(split.data$group)

    ## for > 1 pollutant
    
    if (length(pollutant) > 1) key.lab <- sapply(seq_along(pollutant), function(x)
                                                 quickText(pollutant[x], auto.text))

    if (npol > 1) {
        key.columns <- npol
        key <- list(lines = list(col = myColors[1:npol], lty = lty, lwd = lwd,
                    pch = pch, type = "b", cex = cex),
                    text = list(lab = key.lab),  space = "bottom", columns = key.columns)
        if (missing(ylab)) ylab <-  paste(pollutant, collapse = ", ") 
        
    } else {
        key <- NULL ## either there is a key or there is not
    }

    plt <- xyplot(conc ~ date | cond, data = split.data, groups = group,
           as.table = TRUE,
           strip = strip,
           layout = layout,
           key = key,
           lwd = lwd,
           lty = lty,
           pch = pch,
           cex = cex,
           skip = skip,
           xlab = quickText(xlab, auto.text),
           ylab = quickText(ylab, auto.text),
           main = quickText(main, auto.text),
           scales = list(x = list(at = date.at, format = date.format)),
           panel = panel.superpose,
           ...,

           panel.groups = function(x, y, group.number, lwd, lty, pch, col, col.line, col.symbol,
           subscripts, type = "b",...) {


               if (group.number == 1) {  ## otherwise this is called every time

                   panel.shade(split.data, start.year, end.year, ylim = current.panel.limits()$ylim)

               }
               panel.xyplot(x, y, type = "b", lwd = lwd, lty = lty, pch = pch,
                            col.line = myColors[group.number],col.symbol = myColors[group.number], ...)

               panel.gam(x, y, col =  myColors[group.number], col.se =  "black",#myColors[group.number],
                         simulate = simulate, n.sim = n,
                         autocor = autocor, lty = 1, lwd = 1, se = ci, ...)

           })

    #################
    #output
    #################
    plot(plt)
    newdata <- split.data
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)  

}







