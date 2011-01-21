
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

    ## reshape data
    ## in the case of mutiple percentiles, these are assinged and treated like multiple pollutants
    mydata <- melt(mydata, measure.vars = pollutant)

    if (length(percentile) > 1) {
        
        mydata <- ddply(mydata, c(type, "variable"), calcPercentile, pollutant = "value", period = "month",
                        percentile = percentile, data.thresh = data.thresh)
        
        mydata <- melt(subset(mydata, select = -variable), measure.vars = paste("percentile.", percentile, sep = ""))
        
    } else {
        mydata <- ddply(mydata, c(type, "variable"), timeAverage, period = "month", statistic = statistic,
                        percentile = percentile, data.thresh = data.thresh)               
    }
    

    process.cond <- function(mydata) {

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
            
            results <- data.frame(date = mydata$date, conc = as.vector(deseas)) 

        } else {

            results <- data.frame(date = mydata$date, conc = mydata[, "value"])
            
        }
        
        results
    }
        
    split.data <- ddply(mydata, c(type, "variable"),  process.cond)    
    
    skip <- FALSE
    layout <- NULL
    
    if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
    
    if (length(type) == 1 & type[1] == "wd") {
        ## re-order to make sensible layout
        split.data$wd <- ordered(split.data$wd, levels = c("NW", "N", "NE", "W", "E", "SW", "S", "SE"))
        skip <-  c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
        layout = if (type == "wd") c(3, 3) else NULL
    }

        ## proper names of labelling ##############################################################################
    pol.name <- sapply(levels(split.data[ , type[1]]), function(x) quickText(x, auto.text))
    strip <- strip.custom(factor.levels = pol.name)

    if (length(type) == 1 ) {
        
        strip.left <- FALSE
        
    } else { ## two conditioning variables        
        
        pol.name <- sapply(levels(split.data[ , type[2]]), function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels = pol.name)       
    }
    ## ########################################################################################################


    ## colours according to number of percentiles
    npol <- max(length(percentile), length(pollutant)) ## number of pollutants

    ## set up colours
    myColors <- openColours(cols, npol)

    ## information for key
    npol <- unique(split.data$variable)
    key.lab <- sapply(seq_along(npol), function(x) quickText(npol[x], auto.text))

    if (length(npol) > 1) {
        key.columns <- length(npol)
        key <- list(lines = list(col = myColors[1 : length(npol)], lty = lty, lwd = lwd,
                    pch = pch, type = "b", cex = cex),
                    text = list(lab = key.lab),  space = "bottom", columns = key.columns)
        if (missing(ylab)) ylab <-  paste(pollutant, collapse = ", ") 
        
    } else {
        key <- NULL ## either there is a key or there is not
    }

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("conc ~ date| ", temp, sep = ""))
    
    plt <- xyplot(myform, data = split.data, groups = variable,
                  as.table = TRUE,
                  strip = strip,
                  strip.left = strip.left,
                  layout = layout,
                  key = key,
                  lwd = lwd,
                  lty = lty,
                  pch = pch,
                  cex = cex,
                  skip = skip,
                  par.strip.text = list(cex = 0.8),
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
                          panel.grid(-1, 0)


                      }
                      panel.xyplot(x, y, type = "b", lwd = lwd, lty = lty, pch = pch,
                                   col.line = myColors[group.number], col.symbol = myColors[group.number], ...)

                      panel.gam(x, y, col =  myColors[group.number], col.se =  "black",
                                simulate = simulate, n.sim = n,
                                autocor = autocor, lty = 1, lwd = 1, se = ci, ...)

                  })

#################
                                        #output
#################
    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- split.data
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)  

}







