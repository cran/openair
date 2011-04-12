percentileRose <- function (mydata, pollutant = "nox", type = "default",
                            percentile = c(25, 50, 75, 90, 95), cols = "default",
                            fill = TRUE, lwd = 2, 
                            angle.scale = 45,
                            main = "",  auto.text = TRUE,  key.header = NULL,
                            key.footer = "percentile", key.position = "bottom",
                            key = NULL,  ...) 

{ 

    vars <- c("wd", pollutant)
    if (any(type %in%  dateTypes)) vars <- c(vars, "date")
    
    mydata <- checkPrep(mydata, vars, type)
    mydata <- na.omit(mydata)

    #greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        #strip only
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
    }

    if (!fill) { ## labels depend on whether line or area are used
        theLabels <- percentile
    } else {
        values <- cbind(c(0, percentile[-length(percentile)]), percentile)
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

            ## fit a spline through the data; making sure it goes through each wd value
            spline.res <- spline(x = thedata[ , "wd"], y = thedata[, pollutant], n = 361)
          
            pred <- data.frame(percentile = i, wd = 0:360, pollutant = spline.res$y)
            
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
    
    legend <- list(temp = list(fun = drawOpenKey, args = list(key = legend, draw = FALSE)))
    
    names(legend)[1] <- if(is.null(key$space)) key.position else key$space


    temp <- paste(type, collapse = "+")
    myform <- formula(paste("y ~ x | ", temp, sep = ""))

    results.grid <- transform(results.grid, x = pollutant * sin(wd * pi / 180),
                              y = pollutant * cos(wd * pi / 180))
    
    ## nice intervals for pollutant concentrations
    intervals <- pretty(results.grid$pollutant)
    
    plt <- xyplot(myform,
                  xlim = c(max(intervals) * -1, max(intervals) * 1),
                  ylim = c(max(intervals) * -1, max(intervals) * 1),
                  data = results.grid,
                  type = "n",
                  strip = strip,
                  strip.left = strip.left,
                  xlab = "", ylab = "",
                  main = quickText(main, auto.text),
                  as.table = TRUE,
                  aspect = 1,
                  par.strip.text = list(cex = 0.8),
                  scales = list(draw = FALSE),...,

                  panel = function(x, y, subscripts, ...) {

                      if (fill) { ## filled polygons

                          for (i in rev(seq_along(percentile))) {
                              value <- percentile[i]

                              if (i == 1) {
                                  subdata <- subset(results.grid[subscripts, ], percentile == value)
                                  lpolygon(subdata$x, subdata$y, col = col[1], border = NA)
                                  
                              } else {
                                  subdata1 <- subset(results.grid[subscripts, ], percentile == value)
                                  value2 <- percentile[i - 1]
                                  subdata2 <- subset(results.grid[subscripts, ],
                                                     percentile == value2)
                                  lpolygon(c(subdata1$x, subdata2$x),  c(subdata1$y, subdata2$y),
                                           col = col[i], border = NA)  
                              }
                          }
                      }

                      angles <- seq(0, 2 * pi, length = 360)
                      sapply(intervals, function(x) llines(x * sin(angles), x * cos(angles), 
                                                           col = "grey85", lty = 5))

                      ## add axis lines
                      larrows(max(intervals) * -1, 0, max(intervals), 0, code = 3, length = 0.1)
                      larrows(0, max(intervals) * -1, 0, max(intervals), code = 3, length = 0.1)
                      

                      ltext(1.2 * sin(pi * angle.scale / 180) * max(intervals),
                            1.2 * cos(pi * angle.scale / 180) * max(intervals),
                            quickText(pollutant, auto.text), srt = 0, cex = 0.8)

                      
                      ltext(max(intervals) * -1 * 0.95, 0.07 * max(intervals), "W", cex = 0.7)
                      ltext(0.07 * max(intervals), max(intervals) * -1 * 0.95, "S", cex = 0.7)
                      ltext(0.07 * max(intervals), max(intervals) * 0.95, "N", cex = 0.7)
                      ltext(max(intervals) * 0.95, 0.07 * max(intervals), "E", cex = 0.7)

                      ## draw lines if fill = FALSE
                      if (!fill) {
                          for (i in seq_along(percentile)) {
                              value <- percentile[i]
                              subdata <- subset(results.grid[subscripts, ], percentile == value)
                              llines(subdata$x, subdata$y, col = col[i], lwd = lwd)
                          }

                      }

                      ltext(intervals * sin(pi * angle.scale / 180),
                            intervals * cos(pi * angle.scale / 180),
                            paste(intervals, c("", "", rep("", 7))), cex = 0.7)
                      
                      
                  }, legend = legend)

    ## output ####################################################################################
    
    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- results.grid
                                       
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale") 
        trellis.par.set("strip.background", current.strip)

    invisible(output)  

}
