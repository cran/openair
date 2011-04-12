polarFreq <- function(mydata,
                      pollutant = "",
                      statistic = "frequency",
                      ws.int = 1,
                      grid.line = 5, 
                      breaks = seq(0, 5000, 500),
                      cols = "default",
                      trans = TRUE,
                      type = "default",
                      min.bin = 1,
                      border.col = "transparent",
                      main = "",
                      key.header = statistic,
                      key.footer = pollutant,
                      key.position = "right",
                      key = NULL,
                      auto.text = TRUE,...) {
    
    

    ## extract necessary data
    vars <- c("wd", "ws")
    if (any(type %in%  dateTypes)) vars <- c(vars, "date")

    #greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        #strip only
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
    }
    
    if (!missing(pollutant)) vars <- c(vars, pollutant)

    ## data checks
    mydata <- checkPrep(mydata, vars, type)
    mydata <- cutData(mydata, type, ...)

    ## if pollutant chosen but no statistic - use mean, issue warning
    if (!missing(pollutant) & missing(statistic)) {
        statistic <- "mean"
        warning("No statistic chosen, using mean")
    }

    ## if statistic chosen but no pollutant stop
    if (!missing(statistic) & missing(pollutant)) {
        stop("No pollutant chosen, please choose one e.g. pollutant = 'nox'")
    }

    if (!missing(breaks)) trans <- FALSE  ## over-ride transform if breaks supplied

    ## apply square root transform?
    if (trans) coef <- 2 else coef <- 1

    ## remove all NAs
    mydata <- na.omit(mydata)

    max.ws <- max(ceiling(mydata$ws), na.rm = TRUE)

    prepare.grid <- function(mydata)
    {
        wd <- factor(mydata$wd)
        ws <- factor(ws.int * ceiling(mydata$ws / ws.int))

        if (statistic == "frequency")     ## case with only ws and wd
        {
            weights <- tapply(mydata$ws, list(wd, ws), function(x) length(na.omit(x)))}

        if (statistic == "mean")
        {
            weights <- tapply(mydata[, pollutant],
                              list(wd, ws), function(x) mean(x, na.rm = TRUE))}

        if (statistic == "median")
        {
            weights <- tapply(mydata[, pollutant],
                              list(wd, ws), function(x) median(x, na.rm = TRUE))}

        if (statistic == "max")
        {
            weights <- tapply(mydata[, pollutant],
                              list(wd, ws), function(x) max(x, na.rm = TRUE))}

        if (statistic == "stdev")
        {
            weights <- tapply(mydata[, pollutant],
                              list(wd, ws), function(x) sd(x, na.rm = TRUE))}

        if (statistic == "weighted.mean")
        {
            weights <- tapply(mydata[, pollutant], list(wd, ws),
                              function(x) (mean(x) * length(x) / nrow(mydata)))

            ## note sum for matrix
            weights <- 100 * weights / sum(sum(weights, na.rm = TRUE))
        }

        weights <- as.vector(t(weights))

        ## frequency - remove points with freq < min.bin
        bin.len <- tapply(mydata$ws, list(wd, ws), function(x) length(na.omit(x)))
        binned.len <- as.vector(t(bin.len))
        ids <- which(binned.len < min.bin)
        weights[ids] <- NA

        ws.wd <- expand.grid(ws = as.numeric(levels(ws)), wd = as.numeric(levels(wd)))
        weights <- cbind(ws.wd, weights) 
        weights
    }


    poly <- function(dir, speed, colour)
    {
        ## offset by 3 so that centre is not compressed
        angle <- seq(dir - 5, dir + 5, length = 10)
        x1 <- (speed + 3) * sin(pi * angle/180)
        y1 <- (speed + 3) * cos(pi * angle/180)
        x2 <- rev((speed + ws.int + 3) * sin(pi * angle/180))
        y2 <- rev((speed + ws.int + 3) * cos(pi * angle/180))
        lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border.col, lwd = 0.5)
    }

    results.grid <- ddply(mydata, type, prepare.grid)
    results.grid <- na.omit(results.grid)

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
########################################################################################################

    results.grid$weights <- results.grid$weights ^ (1 / coef)

    nlev <- 200
    ## handle missing breaks arguments
    if(missing(breaks)) {

        breaks <- unique(c(0, pretty(results.grid$weights, nlev)))
        br <- pretty((results.grid$weights ^ coef), n = 10)  ## breaks for scale

    } else {

        br <- breaks

    }

    nlev2 <- length(breaks)

    col <- openColours(cols, (nlev2 - 1))

    results.grid$div <- cut(results.grid$weights, breaks)

    ## for pollution data
    results.grid$weights[results.grid$weights == "NaN"] <- 0
    results.grid$weights[which(is.na(results.grid$weights))] <- 0

    ##  scale key setup ################################################################################################
    legend <- list(col = col[1:length(breaks) - 1], at = breaks, 
                   labels = list(at = br^(1/coef), labels = br),
                   space = key.position, 
                   auto.text = auto.text, footer = key.footer, header = key.header, 
                   height = 1, width = 1.5, fit = "all")
    if (!is.null(key)) 
        if (is.list(key)) 
            legend[names(key)] <- key
        else warning("In polarFreq(...):\n  non-list key not exported/applied\n  [see ?drawOpenKey for key structure/options]", 
                     call. = FALSE)
    legend <- list(temp = list(fun = drawOpenKey, args = list(key = legend, 
                                                  draw = FALSE)))
    names(legend)[1] <- if(is.null(key$space)) key.position else key$space
    
    temp <- paste(type, collapse = "+")
    myform <- formula(paste("ws ~ wd | ", temp, sep = ""))
   
    plt <- xyplot(myform,
                  xlim = c(-max.ws - 4.0, max.ws + 4.0),
                  ylim = c(-max.ws - 4.0, max.ws + 4.0),
                  data = results.grid,
                  main = quickText(main, auto.text),
                  par.strip.text = list(cex = 0.8),
                  type = "n",
                  strip = strip,
                  strip.left = strip.left,
                  xlab = "",
                  ylab = "",
                  as.table = TRUE,
                  aspect = 1,
                  scales = list(draw = FALSE),...,

                  panel = function(x, y, subscripts,...) {
                      panel.xyplot(x, y,...)

                      subdata <- results.grid[subscripts,]

                      for (i in 1:nrow(subdata)) {
                          colour <- col[as.numeric(subdata$div[i])]
                          if (subdata$weights[i] == 0) colour <- "transparent"
                          poly(subdata$wd[i], subdata$ws[i], colour)
                      }

                      ## annotate
                      angles <- seq(0, 2 * pi, length = 360)
                      sapply(seq(0, 20 * grid.line, by = grid.line), function(x)
                             llines((3 + x + ws.int) * sin(angles),
                                    (3 + x + ws.int) * cos(angles),
                                    col = "grey", lty = 5))

                      ## radial labels
                      sapply(seq(0, 20 * grid.line, by = grid.line), function(x)
                             ltext((3 + x + ws.int) * sin(pi / 4), (3 + x + ws.int) * cos(pi / 4),
                                   x, cex = 0.7))                                                 

                       larrows(-max.ws - 4, 0,  -4, 0, code = 1, length = 0.1)
                      larrows(max.ws + 4, 0,  4, 0, code = 1, length = 0.1)
                      larrows(0, -max.ws - 4, 0, -4, code = 1, length = 0.1)
                      larrows(0, max.ws + 4, 0, 4, code = 1, length = 0.1)

                      ltext((-max.ws - 4) * 0.95, 0.07 * (max.ws +4), "W", cex = 0.7)
                      ltext(0.07 * (max.ws + 4), (-max.ws - 4)  * 0.95, "S", cex = 0.7)
                      ltext(0.07 * (max.ws + 4), (max.ws + 4) * 0.95, "N", cex = 0.7)
                      ltext((max.ws + 4) * 0.95, 0.07 * (max.ws + 4), "E", cex = 0.7)

                  },
                  legend = legend 
                  )

#################
                                        #output
#################
    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- results.grid
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

    #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale") 
        trellis.par.set("strip.background", current.strip)

    invisible(output)  


}
