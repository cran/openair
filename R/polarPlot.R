
polarPlot <- function(polar,
                      pollutant = "nox",
                      type = "default",
                      resolution = "normal",
                      limits = NA,
                      exclude.missing = TRUE,
                      uncertainty = FALSE,
                      cols = "default",
                      min.bin = 1,
                      upper = NA,
                      ws.int = 5,
                      angle.scale = 45,
                      units = "(m/s)",
                      force.positive = TRUE,
                      k = 100,
                      normalise = FALSE,
                      main = "",
                      key.header = "",
                      key.footer = pollutant,
                      key.position = "right",
                      key = NULL,
                      auto.text = TRUE, ...) {
    
    
    ## initial checks ##########################################################################################
    if (length(type) > 2) {stop("Maximum number of types is 2.")}
    
    if (uncertainty) type <- "default" ## can't have conditioning here

    if (uncertainty & length(pollutant) > 1) stop("Can only have one pollutant when uncertainty = TRUE")

    ## extract variables of interest
    vars <- c("ws", "wd", "date", pollutant)

    polar <- checkPrep(polar, vars, type)

    ## if more than one pollutant, need to stack the data and set type = "variable"
    ## this case is most relevent for model-measurement compasrions where data are in columns
    ## Can also do more than one pollutant and a single type that is not "default", in which
    ## case pollutant becomes a conditioning variable
    if (length(pollutant) > 1) {
        
        if (length(type) > 1) {
            warning(paste("Only type = '", type[1], "' will be used", sep = ""))
            type <- type[1]
        }
        ## use pollutants as conditioning variables
        polar <- melt(polar, measure.vars = pollutant)
        ## now set pollutant to "value"
        pollutant <- "value"
        type <- c(type, "variable")
    }

    ## ##########################################################################################################
    
    polar <- na.omit(polar)
    ## cutData depending on type
    polar <- cutData(polar, type)
    

    ## if upper ws not set, set it to the max to display all information
    max.ws <- ceiling(max(polar$ws, na.rm = TRUE))
    if(missing(upper)) upper <- max.ws

    ## for resolution of grid plotting (default = 101; fine =201)
    if (resolution == "normal") int <- 101
    if (resolution == "fine") int <- 201
    if (resolution == "ultra.fine") int <- 401  ## very large files!

    ## binning wd data properly
    ws <- seq(0, max.ws, length = 30)
    wd <- seq(from = 10, to = 360, by = 10) ## wind directions from 10 to 360
    ws.wd <- expand.grid(ws = ws, wd = wd)

    u <- with(ws.wd, ws * sin(pi * wd / 180))  ## convert to polar coords
    v <- with(ws.wd, ws * cos(pi * wd / 180))

    ## data to predict over
    input.data <- expand.grid(u = seq(-upper, upper, length = int),
                              v = seq(-upper, upper, length = int))

    prepare.grid <- function(polar) {
        ## identify which ws and wd bins the data belong
        wd <- cut(polar$wd, breaks = seq(0, 360, 10), include.lowest = TRUE)
        ws <- cut(polar$ws, breaks = seq(0, max.ws, length = 31))

        ## this automatically deals with missing data
        binned <- tapply(polar[, pollutant], list(wd, ws), mean, na.rm = TRUE)
        binned <- as.vector(t(binned))

        ## frequency - remove points with freq < min.bin
        bin.len <- tapply(polar[, pollutant], list(ws, wd), length)
        binned.len <- as.vector(bin.len)
        ids <- which(binned.len < min.bin)
        binned[ids] <- NA
######################Smoothing#################################################
        if (force.positive) n <- 0.5 else n <- 1

        ## no uncertainty to calculate
        if (!uncertainty) {
            Mgam <- gam(binned ^ n ~ s(u, v, k = k))
            pred <- predict.gam(Mgam, input.data)
            pred <- pred ^ (1 / n)
            pred <- as.vector(pred)
            results <- data.frame(u = input.data$u, v = input.data$v, z = pred)

        } else {

            ## uncertainties calculated, weighted by number of points in each bin
            Mgam <- gam(binned ^ n ~ s(u, v, k = k), weights = binned.len)
            pred <- predict.gam(Mgam, input.data, se = TRUE)
            uncer <- 2 * as.vector(pred[[2]]) ## for approx 95% CI
            pred <- as.vector(pred[[1]]) ^ (1 / n)
            

            ## do not weight for central prediction
            Mgam <- gam(binned ^ n ~ s(u, v, k = k))
            pred <- predict.gam(Mgam, input.data)
            pred <- as.vector(pred)
            Lower <- (pred - uncer) ^ (1 / n)
            Upper <- (pred + uncer) ^ (1 / n)
            pred <- pred ^ (1 / n)
            
            n <- length(pred)
            results <-  data.frame(u = rep(input.data$u, 3), v = rep(input.data$v, 3),
                                   z = c(pred, Lower, Upper),
                                   default = rep(c("prediction", "lower uncertainty",
                                   "upper uncertainty"), each = n))
        }

#############################################################################
        ## function to remove points too far from original data
        exclude <- function(results) {

            ## exclude predictions too far from data (from mgcv)
            x <- seq(-upper, upper, length = int)
            y <- x
            res <- int
            wsp <- rep(x, res)
            wdp <- rep(y, rep(res, res))

            ## data with gaps caused by min.bin
            all.data <- na.omit(data.frame(u, v, binned))
            ind <- with(all.data, exclude.too.far(wsp, wdp, u, v, dist = 0.05))

            results$z[ind] <- NA
            results
        }

        if (exclude.missing) results <- exclude(results) 

        results
    }

    ## ########################################################################################################

    results.grid <- ddply(polar, type, prepare.grid)

    ## remove wind speeds > upper to make a circle
    results.grid$z[(results.grid$u ^ 2 + results.grid$v ^ 2) ^ 0.5 > upper] <- NA
    
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
    

    skip <- FALSE
    if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
    if (uncertainty) strip <- TRUE

    ## normalise by divining by mean conditioning value if needed
    if (normalise){
        results.grid <- ddply(results.grid, type, transform, z = z / mean(z, na.rm = TRUE))
        if (missing(key.footer)) key.footer <- "normalised \nlevel"
    }


    ## auto-scaling
    nlev <- 200  ## preferred number of intervals

    ## handle missing breaks arguments
    if(missing(limits)) breaks <- pretty(results.grid$z, n = nlev) else breaks <-
        pretty(limits, n = nlev)

    nlev2 = length(breaks)

    col <- openColours(cols, (nlev2 - 1))

    col.scale = breaks

    if (uncertainty) layout <- c(3, 1)
    layout = if (uncertainty) c(3, 1) else NULL

    ## scale key setup ######################################################################################

    legend <- list(col = col, at = col.scale, space = key.position, 
                   auto.text = auto.text, footer = key.footer, header = key.header, 
                   height = 1, width = 1.5, fit = "all")
    if (!is.null(key)) 
        if (is.list(key)) 
            legend[names(key)] <- key
        else warning("In polarPlot(...):\n  non-list key not exported/applied\n  [see ?drawOpenKey for key structure/options]", 
                     call. = FALSE)
    legend <- list(temp = list(fun = drawOpenKey, args = list(key = legend, draw = FALSE)))
    names(legend)[1] <- if(is.null(key$space)) key.position else key$space
########################################################################################################

    temp <- paste(type, collapse = "+")
    myform <- formula(paste("z ~ u * v | ", temp, sep = ""))

    plt <- levelplot(myform, results.grid, axes = FALSE,
                     as.table = TRUE,
                     layout = layout,
                     strip = strip,
                     strip.left = strip.left,
                     col.regions = col,
                     region = TRUE,
                     aspect = 1,
                     at = col.scale,
                     xlab = "",
                     ylab = "",
                     par.strip.text = list(cex = 0.8),
                     main = quickText(main, auto.text),
                     scales = list(draw = FALSE),
                     xlim = c(-upper * 1.15, upper * 1.15),
                     ylim = c(-upper * 1.15, upper * 1.15),
                     colorkey = FALSE, legend = legend, 
                     ...,

                     panel = function(x, y, z,subscripts,...) {
                         panel.levelplot(x, y, z,
                                         subscripts,
                                         at = col.scale,
                                         pretty = TRUE,
                                         col.regions = col,
                                         labels = FALSE)

                         angles <- seq(0, 2 * pi, length = 360)

                         sapply(seq(ws.int, 10 * ws.int, ws.int), function(x)
                                llines(x * sin(angles), x * cos(angles), col = "grey", lty = 5))

                         ltext(seq(ws.int, 10 * ws.int, by = ws.int) * sin(pi * angle.scale / 180),
                               seq(ws.int, 10 * ws.int, by = ws.int) * cos(pi * angle.scale / 180),
                               paste(seq(ws.int, 10 * ws.int, by = ws.int), c("", "",
                                                              units, rep("", 7))), cex = 0.7)

                         ## add axis line to central polarPlot
                         larrows(-upper, 0, upper, 0, code = 3, length = 0.1)
                         larrows(0, -upper, 0, upper, code = 3, length = 0.1)

                         ltext(-upper * 1.07, 0, "W", cex = 0.7)
                         ltext(0, -upper * 1.07, "S", cex = 0.7)
                         ltext(0, upper * 1.07, "N", cex = 0.7)
                         ltext(upper * 1.07, 0, "E", cex = 0.7)
                     })


    ## output ##############################################################################################
    
    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    
    newdata <- results.grid
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)  

}


