polarFreq <- function(polar,
                       pollutant = "",
                       statistic = "frequency",
                       ws.int = 1,
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
    if (pollutant == "") {
        if (type == "site"){
            vars <- c("ws", "wd", "date", "site")
        } else {
            vars <- c("ws", "wd", "date")
        }
    } else {
        if (type == "site") {
            vars <- c("ws", "wd", "date", pollutant, "site")
        } else {
            vars <- c("ws", "wd", "date", pollutant)
        }
    }

    ## data checks
    polar <- checkPrep(polar, vars, type)
    polar <- cutData(polar, type)

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
    polar <- na.omit(polar)

    max.ws <- max(ceiling(polar$ws), na.rm = TRUE)

    prepare.grid <- function(polar)
    {
        wd <- factor(polar$wd)
        ws <- factor(ws.int * ceiling(polar$ws / ws.int))

        if (statistic == "frequency")     ## case with only ws and wd
        {
            weights <- tapply(polar$ws, list(wd, ws), function(x) length(na.omit(x)))}

        if (statistic == "mean")
        {
            weights <- tapply(polar[, pollutant],
                              list(wd, ws), function(x) mean(x, na.rm = TRUE))}

        if (statistic == "median")
        {
            weights <- tapply(polar[, pollutant],
                              list(wd, ws), function(x) median(x, na.rm = TRUE))}

        if (statistic == "max")
        {
            weights <- tapply(polar[, pollutant],
                              list(wd, ws), function(x) max(x, na.rm = TRUE))}

        if (statistic == "stdev")
        {
            weights <- tapply(polar[, pollutant],
                              list(wd, ws), function(x) sd(x, na.rm = TRUE))}

        if (statistic == "weighted.mean")
        {
            weights <- tapply(polar[, pollutant], list(wd, ws),
                              function(x) (mean(x) * length(x) / nrow(polar)))

            ## note sum for matrix
            weights <- 100 * weights / sum(sum(weights, na.rm = TRUE))
        }

        weights <- as.vector(t(weights))

        ## frequency - remove points with freq < min.bin
        bin.len <- tapply(polar$ws, list(wd, ws), function(x) length(na.omit(x)))
        binned.len <- as.vector(t(bin.len))
        ids <- which(binned.len < min.bin)
        weights[ids] <- NA

        ws.wd <- expand.grid(ws = as.numeric(levels(ws)), wd = as.numeric(levels(wd)))
        weights <- cbind(ws.wd, weights, cond = polar$cond[1]) # add cond variable
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

    results.grid <- ddply(polar, .(cond), prepare.grid)
    results.grid <- na.omit(results.grid)

    results.grid$weights <- results.grid$weights ^ (1 / coef)

    nlev = 200
    ## handle missing breaks arguments
    if(missing(breaks)) {

        breaks = unique(c(0, pretty(results.grid$weights, nlev)))
        br = pretty((results.grid$weights ^ coef), n = 10)  ## breaks for scale

    } else {

        br = breaks

    }

    nlev2 = length(breaks)

    col <- openColours(cols, (nlev2 - 1))

    results.grid$div <- cut(results.grid$weights, breaks)

    ## for pollution data
    results.grid$weights[results.grid$weights == "NaN"] <- 0
    results.grid$weights[which(is.na(results.grid$weights))] <- 0

    #################
    #scale key setup
    #################
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

    xyplot(ws ~ wd | cond,
           xlim = c(-max.ws - 4.0, max.ws + 4.0),
           ylim = c(-max.ws - 4.0, max.ws + 4.0),
           data = results.grid,
           layout = layout,
           main = quickText(main, auto.text),
           ## strip = strip,
           type = "n",
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

                                        #annotate
               angles <- seq(0, 2 * pi, length = 360)
               sapply(seq(5, 25, 5), function(x)
                      llines((3 + x + ws.int) * sin(angles),
                             (3 + x + ws.int) * cos(angles),
                             col = "grey", lty = 5))

               ltext(seq(3 + ws.int, 28 + ws.int, length = 6) * sin(pi/4),
                     seq(3 + ws.int, 28 + ws.int, length = 6) * cos(pi/4),
                     seq(0, 25, 5), cex = 0.7, font = 1)

           },
           legend = legend 
           )
}
