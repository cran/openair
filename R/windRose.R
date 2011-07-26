pollutionRose <- function(mydata,
                          pollutant = "nox", key.footer = pollutant,
                          breaks = 6, paddle = FALSE, key.position = "right",
                          ...)
{
    if (is.null(breaks))  breaks <- 6
    if (is.numeric(breaks) & length(breaks) == 1) {
        breaks2 <- co.intervals(mydata[ , pollutant][is.finite(mydata[ ,pollutant])],
                                number = 10, overlap = 0)
        breaks <- pretty(c(min(mydata[ , pollutant], na.rm = TRUE),
                           breaks2[nrow(breaks2), 1]), breaks)
        breaks <- breaks[breaks >= min(mydata[ , pollutant], na.rm = TRUE)]
    }

    windRose(mydata, pollutant = pollutant, paddle = paddle,
             key.position = key.position, key.footer = key.footer,
             breaks = breaks, ...)
}


windRose <- function (mydata, ws.int = 2, angle = 30, type = "default",
                      cols = "default", main = "", grid.line = 5, width = 1,
                      auto.text = TRUE, breaks = 4, offset = 10,
                      paddle = TRUE, key.header = NULL, key.footer = "(m/s)",
                      key.position = "bottom", key = TRUE, dig.lab = 5,
                      statistic = "prop.count", pollutant = NULL,
                      ...)
{

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        ## strip
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
        ## other local colours
        calm.col <- "black"
    } else {
        calm.col <- "forestgreen"
    }


    if (360/angle != round(360/angle)) {
        warning("In windRose(...):\n  angle will produce some spoke overlap",
                "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.", call. = FALSE)
    }
    if (angle < 3) {
        warning("In windRose(...):\n  angle too small",
                "\n  enforcing 'angle = 3'", call. = FALSE)
        angle <- 3
    }

    allowed.statistics <- c("prop.count", "prop.mean")
    if (!is.character(statistic) || !statistic[1] %in% allowed.statistics) {
        warning("In windRose(...):\n  statistic unrecognised",
                "\n  enforcing statistic = 'prop.count'", call. = FALSE)
        statistic <- "prop.count"
    }

    vars <- c("wd", "ws")
    if (any(type %in%  dateTypes)) vars <- c(vars, "date")

    if (!is.null(pollutant)) {
        vars <- c(vars, pollutant)
    }
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    mydata <- na.omit(mydata)

    if (is.null(pollutant))
        pollutant <- "ws"
    mydata$.z.poll <- mydata[, pollutant]

    ## if (type == "ws")  type <- "ws.1"

    mydata$wd <- angle * ceiling(mydata$wd/angle - 0.5)
    mydata$wd[mydata$wd == 0] <- 360

    ## flag calms as negatives
    mydata$wd[mydata$ws == 0] <- -999 ## set wd to flag where there are calms
    ## do after rounding or -999 changes

    if (length(breaks) == 1) breaks <- 0:(breaks - 1) * ws.int

    if (max(breaks) < max(mydata$.z.poll, na.rm = TRUE)) breaks <- c(breaks, max(mydata$.z.poll, na.rm = TRUE))

    if (min(breaks) > min(mydata$.z.poll, na.rm = TRUE)) breaks <- c(min(mydata$.z.poll, na.rm = TRUE), breaks)

    breaks <- unique(breaks)
    mydata$.z.poll <- cut(mydata$.z.poll, breaks = breaks, include.lowest = FALSE,
                          dig.lab = dig.lab)

    theLabels <- gsub("[(]|[)]|[[]|[]]", "", levels(mydata$.z.poll))
    theLabels <- gsub("[,]", "-", theLabels)

######################
    ## statistic handling
#####################

    prepare.grid <- function(mydata) {

        levels(mydata$.z.poll) <- c(paste(".z.poll", 1:length(theLabels),
                                          sep = ""))


        count <- length(mydata$wd)
        calm <- mydata[mydata$wd == -999, ][, pollutant]
        mydata <- mydata[mydata$wd != -999, ]
        mydata <- na.omit(mydata) # needed?

        if(statistic == "prop.count") {
            calm <- length(calm)/count
            weights <- tapply(mydata[, pollutant], list(mydata$wd, mydata$.z.poll),
                              length) / count
        }

        if(statistic == "prop.mean") {
             calm <- sum(calm)

            weights <- tapply(mydata[, pollutant], list(mydata$wd, mydata$.z.poll),
                              sum)
            temp <- sum(sum(weights, na.rm = TRUE), na.rm = TRUE) + calm

            weights <- weights / temp
            calm <- calm / temp

        }

        weights[is.na(weights)] <- 0
        weights <- t(apply(weights, 1, cumsum))

        means <- mean(mydata[ , pollutant])
        weights <- cbind(data.frame(weights), wd = as.numeric(row.names(weights)),
                         calm = calm, means = means)


        weights
    }

    if (paddle) {
        poly <- function(wd, len1, len2, width, colour, x.off = 0,
                         y.off = 0) {
            theta <- wd * pi/180
            len1 <- len1 + off.set
            len2 <- len2 + off.set
            x1 <- len1 * sin(theta) - width * cos(theta) + x.off
            x2 <- len1 * sin(theta) + width * cos(theta) + x.off
            x3 <- len2 * sin(theta) - width * cos(theta) + x.off
            x4 <- len2 * sin(theta) + width * cos(theta) + x.off
            y1 <- len1 * cos(theta) + width * sin(theta) + y.off
            y2 <- len1 * cos(theta) - width * sin(theta) + y.off
            y3 <- len2 * cos(theta) + width * sin(theta) + y.off
            y4 <- len2 * cos(theta) - width * sin(theta) + y.off
            lpolygon(c(x1, x2, x4, x3), c(y1, y2, y4, y3), col = colour,
                     border = NA)
        }
    } else {
        poly <- function(wd, len1, len2, width, colour, x.off = 0,
                         y.off = 0) {
            len1 <- len1 + off.set
            len2 <- len2 + off.set
            theta <- seq((wd - (angle/2) + 1), (wd + (angle/2) -
                                                1), length.out = (angle - 2) * 10)
            theta <- ifelse(theta < 1, 360 - theta, theta)
            theta <- theta * pi/180
            x1 <- len1 * sin(theta) + x.off
            x2 <- rev(len2 * sin(theta) + x.off)
            y1 <- len1 * cos(theta) + x.off
            y2 <- rev(len2 * cos(theta) + x.off)
            lpolygon(c(x1, x2), c(y1, y2), col = colour, border = NA)
        }
    }

    mydata <- cutData(mydata, type, ...)

    results.grid <- ddply(mydata, type, prepare.grid)

    ## proper names of labelling ##############################################################################
    pol.name <- sapply(levels(results.grid[ , type[1]]),
                       function(x) quickText(x, auto.text))
    strip <- strip.custom(factor.levels = pol.name)

    if (length(type) == 1 ) {

        strip.left <- FALSE

    } else { ## two conditioning variables

        pol.name <- sapply(levels(results.grid[ , type[2]]),
                           function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels = pol.name)
    }
    if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip

###############################################################################

    col <- openColours(cols, length(theLabels))
    max.freq <- max(results.grid[, (length(type) + 1) : (length(theLabels) +
                                                         length(type))], na.rm = TRUE)
    off.set <- max.freq * (offset / 100)
    box.widths <- seq(0.002 ^ 0.25, 0.016 ^ 0.25, length.out = length(theLabels)) ^ 4

    #key, colorkey, legend
    legend <- list(col = col, space = key.position, auto.text = auto.text,
                   labels = theLabels, footer = key.footer, header = key.header,
                   height = 0.60, width = 1.5, fit = "scale",
                   plot.style = if(paddle) "paddle"  else "other")
    legend <- makeOpenKeyLegend(key, legend, "windRose")

    temp <- paste(type, collapse = "+")
    myform <- formula(paste(".z.poll1 ~ wd | ", temp, sep = ""))

    sub.title <- "Frequency of counts by wind direction (%)"
    if (statistic == "prop.mean") sub.title <- "Proportion contribution to the mean (%)"

    plt <- xyplot(myform,
                  xlim = 1.03 * c(-max.freq - off.set, max.freq + off.set),
                  ylim = 1.03 * c(-max.freq - off.set, max.freq + off.set),
                  data = results.grid,
                  type = "n",
                  sub = sub.title,
                  strip = strip,
                  strip.left = strip.left,
                  xlab = "", ylab = "",
                  main = quickText(main, auto.text),
                  as.table = TRUE,
                  aspect = 1,
                  par.strip.text = list(cex = 0.8),
                  scales = list(draw = FALSE),

                  panel = function(x, y, subscripts, ...) {
                      panel.xyplot(x, y, ...)
                      angles <- seq(0, 2 * pi, length = 360)
                      sapply(seq(off.set, 1 + off.set, by = grid.line/100),
                             function(x) llines(x * sin(angles), x * cos(angles),
                                                col = "grey85", lwd = 1))

                      subdata <- results.grid[subscripts, ]
                      for (i in 1:nrow(subdata)) {
                          with(subdata, {
                              for (j in 1:length(theLabels)) {
                                  if (j == 1) {
                                      temp <- "poly(wd[i], 0, .z.poll1[i], width * box.widths[1], col[1])"
                                  }
                                  else {
                                      temp <- paste("poly(wd[i], .z.poll", j -
                                                    1, "[i], .z.poll", j, "[i], width * box.widths[",
                                                    j, "], col[", j, "])", sep = "")
                                  }
                                  eval(parse(text = temp))
                              }
                          })
                      }
                      ltext(seq((grid.line / 100 + off.set), 1 + off.set,
                                grid.line / 100) * sin(pi/4),
                            seq((grid.line/100 +  off.set), 1 + off.set,
                                grid.line / 100) *
                            cos(pi / 4), paste(seq(grid.line, 100, by = grid.line),
                                               "%", sep = ""), cex = 0.7)
                      ltext(max.freq, -max.freq,
                            label = paste("mean = ", sprintf("%.1f", subdata$means[1]),
                            "\ncalm = ", sprintf("%.1f", 100 * subdata$calm[1]),
                            "%", sep = ""), adj = c(1, 0), cex = 0.7, col = calm.col)
                  }, legend = legend, ...)

    ## output ################################################################################

    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip,
              strip.left = strip.left))

    ## reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)

    newdata <- results.grid

    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)

}
