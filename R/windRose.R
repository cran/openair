pollutionRose <- function(polar,
                          pollutant = "nox", key.footer = pollutant,
                          breaks = 6, paddle = FALSE, key.position = "right",
                          ...)
{
    if(is.null(breaks))  breaks <- 6 
    if(is.numeric(breaks) & length(breaks) == 1){  
        breaks2 <- co.intervals(polar[ ,pollutant][is.finite(polar[ ,pollutant])], number = 10, overlap = 0)
        breaks <- pretty(c(min(polar[ ,pollutant], na.rm = TRUE), breaks2[nrow(breaks2), 1]), breaks)
        breaks <- breaks[breaks >= min(polar[ , pollutant], na.rm = TRUE)]
    }
    windRose(
             polar, pollutant = pollutant, paddle = paddle, key.position = key.position, 
             key.footer = key.footer, breaks = breaks, ...
             )
}


windRose <- function (polar, ws.int = 2, angle = 30, type = "default", cols = "default", 
                      main = "", grid.line = 5, width = 1, auto.text = TRUE, breaks = 4, 
                      paddle = TRUE, key.header = NULL, key.footer = "(m/s)", key.position = "bottom", 
                      key = NULL, dig.lab = 5, pollutant = NULL, 
                      ...) 
{
    if (360/angle != round(360/angle)) {
        warning("In windRose(...):\n  angle will produce some spoke overlap",
                "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.", call. = FALSE)
    }
    if (angle < 3) {
        warning("In windRose(...):\n  angle too small",
                "\n  enforcing 'angle = 3'", call. = FALSE)
        angle <- 3
    }

    

    vars <- c("ws", "wd", "date")
    if (!is.null(pollutant)) {
        vars <- c(vars, pollutant)
    }
    polar <- checkPrep(polar, vars, type, remove.calm = FALSE)
    polar <- na.omit(polar)

    if (is.null(pollutant)) {
        polar$.z.poll <- polar$ws
    }
    else {
        names(polar)[names(polar) == pollutant] <- ".z.poll"
    }
    
    if (type == "ws")  type <- "ws.1"
    
    
    polar$wd <- angle * ceiling(polar$wd/angle - 0.5)
    polar$wd[polar$wd == 0] <- 360
    
    if (length(breaks) == 1) breaks <- 0:(breaks - 1) * ws.int
    
    if (max(breaks) < max(polar$.z.poll, na.rm = TRUE)) breaks <- c(breaks, max(polar$.z.poll, na.rm = TRUE))
    
    if (min(breaks) > min(polar$.z.poll, na.rm = TRUE)) breaks <- c(min(polar$.z.poll, na.rm = TRUE), breaks)
    
    breaks <- unique(breaks)
    polar$.z.poll <- cut(polar$.z.poll, breaks = breaks, include.lowest = FALSE, 
                         dig.lab = dig.lab)
    
    theLabels <- gsub("[(]|[)]|[[]|[]]", "", levels(polar$.z.poll))
    theLabels <- gsub("[,]", "-", theLabels)
    
    prepare.grid <- function(polar) {
        wd <- factor(polar$wd)
        levels(polar$.z.poll) <- c(paste(".z.poll", 1:length(theLabels), 
                                         sep = ""))
        calm <- length(which(polar$wd < 0))/nrow(polar)
        polar$.z.poll[which(is.na(polar$.z.poll))] <- ".z.poll1"
        weights <- prop.table(table(polar$wd, polar$.z.poll))
        weights <- as.data.frame.matrix(weights)
        weights <- data.frame(t(apply(weights, 1, cumsum)))
        weights$cond <- polar$cond[1]
        weights$wd <- as.numeric(row.names(weights))
        weights <- subset(weights, wd > 0)
        weights$calm <- calm
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
    }
    else {
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
    
    polar <- cutData(polar, type)
    results.grid <- ddply(polar, .(cond), prepare.grid)
    col <- openColours(cols, length(theLabels))
    max.freq <- max(results.grid[, 1:length(theLabels)], na.rm = TRUE)
    off.set <- max.freq/10
    box.widths <- seq(0.002^0.25, 0.016^0.25, length.out = length(theLabels))^4

    legend <- list(col = col, space = key.position, auto.text = auto.text, 
                   labels = theLabels, footer = key.footer, header = key.header,
                   height = 0.60, width = 1.5, fit = "scale",
                   plot.style = if(paddle) "paddle"  else "other")
    if(!is.null(key))
        if(is.list(key)) legend[names(key)] <- key else 
    warning("In windRose(...):\n  non-list key not exported/applied\n  [see ?drawOpenKey for key structure/options]", 
            call. = FALSE)
    legend <- list(temp = list(fun = drawOpenKey, args = list(key = legend, draw = FALSE)))
    
    names(legend)[1] <- if(is.null(key$space)) key.position else key$space

    plt <- xyplot(.z.poll1 ~ wd | cond,
                  xlim = c(-max.freq - off.set, max.freq + off.set),
                  ylim = c(-max.freq - off.set, max.freq + off.set),
                  data = results.grid,
                  type = "n",
                  xlab = "", ylab = "",
                  main = quickText(main, auto.text),
                  as.table = TRUE,
                  aspect = 1,
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
                                grid.line/100) * sin(pi/4), seq((grid.line/100 + 
                                                                 off.set), 1 + off.set, grid.line / 100) * cos(pi / 4), 
                            seq(grid.line, 100, by = grid.line), cex = 0.7)
                      ltext(max.freq, -max.freq, label = paste("calm = ", 
                                                 sprintf("%.1f", 100 * subdata$calm[1]), "%", 
                                                 sep = ""), adj = c(1, 0), cex = 0.7, col = "forestgreen")
                  }, legend = legend)

    #################
    #output
    #################
    plot(plt)
    newdata <- results.grid
    if(is.null(pollutant))
        theLabels <- paste("ws", theLabels, sep=".") else
        theLabels <- paste(pollutant, theLabels, sep=".")
    names(newdata)[1:length(theLabels)] <- theLabels
    newdata <- newdata[c("cond", "wd", "calm", theLabels)]
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)  

}
