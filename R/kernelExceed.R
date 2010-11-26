kernelExceed <- function(polar,
                         x = "wd",
                         y = "ws",
                         pollutant = "pm10",
                         type = "default",                         
                         by = c("day", "dayhour", "all"),
                         limit = 50,
                         data.thresh = 0,
                         more.than = TRUE,
                         cols = "default",
                         xlab = x,
                         ylab = y,
                         nbin = 256,
                         main = "",
                         auto.text = TRUE, ...) {

    ## extract variables of interest
    vars <- c(y, x, "date", pollutant)
    polar <- checkPrep(polar, vars, type, remove.calm = FALSE)
    polar <- subset(polar, wd > 0)
    
    ## cut data depending on type
    polar <- cutData(polar, type)

    if (by[1] == "day" | by[1] == "dayhour") {
        ## identify days where pm10 > limit
        daily <- timeAverage(polar, "day", data.thresh = data.thresh)

        ## days where this is true - more than or less than a threshold
        if (more.than)   ids <- which(daily[pollutant] > limit) else ids <- which(daily[pollutant] < limit)
        days <- daily$date[ids]
        
        ## ids for the hours
        ids <- which(as.Date(polar$date) %in% as.Date(days))

        subdata <- polar[ids, ]

        ## only select hour on days that exceed
        if (by[1] == "dayhour") subdata <- subdata[subdata[pollutant] > limit, ]
        subdata <- na.omit(subdata)        

    } else {
        if (more.than)   ids <- which(polar[pollutant] > limit) else ids <- which(polar[pollutant] < limit)
        days <- polar$date[ids]

        subdata <- polar[ids, ]
    }

    if(nrow(subdata) == 0) stop(call. = FALSE, "No data above threshold to plot")

    prepare.grid <- function(subdata) {
        x <- subdata[ , x] 
        y <- subdata[ , y]

        ## pearson correlation

        if (type %in% names(subdata)) {
            cor.dat <- na.omit(subdata[, c(pollutant, type)])
            Pcor <-  cor(cor.dat[, pollutant], cor.dat[, type], method = "pearson")
        } else {
            Pcor <- NA
        }

        xy <- xy.coords(x, y, "xlab", "ylab")
        xlab <-  xy$xlab
        ylab <- xy$ylab

        x <- cbind(xy$x, xy$y)[is.finite(xy$x) & is.finite(xy$y), , drop = FALSE]

        xlim <- range(x[, 1])
        ylim <- range(x[, 2])
        
        map <- grDevices:::.smoothScatterCalcDensity(x, nbin)
        xm <- map$x1
        ym <- map$x2

        dens <- map$fhat

        grid <- expand.grid(x = xm, y = ym)

        results <- data.frame(u = grid$x, v = grid$y, z = as.vector(dens), cond = subdata$cond[1],
                              freq = nrow(subdata) / 24, Pcor = Pcor)

        results
    }

#############################################################################

    results.grid <-  ddply(subdata, .(cond), prepare.grid)

    ## adjust to get number of exceedance days
    total.sum <-  sum(unique(results.grid$freq)) ## by each condition
    results.grid$freq <- results.grid$freq * length(days) / total.sum

    strip <- TRUE
    skip <- FALSE
    if (type == "default") strip <- FALSE ## remove strip

    ## auto-scaling
    nlev <- 200  ## preferred number of intervals
    breaks <- unique(pretty(results.grid$z, n = nlev))
    nlev2 <- length(breaks)

    col <- openColours(cols, (nlev2 - 1))
    col <- c("transparent", col) ## add white at bottom
    col.scale <- breaks

    X <- x
    Y <- y ## to avoid confusion with lattice function

    scales <- list()
    if (X == "wd")  scales <- list(x = list(at = seq(0, 360, 90)))

    levelplot(z ~ u * v | cond, results.grid,
              as.table = TRUE,
              strip = strip,
              region = TRUE,
              xlab = quickText(xlab, auto.text),
              ylab = quickText(ylab, auto.text),
              main = quickText(main, auto.text),
              scales = scales,              
              colorkey = FALSE,
              ...,
              
              panel = function(x, y, z, subscripts,...) {

                  panel.levelplot(x, y, z,
                                  subscripts,
                                  at = col.scale,
                                  pretty = TRUE,
                                  col.regions = col,
                                  labels = FALSE)
                  panel.grid(-1, 0, col = "grey90", lty = 5)
                  if (X == "wd") {
                      
                      panel.abline(v = seq(0, 360, by = 90), col = "grey90", lty = 5)
                      
                  } else {
                      
                      panel.grid(0, -1, col = "grey90", lty = 5)
                      
                  }
                  if (by[1] == "day" | by[1] == "dayhour") len <- "days" else len <- "hours"
                  panel.text(0.03 * max(results.grid$u, na.rm = TRUE), 0.95 * max(results.grid$v,
                                                       na.rm = TRUE),
                             paste(round(results.grid[subscripts[1],
                                                      "freq"]), len, ", Cor =",
                                   format(results.grid[subscripts[1], "Pcor"], digits = 2)), pos = 4,
                             cex = 0.7, ...)

              })
    
}

