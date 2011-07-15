TaylorDiagram <- function(mydata,
                          obs = "obs",
                          mod = "mod",
                          group = NULL,
                          type = "default",
                          normalise = FALSE,
                          layout = NULL,
                          cols = "brewer1",
                          main = "",
                          ylab = NULL,
                          xlab = NULL,
                          pch = 20,
                          cex = 2,
                          rms.col = "darkgoldenrod",
                          cor.col = "black",
                          key = TRUE,
                          key.title = group,
                          key.columns = 1,
                          key.pos = "bottom",
                          strip = TRUE,
                          auto.text = TRUE, ...)   {


    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {
        ## strip
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
        ## other local colours
        method.col <- "greyscale"
    } else {
        method.col <- "default"
    }


################################################################################################
    if (any(type %in%  openair:::dateTypes)) {

        vars <- c("date", obs, mod)

    } else {

        vars <- c(obs, mod)
    }

    ## if group is present, need to add that list of variables unless it is a pre-defined date-based one
    if (!missing(group)){

        if (group %in%  openair:::dateTypes | any(type %in% openair:::dateTypes)) {
            if (group %in%  openair:::dateTypes) {
                vars <- unique(c(vars, "date")) ## don't need group because it is defined by date
            } else {
                vars <- unique(c(vars, "date", group))
            }

        } else {

            vars <- unique(c(vars, group))
        }
    }

    if (!missing(group)) if (group %in% type) stop ("Can't have 'group' also in 'type'.")

    ## data checks
    mydata <- openair:::checkPrep(mydata, vars, type)

    ## remove missing data
    mydata <- na.omit(mydata)

    mydata <- cutData(mydata, type, ...)

    if (missing(group)) {

        if ((!"group" %in% type) & (!"group" %in% c(obs, mod))) {
            mydata$group <- factor("group")
            group <- "group"
        }
        ## don't overwrite a
    } else {  ## means that group is there
        mydata <- cutData(mydata, group, ...)
    }

    legend <- NULL

    npol <- length(levels(mydata[ , group]))

    ## function to calculate stats for TD
    calcStats <- function(mydata) {
        R <- cor(mydata[[obs]], mydata[[mod]], use = "pairwise")
        sd.obs <- sd(mydata[[obs]])
        sd.mod <- sd(mydata[[mod]])
        if (normalise) {
            sd.mod <- sd.mod / sd.obs
            sd.obs <- 1
        }

        res <- data.frame(R, sd.obs, sd.mod)
        res
    }

    results <- ddply(mydata, c(group, type), calcStats)

    ## if no group to plot, then add a dummy one to make xyplot work
    if (is.null(group)) {results$MyGroupVar <- factor("MyGroupVar"); group <-  "MyGroupVar"}

    ## set up colours
    myColors <- openColours(cols, npol)

    ## basic function for lattice call + defaults
    temp <- paste(type, collapse = "+")

    myform <- formula(paste("R ~ sd.mod", "|", temp, sep = ""))

    scales <- list(x = list(rot = 0), y = list(rot = 0))

    pol.name <- sapply(levels(mydata[ , group]), function(x) quickText(x, auto.text))


    if (missing(key.columns)) if (npol < 5) key.columns <- npol else key.columns <- 4

    if (key & npol > 1) {

        key <- list(points = list(col = myColors[1:npol]), pch = pch, cex = cex,
                    text = list(lab = pol.name, cex = 0.8), space = key.pos,
                    columns = key.columns,
                    title = quickText(key.title, auto.text),
                    cex.title = 0.8, lines.title = 3)

    } else {

        key <- NULL
    }

    ## special wd layout
    skip <- FALSE
    if (length(type) == 1 & type[1] == "wd" ) {
        ## re-order to make sensible layout
        wds <-  c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
        mydata$wd <- ordered(mydata$wd, levels = wds)

        ## see if wd is actually there or not
        wd.ok <- sapply(wds, function (x) {if (x %in% unique(mydata$wd)) FALSE else TRUE })
        skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])

        mydata$wd <- factor(mydata$wd)  ## remove empty factor levels

        layout = if (type == "wd") c(3, 3) else NULL
    }

    ## proper names of labelling ##############################################################################

    stripName <- sapply(levels(mydata[ , type[1]]), function(x) quickText(x, auto.text))
    if (strip) strip <- strip.custom(factor.levels = stripName)

    if (length(type) == 1 ) {

        strip.left <- FALSE

    } else { ## two conditioning variables
        stripName <- sapply(levels(mydata[ , type[2]]), function(x) quickText(x, auto.text))
        strip.left <- strip.custom(factor.levels =  stripName)
    }
    ## ########################################################################################################


    ## no strip needed for single panel
    if (length(type) == 1 & type[1]  == "default") strip <- FALSE

    ## not sure how to evaluate "group" in xyplot, so change to a fixed name
    id <- which(names(results) == group)
    names(results)[id] <- "MyGroupVar"

    maxsd <- 1.2 * max(results$sd.obs, results$sd.mod)

    if (missing(ylab)) {
        if (normalise) ylab <- "standard deviation (normalised)" else ylab <- "standard deviation"
    }

    if (missing(xlab)) xlab <- ylab

    plt <- xyplot(myform,  data = results, groups = MyGroupVar,
                  xlim = 1.12 * c(0, maxsd),
                  ylim = 1.12 * c(0, maxsd),
                  aspect = 1,
                  type = "n",
                  as.table = TRUE,
                  pch = pch,
                  cex = cex,
                  main = quickText(main, auto.text),
                  ylab = quickText(ylab, auto.text),
                  xlab = quickText(xlab, auto.text),
                  scales = scales,
                  key = key,
                  par.strip.text = list(cex = 0.8),
                  strip = strip,
                  strip.left = strip.left,
                  layout = layout,
                  skip = skip,
                  panel =  panel.superpose,...,
                  panel.groups = function(x, y, col.symbol, col, type, col.line, lty, lwd,
                  group.number,

                  subscripts,...)
              {

                  ## draw the diagram
                  if (group.number == 1) {
                      xcurve <- cos(seq(0, pi / 2, by = 0.01)) * maxsd
                      ycurve <- sin(seq(0, pi / 2, by = 0.01)) * maxsd
                      llines(xcurve, ycurve, col = "black")

                      xcurve <- cos(seq(0, pi / 2, by = 0.01)) * results$sd.obs[subscripts]
                      ycurve <- sin(seq(0, pi / 2, by = 0.01)) * results$sd.obs[subscripts]
                      llines(xcurve, ycurve, col = "black", lty = 5)

                      corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9)

                      ## grid line with alpha transparency
                      theCol <- t(col2rgb(cor.col)) / 255

                      for (gcl in corr.lines) llines(c(0, maxsd * gcl), c(0, maxsd * sqrt(1 - gcl ^ 2)),
                                                     col = rgb(theCol, alpha = 0.4), alpha = 0.5)

                      bigtick <- acos(seq(0.1, 0.9, by = 0.1))
                      medtick <- acos(seq(0.05, 0.95, by = 0.1))
                      smltick <- acos(seq(0.91, 0.99, by = 0.01))

                      lsegments(cos(bigtick) * maxsd, sin(bigtick) *
                                maxsd, cos(bigtick) * 0.96 * maxsd, sin(bigtick) * 0.96 * maxsd,
                                col = cor.col)

                      lsegments(cos(medtick) * maxsd, sin(medtick) *
                                maxsd, cos(medtick) * 0.98 * maxsd, sin(medtick) * 0.98 * maxsd,
                                col = cor.col)
                      lsegments(cos(smltick) * maxsd, sin(smltick) *
                                maxsd, cos(smltick) * 0.99 * maxsd, sin(smltick) * 0.99 * maxsd,
                                col = cor.col)

                      ## arcs for standard deviations (3 by default)
                      gamma <- pretty(c(0, maxsd), n = 5)
                      if (gamma[length(gamma)] > maxsd)
                          gamma <- gamma[-length(gamma)]
                      labelpos <- seq(45, 70, length.out = length(gamma))

                      ## from plotrix
                      for (gindex in 1:length(gamma)) {
                          xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] +
                              results$sd.obs[subscripts]
                          endcurve <- which(xcurve < 0)
                          endcurve <- ifelse(length(endcurve), min(endcurve) - 1, 105)
                          ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
                          maxcurve <- xcurve * xcurve + ycurve * ycurve
                          startcurve <- which(maxcurve > maxsd * maxsd)
                          startcurve <- ifelse(length(startcurve), max(startcurve) + 1, 0)

                          llines(xcurve[startcurve : endcurve], ycurve[startcurve : endcurve],
                                 col = rms.col, lty = 5)

                          ltext(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]],
                                 gamma[gindex], cex = 0.7, col = rms.col, pos = 1,
                                srt = 0, font = 2)

                          ltext(1.1 * maxsd, 1.05 * maxsd, "centred\nRMS error", cex = 0.7,
                                col = rms.col, pos = 2)
                      }

                      ## angles for R key
                      angles <- 180 * c(bigtick, acos(c(0.95, 0.99))) / pi

                      ltext(cos(c(bigtick, acos(c(0.95, 0.99)))) *
                            1.06 * maxsd, sin(c(bigtick, acos(c(0.95, 0.99)))) *
                            1.06 * maxsd, c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99), cex = 0.7,
                            adj = 0.5, srt = angles, col = cor.col)

                      ltext(0.82 * maxsd, 0.82 * maxsd, "correlation", srt = 315, cex = 0.7,
                            col = cor.col)


                      ## measured point and text
                      lpoints(results$sd.obs[subscripts], 0, pch = 20, col = "purple", cex = 1.5)
                      ltext(results$sd.obs[subscripts], 0, "observed", col = "purple", cex = 0.7, pos = 3)

                  }

                  results <- transform(results, x = sd.mod * R, y = sd.mod * sin(acos(R)))

                  lpoints(results$x[subscripts], results$y[subscripts],
                          col.symbol = myColors[group.number], ...)



              })



    if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
    newdata <- results
    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"

                                        ## reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)

    invisible(output)

}







