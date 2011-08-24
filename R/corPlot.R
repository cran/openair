
corPlot <- function(mydata, type = "default", cluster = TRUE, cols = "default", r.thresh = 0.8,
                         text.col = c("black", "black"),  main = "", auto.text = TRUE) {

    if (length(type) > 1) stop ("Only one 'type' allowed in this function.")

    ## make sure date is present for types requiring it
    if (any(type %in% openair:::dateTypes)) {
        if (!"date" %in% names(mydata)) stop ("Need a field 'date'")
    }

    ## remove variables where all are NA
    mydata <- mydata[ , sapply(mydata, function(x) !all(is.na(x)))]

    ## cut data depending on type
    mydata <- cutData(mydata, type)

    ## proper names of labelling
    pol.name <- sapply(names(mydata[, sapply(mydata, is.numeric)]),
                       function(x) quickText(x, auto.text))

    ## number of pollutants
    npol <- length(pol.name)


    prepare.cond <- function(mydata) {
        ## calculate the correlations
        thedata <- suppressWarnings(cor(mydata[, sapply(mydata, is.numeric)],
                                        use = "pairwise.complete.obs" ))

        ## remove columns/rows where all are NA
        therows <- apply(thedata, 1, function(x) !all(is.na(x)))
        thecols <- apply(thedata, 2, function(x) !all(is.na(x)))
        thedata <- thedata[therows, thecols]

        ## maybe reduced number of pollutants, hence select only those present
        thepols <-  pol.name[thecols]

        if (cluster) {
            ord.dat <- order.dendrogram(as.dendrogram(hclust(dist(thedata))))

        } else {
            ord.dat <- 1:ncol(thedata)
        }

        npol <- length(ord.dat)
        grid <- expand.grid(x= 1:npol, y = 1:npol)

        thepols <- thepols[ord.dat]

        thedata <- thedata[ord.dat, ord.dat]
        thedata <- as.vector(thedata)

        thedata <- cbind(grid, z = thedata, type = mydata[1, type])
        thedata <- list(thedata = thedata, pol.name = thepols)
        thedata

    }

    results.grid <- dlply(mydata, type, prepare.cond)

    ## list of labels
    labels <-  llply(results.grid, function(x) x$pol.name)
    results.grid <- do.call(rbind, llply(results.grid, function(x) x$thedata))

    div.col <- function (x) openColours(cols, x)

    ## labelleing of strips
    pol.name <- sapply(levels(results.grid[ , "type"]), function(x) quickText(x, auto.text))
    strip <- strip.custom(factor.levels = pol.name)
    if (type == "default") strip <- FALSE

    levelplot(z ~ x * y | type , data = results.grid,
              at = do.breaks(c(-1.01, 1.01), 100),
              xlab = NULL, ylab = NULL,
              as.table = TRUE,
              strip = strip,
              aspect = 1,
              main = quickText(main, auto.text),
              colorkey = FALSE,
              col.regions = div.col,
              par.strip.text = list(cex = 0.8),
              scales = list(x = list(rot = 90, labels = labels, at = 1 : npol),
              y = list(labels = labels, at = 1 : npol), relation = "free"),
              panel = panel.corrgram,  text.col = text.col, r.thresh = r.thresh, label = TRUE)

}

panel.corrgram <- function(x, y, z, subscripts, at, level = 0.9, text.col, r.thresh = r.thres,
                           label = FALSE, ...) {
    require("ellipse", quietly = TRUE)
    x <- as.numeric(x)[subscripts]
    y <- as.numeric(y)[subscripts]
    z <- as.numeric(z)[subscripts]

    zcol <- level.colors(z, at = at, ...)
    for (i in seq(along = z)) {
        ell <- ellipse(z[i], level = level, npoints = 50, scale = c(.2, .2),
                       centre = c(x[i], y[i]))
        panel.polygon(ell, col = zcol[i], border = zcol[i],...)

    }
    if (label)
        panel.text(x = x, y = y, lab = 100 * round(z, 2),
                   cex = 0.8, col = ifelse(z < 0, text.col[1], text.col[2]),
                   font = ifelse(z < r.thresh, 1, 2))
}

