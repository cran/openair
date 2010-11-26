cutData <- function(x, type = "default") {

    ## function to cutData depending on choice of variable
    ## pre-defined types and user-defined types
    ## If another added, then amend checkPrep

    ## adds a column "cond"
    conds <- c("default", "year", "hour", "month", "season", "weekday", "ws", "site", "weekend", "monthyear",
               "bstgmt", "gmtbst")

    if (type %in% conds == FALSE) { ## generic, user-defined
        ## split by four quantiles unless it is a factor, in which case keep as is

        if (is.factor(x[, type]) | is.character(x[, type])) {

            if (is.character(x[, type])) x$cond <- as.character(x[, type])

            if (is.factor(x[, type])) x$cond <- x[, type]
            

        } else {

            x$cond <- cut(x[, type], unique(quantile(x[, type],
                                                               probs = seq(0, 1, length = 5),
                                                               na.rm = TRUE)), include.lowest = TRUE,
                               labels = FALSE)

            temp.levels <- levels(cut(x[, type], unique(quantile(x[, type],
                                                                      probs = seq(0, 1, length = 5),
                                                                      na.rm = TRUE)),
                                      include.lowest = TRUE))

            x$cond <- as.factor(x$cond)
            temp.levels <- gsub("[(]|[)]|[[]|[]]", "", temp.levels)
            temp.levels <- gsub("[,]", " to ", temp.levels)
            levels(x$cond) = temp.levels
        }

    }

    if (type == "default") {
        ## shows dates (if available)
        ## not always available e.g. scatterPlot
        if ("date" %in% names(x)) {

            x$cond <- paste(format(min(x$date), "%d %B %Y"), " to ",
                                 format(max(x$date), "%d %B %Y"), sep = "")
            ## order the data by date
            x <- x[order(x$date), ]

        } else {
            x$cond <- "all data"
        }

    }

    if (type == "year") x$cond <- format(x$date, "%Y")

    if (type == "hour") x$cond <- format(x$date, "%H")

    if (type == "month") {x$cond <- format(x$date, "%B")
                          x$cond <- ordered(x$cond, levels = make.month.names())
                          period <- "annual"} #does not make sense otherwise

    if (type == "monthyear") {
        x$cond <- format(x$date, "%B %Y")
        x$cond <- ordered(x$cond, levels = unique(x$cond))
    }

    if (type == "season") {
        x$cond <- "winter" ## define all as winter first, then assign others
        ids <- which(as.numeric(format(x$date, "%m")) %in% 3:5)
        x$cond[ids] <- "spring"
        ids <- which(as.numeric(format(x$date, "%m")) %in% 6:8)
        x$cond[ids] <- "summer"
        ids <- which(as.numeric(format(x$date, "%m")) %in% 9:11)
        x$cond[ids] <- "autumn"
        x$cond <- ordered(x$cond, levels =c("spring", "summer", "autumn", "winter"))
        period <- "annual"
    } #does not make sense otherwise

    if (type == "weekend") {
        ## split by weekend/weekday
        weekday <- selectByDate(x, day = "weekday")
        weekday$cond <- "weekday"
        weekend <- selectByDate(x, day = "weekend")
        weekend$cond <- "weekend"

        x <- rbind(weekday, weekend)

    }

    if (type == "weekday") {
        x$cond <- format(x$date, "%A")
        x$cond <- ordered(x$cond, levels = make.weekday.names())
    }

    if (type == "wd") {

        x$cond <- cut(x$wd, breaks = seq(22.5, 382.5, 45),
                           labels =c("NE", "E", "SE", "S", "SW", "W", "NW", "N"))
        x$cond[is.na(x$cond)] <- "N" # for wd < 22.5
        x$cond <- ordered(x$cond, levels = c("NW", "N", "NE",
                                            "W", "E", "SW", "S", "SE"))}

    if (type == "ws") {x$cond <- cut(x$ws, breaks = quantile(x$ws,
                                                     probs = 0:8/8, na.rm = TRUE))
                       ws.levels = levels(x$cond)
                       ws.levels <- gsub("[,]", " to ", ws.levels)
                       ws.levels <- gsub("[(]|[)]|[[]|[]]", "", ws.levels)
                       levels(x$cond) <- ws.levels
                   }

    if (type == "site") {
        x$cond <- x$site
        x$cond <- factor(x$cond) ## will get rid of any unused factor levels
    }

    if (type == "gmtbst" | type == "bstgmt") {
        ## how to extract BST/GMT
        ## first format date in local time
        x$date <- format(x$date, usetz = TRUE, tz = "Europe/London")
        ## extract ids where BST/GMT
        id.BST <- grep("BST", x$date)
        id.GMT <- grep("GMT", x$date)

        bst <- x[id.BST, ]
        bst$cond <- "BST hours"

        gmt <- x[id.GMT, ]
        gmt$cond <- "GMT hours"
        
        x <- rbind.fill(bst, gmt)
        x$date <- as.POSIXct(x$date, "GMT")
        x <- x[order(x$date), ]

    }

    x

}
