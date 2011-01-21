cutData <- function(x, type = "default", hemisphere = "northern", n.levels = 4, is.axis = FALSE) {

    ## function to cutData depending on choice of variable
    ## pre-defined types and user-defined types
    ## If another added, then amend checkPrep

    ## note: is.axis modifies factor levels to give shorter labels for axis
    ##       generic label shortening handled at end of section
    ##       format(date, "%?") outputs modified by is.axis are set using temp 
    ##       declared at at start of associated type section - karl

    makeCond <- function(x, type = "default") {
        ## adds a column "cond"

        
        conds <- c("default", "year", "hour", "month", "season", "weekday", "wd", "site", "weekend", "monthyear",
                   "bstgmt", "gmtbst")

        ## if conditioning type already built in, is present in data frame and is a factor
        if (type %in% conds & type %in% names(x)) {
           
            if (is.factor(x[ , type])) {
               
                x[ , type] <- factor(x[ , type])  ## remove unused factor levels
                return(x)
            }
        }

        if (type %in% conds == FALSE) { ## generic, user-defined
            ## split by quantiles unless it is a factor, in which case keep as is
            ## number of quantiles set by n.levels

            if (is.factor(x[, type]) | is.character(x[, type])) {

                ## drop unused levels while we are at it             
                x[, type] <- factor(x[, type])

            } else {


                temp.levels <- levels(cut(x[, type], unique(quantile(x[, type], probs = seq(0, 1, length = n.levels + 1),
                                                                     na.rm = TRUE)), include.lowest = TRUE))

                x[ , type] <- cut(x[, type], unique(quantile(x[, type], probs = seq(0, 1, length = n.levels + 1),
                                                             na.rm = TRUE)), include.lowest = TRUE, labels = FALSE)

                x[ , type] <- as.factor(x[ , type])
                temp.levels <- gsub("[(]|[)]|[[]|[]]", "", temp.levels)
                temp.levels <- gsub("[,]", " to ", temp.levels)
                levels(x[ , type]) <- if(is.axis) temp.levels else paste(type, temp.levels)
            }

        }

        if (type == "default") {
            ## shows dates (if available)
            ## not always available e.g. scatterPlot
            if ("date" %in% names(x)) {

                x[ , type] <- factor(paste(format(min(x$date), "%d %B %Y"), " to ",
                                    format(max(x$date), "%d %B %Y"), sep = ""))
                ## order the data by date
                x <- x[order(x$date), ]

            } else {
                x[ , type] <- factor("all data")
            }

        }

        if (type == "year") x[ , type] <- factor(format(x$date, "%Y"))

        if (type == "hour") x[ , type] <- factor(format(x$date, "%H"))

        if (type == "month") {
             temp <- if(is.axis) "%b" else "%B"
             x[ , type] <- format(x$date, temp)
             x[ , type] <- ordered(x[ , type], levels = format(seq(as.Date("2000-01-01"),
                                   as.Date("2000-12-31"), "month"), temp))
        } 

        if (type == "monthyear") {
            x[ , type] <- format(x$date, "%B %Y")
            x[ , type] <- ordered(x[ , type], levels = unique(x[ , type]))
        }

        if (type == "season") {
            
            if (!hemisphere %in% c("northern", "southern")) {stop("hemisphere must be 'northern' or 'southern'")}
            if (hemisphere == "northern") {
                x[ , type] <- "winter (DJF)" ## define all as winter first, then assign others
                ids <- which(as.numeric(format(x$date, "%m")) %in% 3:5)
                x[ , type][ids] <- "spring (MAM)"
                ids <- which(as.numeric(format(x$date, "%m")) %in% 6:8)
                x[ , type][ids] <- "summer (JJA)"
                ids <- which(as.numeric(format(x$date, "%m")) %in% 9:11)
                x[ , type][ids] <- "autumn (SON)"
                x[ , type] <- ordered(x[ , type], levels = c("spring (MAM)", "summer (JJA)", "autumn (SON)", "winter (DJF)"))
            }
            if (hemisphere == "southern") {
                x[ , type] <- "summer (DJF)" ## define all as winter first, then assign others
                ids <- which(as.numeric(format(x$date, "%m")) %in% 3:5)
                x[ , type][ids] <- "autumn (MAM)"
                ids <- which(as.numeric(format(x$date, "%m")) %in% 6:8)
                x[ , type][ids] <- "winter (JJA)"
                ids <- which(as.numeric(format(x$date, "%m")) %in% 9:11)
                x[ , type][ids] <- "spring (SON)"
                x[ , type] <- ordered(x[ , type], levels = c("spring (SON)", "summer (DJF)", "autumn (MAM)", "winter (JJA)"))
                
            }
            
        } 

        if (type == "weekend") {
            ## split by weekend/weekday
            weekday <- selectByDate(x, day = "weekday")
            weekday[ , type] <- "weekday"
            weekend <- selectByDate(x, day = "weekend")
            weekend[ , type] <- "weekend"

            x <- rbind(weekday, weekend)
            x[ , type] <- ordered(x[ , type], levels = c("weekday", "weekend")) 

        }

        if (type == "weekday") {
            x[ , type] <- format(x$date, "%A")
            x[ , type] <- ordered(x[ , type], levels = format(ISOdate(2000, 1, 3:9), "%A"))
        }

        if (type == "wd") {

            x[ , type] <- cut(x$wd, breaks = seq(22.5, 382.5, 45),
                              labels = c("NE", "E", "SE", "S", "SW", "W", "NW", "N"))
            x[ , type][is.na(x[ , type])] <- "N" # for wd < 22.5
            x[ , type] <- ordered(x[ , type], levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))}

        
        if (type == "site") {
            x[ , type] <- x$site
            x[ , type] <- factor(x[ , type]) ## will get rid of any unused factor levels
        }

        if (type == "gmtbst" | type == "bstgmt") {
            ## how to extract BST/GMT
            ## first format date in local time
            x$date <- format(x$date, usetz = TRUE, tz = "Europe/London")
            ## extract ids where BST/GMT
            id.BST <- grep("BST", x$date)
            id.GMT <- grep("GMT", x$date)

            bst <- x[id.BST, ]
            bst[ , type] <- "BST hours"

            gmt <- x[id.GMT, ]
            gmt[ , type] <- "GMT hours"
            
            x <- rbind.fill(bst, gmt)
            x[ , type] <- factor(x[ , type])
            x$date <- as.POSIXct(x$date, "GMT")
            x <- x[order(x$date), ]

        }

        x
    }

    for (i in 1:length(type)) {
        x <- makeCond(x, type[i])
    }
    x

}
