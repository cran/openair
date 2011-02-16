cutData <- function(x, type = "default", hemisphere = "northern", n.levels = 4, is.axis = FALSE, ...) {

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
                   "bstgmt", "gmtbst", "daylight")

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

        if (type == "daylight") {
            x <- cutDaylight(x, ...)
        }

        x
    }

    for (i in 1:length(type)) {
        x <- makeCond(x, type[i])
    }
    x

}

###########################################################################################
#cutDaylight function

cutDaylight <- function(mydata, local.hour.offset = 0,
                  latitude = 0, longitude = 0, ... 
                  ){

##long, hour.off

#condition openair data by daylight
#using date (POSIXt) 
#kr v 0.2
#################################
#based on noaa methods
#http://www.srrb.noaa.gov/highlights/sunrise/calcdetails.html
#by Chris Cornwall, Aaron Horiuchi and Chris Lehman
#

######################
#notes
######################
#calculations use
#(lat, long) position relative to sun
#to estimate if daylight or nighttime hour
######################
#solar.noon.lst, etc are factions of day 
#seconds into that day = p.time * 86400 
#so for example sunset time is
#as.POSIXct(sunset.time.lst * 86400, origin = format(mydata$date, "%Y-%m-%d"))
#(assuming you do not run into next day!)
######################
#currently unsure about extremes 
#long nights and days at poles need checking
# 

##################
#suggestions:
##################
#local hour offset could be a lookup table linked to tz
#

if(!"POSIXt" %in% class(mydata$date))
   stop("required field 'date' missing or not POSIXt\n", call. = FALSE)

###################
#temp functions
###################
rad <- function(x) x * pi / 180
degrees <- function(x) x * (180 / pi)

###############
#get local time
###############
temp <- mydata$date 

#################
#make julian.refs
#################
#ref Gregorian calendar back extrapolated.
#assumed good for years between 1800 and 2100 

p.day <- (as.numeric(format(temp, "%H")) * 3600) +
         (as.numeric(format(temp, "%M")) * 60) +
         as.numeric(format(temp, "%S"))
p.day <- p.day/86400

#julian century (via julian day)
julian.century <- as.numeric(as.Date(temp, format= "%m/%d/%Y")) + 2440587.5 + p.day - (local.hour.offset/24)
julian.century <- (julian.century-2451545)/36525

##################
#main calcs
##################
#as of noaa

geom.mean.long.sun.deg <- (280.46646 + julian.century *(36000.76983 + julian.century * 0.0003032)) %% 360

geom.mean.anom.sun.deg <- 357.52911 + julian.century * (35999.05029 - 0.0001537 * julian.century)

eccent.earth.orbit <- 0.016708634 - julian.century * (0.000042037 + 0.0001537 * julian.century)

sun.eq.of.ctr <- sin(rad(geom.mean.anom.sun.deg)) * 
                 (1.914602 - julian.century * (0.004817 + 0.000014*julian.century)) + 
                 sin(rad(2 * geom.mean.anom.sun.deg)) * 
                 (0.019993 - 0.000101*julian.century) + 
                 sin(rad(3 * geom.mean.anom.sun.deg)) * 0.000289

sun.true.long.deg <- sun.eq.of.ctr + geom.mean.long.sun.deg

sun.app.long.deg <- sun.true.long.deg - 0.00569 - 0.00478 * 
                    sin(rad(125.04 - 1934.136 * julian.century))

mean.obliq.ecliptic.deg <- 23 + (26 + ((21.448 - julian.century * 
                           (46.815 + julian.century * 
                           (0.00059 - julian.century 
                           * 0.001813)))) / 60) / 60

obliq.corr.deg <- mean.obliq.ecliptic.deg + 
                  0.00256 * cos(rad(125.04 - 1934.136 * julian.century))

sun.declin.deg <- degrees(asin(sin(rad(obliq.corr.deg)) * 
                  sin(rad(sun.app.long.deg))))

vary <- tan(rad(obliq.corr.deg / 2)) * tan(rad(obliq.corr.deg/2))

eq.of.time.minutes <- 4 * degrees(vary * sin(2 * rad(geom.mean.long.sun.deg)) - 
                      2 * eccent.earth.orbit * sin(rad(geom.mean.anom.sun.deg)) + 
                      4 * eccent.earth.orbit * vary * sin(rad(geom.mean.anom.sun.deg)) * 
                      cos(2 * rad(geom.mean.long.sun.deg)) - 0.5 * vary * vary * 
                      sin(4 * rad(geom.mean.long.sun.deg)) - 1.25 * eccent.earth.orbit * 
                      eccent.earth.orbit * sin(2 * rad(geom.mean.anom.sun.deg)))

#original nooa code
##
#ha.sunrise.deg <- degrees(acos(cos(rad(90.833)) / 
#                  (cos(rad(latitude)) * cos(rad(sun.declin.deg))) - 
#                  tan(rad(latitude)) * tan(rad(sun.declin.deg))))
##
#R error catcher added
#for long nights>24hours/short nights<0

ha.sunrise.deg <- cos(rad(90.833)) / 
                  (cos(rad(latitude)) * cos(rad(sun.declin.deg))) - 
                  tan(rad(latitude)) * tan(rad(sun.declin.deg))
ha.sunrise.deg <- ifelse(ha.sunrise.deg > 1, 1, ha.sunrise.deg)
ha.sunrise.deg <- ifelse(ha.sunrise.deg < -1, -1, ha.sunrise.deg)
ha.sunrise.deg <- degrees(acos(ha.sunrise.deg))

solar.noon.lst <- (720 - 4 * longitude - eq.of.time.minutes + local.hour.offset * 60) / 1440

sunrise.time.lst <- solar.noon.lst - ha.sunrise.deg * 4 / 1440

sunset.time.lst <- solar.noon.lst + ha.sunrise.deg * 4 / 1440

sunlight.duration.minutes <- 8 * ha.sunrise.deg

#################################
#daylight factor
#################################
#need to confirm dusk/dawn handing

daylight <- ifelse(sunlight.duration.minutes==0, FALSE,
                 ifelse(sunlight.duration.minutes==1440, TRUE,
                     ifelse(sunrise.time.lst<sunset.time.lst,
                          ifelse(p.day < sunset.time.lst & p.day > sunrise.time.lst, TRUE, FALSE),
                          ifelse(p.day <= sunrise.time.lst & p.day >= sunset.time.lst, FALSE, TRUE)
             )))
#as ordered factor 
daylight <- factor(daylight, levels=c(TRUE, FALSE), labels=c("daylight", "nighttime"))

###############################
#output 
###############################
mydata <- cbind(mydata, daylight = daylight)

}


