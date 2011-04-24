## function to provide a summary of air quality statistics by year
aqStats <- function(mydata, pollutant = "no2", data.thresh = 75, percentile = c(95, 99),
                    transpose = FALSE, ...) {

    ## check data and add 'ste' filed if not there
    if (!"site" %in% names(mydata)) mydata$site <- "site"

    vars <- c("date", pollutant, "site")
    mydata <- checkPrep(mydata, vars, "default")

    ## pre-defined lits of pollutants that need special treatment
    thePolls <- c("no2", "o3", "pm10", "co")
    
    calcStats <- function(mydata, pollutant, percentile, ...) { 
        
        ## file any missing hours
        start.date <- as.POSIXct(dateTrunc(min(mydata$date), "year"))
        end.date <- as.POSIXct(dateCeil(max(mydata$date), "year") - 3600)

        ## find time interval of data and pad any missing times
        interval <- find.time.interval(mydata$date)
        all.dates <- data.frame(date = seq(start.date, end.date, by = interval))
        mydata <- merge(mydata, all.dates, all = TRUE)
        mydata$year <- format(mydata$date, "%Y")

        dataCapture <- function(mydata, ...){
            ## % data capture
            value <- mydata[ , pollutant]
            all.hours <- length(value)
            missing.hours <- sum(is.na(value))
            data.cap <- round(100 * (all.hours - missing.hours) / all.hours, 1)
            data.cap
        }

        daysMoreThan <- function(mydata, threshold, ...) {
            ## identify days where pm10 > limit
            daily <- timeAverage(mydata, "day", data.thresh)
            days <- length(which(daily[ , pollutant] > threshold))
            days
        }

        ozoneRolling <- function(mydata, ...) {
            ## first calculate rolling hourly means            
            mydata <- rollingMean(mydata, pollutant, hours = 8, new.name = "rolling",
                                  data.thresh)            
            daily <- timeAverage(mydata, avg.time = "day", statistic = "max", data.thresh)           
            days <- length(which(daily[ , "rolling"] > 100))
            days
        }

        hoursMoreThan <- function(mydata, threshold = 200, ...) {
            hours <- length(which(mydata[ , pollutant] > threshold))
            hours
        }

        AOT40 <- function(mydata, ...) {
            ## note the assumption is the O3 is in ug/m3
            AOT40 <- ifelse(mydata[ , pollutant] - 80 < 0 , 0, mydata[ , pollutant] - 80)
            AOT40 <- sum(AOT40, na.rm = TRUE) * 0.50 ## for ppb
            AOT40
        }

        maxDaily <- function(mydata, threshold = 50, ...) {
            maxDaily <- timeAverage(mydata, "day", statistic = "mean", data.thresh)            
            maxDaily <- max(maxDaily[ , pollutant], na.rm = TRUE)
            maxDaily
        }

        rollMax <- function(mydata, hours = hours, ...) {
            ## first calculate rolling hourly means
            mydata <- rollingMean(mydata, pollutant = pollutant, hours = hours, data.thresh,
                                  new.name = "rolling")
            rollMax <- max(mydata[ , "rolling"], na.rm = TRUE)
            rollMax
        }

        
        Mean <- ddply(mydata[ , c("year", pollutant)], .(year), numcolwise(mean), na.rm = TRUE)
        names(Mean)[2] <- "mean"

        Min <- ddply(mydata[ , c("year", pollutant)], .(year), numcolwise(min), na.rm = TRUE)
        names(Min)[2] <- "minimum"

        Max <- ddply(mydata[ , c("year", pollutant)], .(year), numcolwise(max), na.rm = TRUE)
        names(Max)[2] <- "maximum"

        maxDaily <- ddply(mydata[ , c("date", "year", pollutant)], .(year), maxDaily, ...)
        names(maxDaily)[2] <- "max.daily"        

        Median <- ddply(mydata[ , c("year", pollutant)], .(year), numcolwise(median),
                        na.rm = TRUE)
        names(Median)[2] <- "median"
        
        dataCapture <- ddply(mydata[ , c("year", pollutant)], .(year), dataCapture,
                             pollutant, ...)
        names(dataCapture)[2] <- "data.capture"

        rollMax8 <- ddply(mydata[ , c("date", "year", pollutant)], .(year),
                          rollMax, hours = 8, ...)
        names(rollMax8)[2] <- "max.rolling.8"

        rollMax24 <- ddply(mydata[ , c("date", "year", pollutant)], .(year),
                           rollMax, hours = 24, ...)
        names(rollMax24)[2] <- "max.rolling.24"

        
        ## use openair function
        Percentile <- calcPercentile(mydata[ , c("date", pollutant)],
                                     pollutant = pollutant, data.thresh,
                                     percentile = percentile, period = "year")
        names(Percentile)[1] <- "year"
        Percentile$year <- format(Percentile$year, "%Y")
        
        

        if (tolower(pollutant) == "o3") {
            rollingO3 <- ddply(mydata[ , c("date", "year", pollutant)], .(year),
                               ozoneRolling, ...)
            names(rollingO3)[2] <- "roll.8.O3.gt.100"

            AOT40 <- ddply(mydata[ , c("date", "year", pollutant)], .(year),
                           AOT40, ...)
            names(AOT40)[2] <- "AOT40"

            o3.results <- list(dataCapture, Mean, Min, Max, Median, maxDaily, rollMax8, rollMax24,
                               Percentile, rollingO3, AOT40)
            o3.results <- Reduce(function(x, y, by = 'year') merge(x, y, by = 'year',
                                                all = TRUE), o3.results)
            o3.results$pollutant <- "O3"
            results <- o3.results
        }

        if (tolower(pollutant) == "no2") {
            hours <- ddply(mydata[ , c("year", pollutant)], .(year), hoursMoreThan, threshold = 200,
                           ...)
            names(hours)[2] <- "hours.gt.200"

            no2.results <- list(dataCapture, Mean, Min, Max, Median,  maxDaily, rollMax8, rollMax24,
                                Percentile, hours)
            no2.results <- Reduce(function(x, y, by = 'year') merge(x, y, by = 'year',
                                                 all = TRUE), no2.results)
            no2.results$pollutant <- "NO2"
            results <- no2.results
        }

        if (tolower(pollutant) == "pm10") {
            days <- ddply(mydata[ , c("date", "year", pollutant)], .(year),
                          daysMoreThan, threshold = 50, ...)
            names(days)[2] <- "days.gt.50"
            
            pm10.results <- list(dataCapture, Mean, Min, Max, Median, maxDaily, rollMax8, rollMax24,
                                 Percentile, days)
            
            pm10.results <- Reduce(function(x, y, by = 'year') merge(x, y, by = 'year',
                                                  all = TRUE), pm10.results)
            pm10.results$pollutant <- "PM10"
            results <- pm10.results
        }

        if (tolower(pollutant) == "co") {
            
            co.results <- list(dataCapture, Mean, Min, Max, Median, maxDaily, rollMax8, rollMax24,
                               Percentile)
            co.results <- Reduce(function(x, y, by = 'year') merge(x, y, by = 'year',
                                                all = TRUE), co.results)
            co.results$pollutant <- "CO"
            results <- co.results
        }

        if (!any(tolower(pollutant) %in% thePolls)) {
            
            results <- list(dataCapture, Mean, Min, Max, Median, maxDaily, rollMax8, rollMax24,
                            Percentile)
            results <- Reduce(function(x, y, by = 'year') merge(x, y, by = 'year',
                                             all = TRUE), results)
            results$pollutant <- pollutant
            results <- results
            
        }
        
        
        results
    }

    ## function to go through sites

    bySite <- function (mydata, pollutant, ...) {

        ## dates should be unique; issue warning if not
        if (any(duplicated(mydata$date))) warning ("Duplicate dates detected - more than one site?",
                                                   call. = FALSE)
        
        
        results <- lapply(pollutant, function (x) calcStats(mydata = mydata, pollutant = x ,...))

        
        results <- do.call (rbind.fill, results)
        results$year <- as.numeric(results$year)
        results
    }   

    results <- ddply(mydata, .(site), bySite, pollutant = pollutant,
                     data.thresh = data.thresh,
                     percentile = percentile,...)
    ## order sensible
    results <- cbind(subset(results, select = c(site, pollutant)), subset(results,
                                     select = -c(site, pollutant)))
    class(results$year) <- "integer"

    ## transpose if requested
    if (transpose) {
        if (length(unique(results$site)) > 1) {
            results <- melt(results, id.vars = c("site", "pollutant", "year"))
            results <- cast(results, ... ~ site + pollutant)
        } else {
            ## only one site and don't need to add name
            results <- subset(results, select = -site)
            results <- melt(results, id.vars = c("pollutant", "year"))
            results <- cast(results, ... ~ pollutant)
        }
        ## sort out names
        names(results) <- gsub("\\_", " ", names(results))
    }
    results
    
}
