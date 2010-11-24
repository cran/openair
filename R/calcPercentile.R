## function to calculate percentiles from time series data
## For SINGLE pollutants and MULTIPLE percentile values

calcPercentile <- function(mydata, pollutant = "o3", period = "month", percentile = 50,
                            data.thresh = 0, start = NA) {
    site <- FALSE
    if ("site" %in% names(mydata)) {
        site <- TRUE
        site.name <- mydata$site[1]
    }

    make.percentile <- function(mydata, pollutant = "o3", period = "month", percentile = 50,
                                data.thresh = 0, start = NA) {

        mydata <- timeAverage(mydata, period, statistic = "percentile", percentile = percentile,
                               data.thresh = 0, start.date = NA)
        ## change column name
        new.name <-  paste("percentile.", percentile,  sep = "")
        names(mydata)[names(mydata) == pollutant] <- new.name
        results <- mydata[, new.name, drop = FALSE]
        results <- data.frame(date = mydata$date, results)

        results


    }

    mydata <- lapply(percentile, function(x) make.percentile(mydata, pollutant = pollutant,
                                                               period = period, percentile = x))
    mydata <- Reduce(function(x, y, by = 'date') merge(x, y, by = 'date', all = TRUE), mydata)

    if (site) mydata$site <- site.name
    mydata
}
