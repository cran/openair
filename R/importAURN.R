
importAURN <- function(site = "my1", year = 2009, pollutant = "all", hc = FALSE) {
    site <- toupper(site)

    ## RData files to import
    ## doamin changed, Feb 2011!
    files <- lapply(site, function (x) paste("http://uk-air.defra.gov.uk/openair/R_data/", x, "_", year, ".RData",
                                             sep = ""))    
 #   files <- lapply(site, function (x) paste("http://www.airquality.co.uk/R_data/", x, "_", year, ".RData", sep = ""))
    files <- do.call(c, files)

    loadData <- function(x) {
        tryCatch({load(url(x), envir=.GlobalEnv)}, error = function(ex) {cat(x, "does not exist - ignoring that one.\n")})
    }

    thedata <- lapply(files, loadData)
    closeAllConnections()
    theObjs <- unlist(thedata)
    ## note unlist will drop NULLs from non-existant sites/years
    mylist <- lapply(theObjs, get)

    thedata <- do.call(rbind.fill, mylist)
    thedata$site <- factor(thedata$site, levels = unique(thedata$site))

    ## change names
    names(thedata) <- tolower(names(thedata))

    ## change nox as no2
    id <- which(names(thedata) %in% "noxasno2")
    if (length(id) == 1) names(thedata)[id] <- "nox"

    ## should hydrocarbons be imported?
    if (hc) {
        thedata <- thedata
         } else {
             ## no hydrocarbons - therefore select conventional pollutants
             theNames <- c("date", "co", "nox", "no2", "no", "o3", "so2", "pm10", "pm2.5",
                           "v10", "v2.5", "ws", "code", "site")

             thedata <- thedata[,  which(names(thedata) %in% theNames)]
         }

     ## if particular pollutants have been selected
    if (!missing(pollutant)) thedata <- thedata[, c("date", pollutant, "site", "code")]

    rm(list = theObjs, pos = 1)

    ## warning about recent, possibly unratified data
    timeDiff <- difftime(Sys.time(),  max(thedata$date), units='days')
    if (timeDiff < 180) {
    warning("You have selected some data that is less than 6-months old.\n This most recent data is not yet ratified and may be changed\n during the QA/QC process. For complete information about the \nratification status of a data set, please use the online tool at:\n http://www.airquality.co.uk/data_and_statistics.php?action=da_1&go=Go")}

     ## make sure it is in GMT
    attr(thedata$date, "tzone") <- "GMT"
    
    thedata
}
