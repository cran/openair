
importAURN <- function(site = "my1", year = 2009, pollutant = "all", hc = FALSE) {
    site <- toupper(site)
 

    files <- lapply(site, function (x) paste(x, "_", year, sep = ""))    
 
    files <- do.call(c, files)
   

    loadData <- function(x) {
        tryCatch({
             fileName <- paste("http://uk-air.defra.gov.uk/openair/R_data/", x, ".RData", sep = "")
             load(url(fileName), envir = .GlobalEnv)          
             x
             },
                  error = function(ex) {cat(x, "does not exist - ignoring that one.\n")})
         
     }

    thedata <- lapply(files, loadData)
   
    theObjs <- unlist(thedata)
    ## note unlist will drop NULLs from non-existant sites/years
    mylist <- lapply(theObjs, get)

    thedata <- do.call(rbind.fill, mylist)
    if (is.null(thedata)) stop("No data to import - check site codes and year.", call. = FALSE)
    
    thedata$site <- factor(thedata$site, levels = unique(thedata$site))
    
    ## change names
    names(thedata) <- tolower(names(thedata))

    ## change nox as no2
    id <- which(names(thedata) %in% "noxasno2")
    if (length(id) == 1) names(thedata)[id] <- "nox"

    ## set class to integer for post-processing convenience
    if ("nox" %in% names(thedata)) class(thedata$nox) <- "integer"
    if ("no" %in% names(thedata)) class(thedata$no) <- "integer"
    if ("no2" %in% names(thedata)) class(thedata$no2) <- "integer"
    if ("o3" %in% names(thedata)) class(thedata$o3) <- "integer"
    if ("so2" %in% names(thedata)) class(thedata$so2) <- "integer"
    if ("pm10" %in% names(thedata)) class(thedata$pm10) <- "integer"
    if ("pm2.5" %in% names(thedata)) class(thedata$pm2.5) <- "integer"
    if ("v10" %in% names(thedata)) class(thedata$v10) <- "integer"
    if ("v2.5" %in% names(thedata)) class(thedata$v2.5) <- "integer"


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
