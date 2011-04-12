## function to import R data objects from server


importKCL <- function(site = "my1", year = 2009, pollutant = "all", met = FALSE, units = "mass") {
  
    site <- toupper(site)

    ## rows with these site codes
    ## this preserves order of site names
    load(url(paste("http://www.londonair.org.uk/r_data/", "sites", ".RData", sep = "")))
    closeAllConnections()
    id <- sapply(site, function(x) which(sites$SiteCode %in% toupper(x)))
    site.name <- sites$SiteName[id]

    ## RData files to import
    files <- lapply(site, function (x) paste("http://www.londonair.org.uk/r_data/", x, "_", year, ".RData", sep = ""))
    files <- do.call(c, files)

    thedata <- suppressWarnings(lapply(files, function(file) tryCatch({get(load(url(file)))}, error = function(ex) {cat(file, "does not exist - ignoring that one.\n")})))
    thedata <- do.call(rbind.fill, thedata)
    closeAllConnections()
    
    thedata$code <- thedata$site
    
    thedata$site <- factor(thedata$site, labels = site.name, levels = site)
    

    ## change names
    names(thedata) <- tolower(names(thedata))

    ## if particular pollutants have been selected
    if (!missing(pollutant)) {
        if (pollutant != "all") {
            thedata <- thedata[, c("date", pollutant, "site", "code")]
        }
    }

    ## change units to mass units, use values in ugm3Conversion table
    if (units == "mass") {
        if ("nox" %in% names(thedata)) thedata$nox <- thedata$nox * 1.91
        if ("no2" %in% names(thedata)) thedata$no2 <- thedata$no2 * 1.91
        if ("o3" %in% names(thedata)) thedata$o3 <- thedata$o3 * 2.00
        if ("so2" %in% names(thedata)) thedata$so2 <- thedata$so2 * 2.66
        if ("co" %in% names(thedata)) thedata$co <- thedata$co * 1.16
        if ("pm10_raw" %in% names(thedata)) thedata$pm10_raw <- thedata$pm10_raw* 1.30
       
        unitMessage <- "NOTE - mass units are used \nug/m3 for NOx, NO2, SO2, O3; mg/m3 for CO\nPM10_raw is raw data multiplied by 1.3\n"        
    }

    if (units != "mass")  {
        if ("pm10" %in% names(thedata)) thedata$pm10_raw <- thedata$pm10_raw* 1.30
        unitMessage <- "NOTE - volume units are used \nppbv for NOx, NO2, SO2, O3; ppmv for CO\nPM10_raw is raw data multiplied by 1.3\n"
    }

    ## warning about recent, possibly unratified data
    timeDiff <- difftime(Sys.time(),  max(thedata$date), units='days')
    if (timeDiff < 180) {
        warning("Some of the more recent data may not be ratified.")}

    if (met) {  ## merge met data
        load(url(paste("http://www.londonair.org.uk/r_data/", "metData", ".RData", sep = "")))
        closeAllConnections()
        thedata <- merge(thedata, met, by = "date")
    }

    ## make sure it is in GMT
    attr(thedata$date, "tzone") <- "GMT"
    thedata <- thedata[order(thedata$site, thedata$date), ]

    cat(unitMessage)
    
    thedata
}
