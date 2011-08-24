importAURNCsv <- function (file = file.choose(), header.at = 5, data.at = 7, na.strings = c("No data", 
    "", "NA"), date.name = "Date", date.break = "-", time.name = "time", 
    misc.info = c(1, 2, 3, 4), is.site = 4, bad.24 = TRUE, correct.time = -3600, 
    output = "final", data.order = c("value", "status", "unit"), 
    simplify.names = TRUE, ...) 
{
    initial.ans <- import(file = file, header.at = header.at, 
        na.strings = na.strings, data.at = data.at, date.name = date.name, 
        date.break = date.break, time.name = time.name, misc.info = misc.info, 
        is.site = NULL, bad.24 = bad.24, correct.time = correct.time, 
        output = "working", ...)
    date.name <- make.names(date.name)
    time.name <- make.names(time.name)
    site.1 <- read.table(file, header = FALSE, sep = initial.ans$ops$sep, 
        skip = (is.site - 1), nrows = 1, colClasses = "character", 
        col.names = initial.ans$names, fill = TRUE, flush = TRUE)
    site.1 <- site.1[1:length(initial.ans$names)]
    names(site.1) <- make.names(initial.ans$names, unique = TRUE)
    site.1 <- site.1[!names(site.1) == date.name]
    site.1 <- site.1[!names(site.1) == time.name]

#revised site handler

    site.2 <- as.character(site.1)
    site.2 <- c(1:length(site.2))[gsub(" ", "", site.2) != ""]

#    site.2 <- as.vector(sapply(site.1[!as.character(site.1) == 
#        "" & !as.character(site.1) == " "], function(x) {
#        grep(x, as.character(site.1), fixed = TRUE)
#    }))

    if (length(site.2) > 1) {
        site.3 <- c(site.2[2:length(site.2)] - 1, ncol(site.1))
    }
    else {
        site.3 <- ncol(site.1)
    }
    site.names <- as.character(as.vector(site.1[site.2]))
    #space at name start
    site.names <- gsub("(^ +)|( +$)", "", site.names)

    initial.ans$data <- lapply(1:(length(site.2)), function(x) {
        ans <- initial.ans$data[site.2[x]:site.3[x]]
        ans.names <- names(ans)
        if (simplify.names == TRUE) {
            ans.names[grep("carbon.monoxide", ans.names, ignore.case = TRUE)] <- "co"
            ans.names[grep("pm10.particulate.matter", ans.names, 
                ignore.case = TRUE)] <- "pm10"
            ans.names[grep("non.volatile.pm10", ans.names, ignore.case = TRUE)] <- "nv.pm10"
            ans.names[grep("volatile.pm10", ans.names, ignore.case = TRUE)] <- "v.pm10"
            ans.names[grep("pm2.5.particulate.matter", ans.names, 
                ignore.case = TRUE)] <- "pm2.5"
            ans.names[grep("non.volatile.pm2.5", ans.names, ignore.case = TRUE)] <- "nv.pm2.5"
            ans.names[grep("volatile.pm2.5", ans.names, ignore.case = TRUE)] <- "v.pm2.5"
            ans.names[grep("nitric.oxide", ans.names, ignore.case = TRUE)] <- "no"
            ans.names[grep("nitrogen.oxides", ans.names, ignore.case = TRUE)] <- "nox"
            ans.names[grep("nitrogen.dioxide", ans.names, ignore.case = TRUE)] <- "no2"
            ans.names[grep("ozone", ans.names, ignore.case = TRUE)] <- "o3"
            ans.names[grep("sulphur.dioxide", ans.names, ignore.case = TRUE)] <- "so2"
        }
        for (i in 1:length(data.order)) {
            if (data.order[i] == "value") {
            }
            else {
                ans.names[grep(data.order[i], ans.names, ignore.case = TRUE)] <- paste(data.order[i], 
                  ".", ans.names[(grep(data.order[i], ans.names, 
                    ignore.case = TRUE)) - (i - 1)], sep = "")
            }
        }
        names(ans) <- ans.names
        site <- rep(site.names[x], nrow(initial.ans$data))
        ans <- cbind(date = initial.ans$date, site = site, ans)
    })
    initial.ans$data <- do.call(rbind.fill, initial.ans$data)
    if (simplify.names == TRUE) {
        initial.ans$misc <- c(initial.ans$misc, "importAURN operation: simplify names applied")
    }
    if (!output == "working") {
        ans <- initial.ans$data
        if (!is.null(misc.info)) {
            comment(ans) <- initial.ans$misc
        }
        ids <- which(is.na(ans$date))
        if (length(ids) > 0) {
            ans <- ans[-ids, ]
            warning(paste("Missing dates detected, removing", 
                length(ids), "lines"))
        }
        print(unlist(sapply(ans, class)))
        return(ans)
    }
    else {
        return(initial.ans)
    }
}
