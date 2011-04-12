
selectRunning <- function (mydata, pollutant = "nox", run.len = 5, threshold = 500) {
    
    ## function to return indices of running values above a certain threshold
    ## make sure the time series is continuous in time with NO gaps
    vars <- c("date", pollutant)

    ## pad out missing data
    thedata <- date.pad(mydata)
    mydata <- thedata ## save for later
    thedata <- checkPrep(mydata, vars, type = "default")
         
    x <- thedata[, pollutant]
    rle.seq = rle(x > threshold)
    cumsum.seq <- cumsum(rle.seq$lengths)
    myruns <- which(rle.seq$values == 1 & rle.seq$lengths >= run.len)

    ends <- cumsum.seq[myruns] 
    newindex <- ifelse(myruns > 1, myruns - 1, 0)
    starts <- cumsum.seq[newindex] + 1
    if (0 %in% newindex) starts = c(1, starts)
    res <- data.frame(starts = starts, ends = ends)
    
    if (nrow(res) > 0) {
        ids <- lapply(1:nrow(res), function(x) seq(res[x, 1], res[x,2]))
        ids <- do.call(c, ids)
        mydata[ids, ]
    } else {
        print("No conditions found that match criteria")
    }
   
}

