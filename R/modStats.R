
modStats <- function(mydata,  mod = "mod", obs = "obs", type = "default", ...) {
    ## function to calculate model evaluation statistics
    ## the default is to use the entire data set.
    ## Requires a field "date" and optional conditioning variables representing measured and modelled values

     ## extract variables of interest
    vars <- c(mod, obs)

    if (any(type %in%  dateTypes)) vars <- c("date", vars)

    ## check the data
    mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)

    mydata <- cutData(mydata, type, ...)

     ## number of valid readings
    n <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <- nrow(x)
        data.frame(n = res)
    }

    ## fraction within a factor of two
    FAC2 <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        ratio <- x[, mod] / x[, obs]
        res <- length(which(ratio >= 0.5 & ratio <= 2)) / nrow(x)
        data.frame(FAC2 = res)
    }

    ## mean bias
    MB <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <- mean(x[, mod] - x[, obs])
        data.frame(MB = res)
    }

    ## mean gross error
    MGE <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <- mean(abs(x[, mod] - x[, obs]))
        data.frame(MGE = res)
    }

    ## normalised mean bias
    NMB <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <- sum(x[, mod] - x[, obs]) / sum(x[, obs])
        data.frame(NMB = res)
    }

    ## normalised mean gross error
    NMGE <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <- sum(abs(x[, mod] - x[, obs])) / sum(x[, obs])
        data.frame(NMGE = res)
    }

    ## root mean square error
    RMSE <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <- mean((x[, mod] - x[, obs]) ^ 2) ^ 0.5
        data.frame(RMSE = res)
    }

    ## correlation coefficient
    r <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <- cor(x[ , mod], x[ , obs])
        data.frame(r = res)
    }

    ## calculate the various statistics
    res.n <- ddply(mydata, type, n, mod, obs)
    res.FAC <- ddply(mydata, type, FAC2, mod, obs)
    res.MB <- ddply(mydata, type, MB, mod, obs)
    res.MGE <- ddply(mydata, type, MGE, mod, obs)
    res.NMB <- ddply(mydata, type, NMB, mod, obs)
    res.NMGE <- ddply(mydata, type, NMGE, mod, obs)
    res.RMSE <- ddply(mydata, type, RMSE, mod, obs)
    res.r <- ddply(mydata, type, r, mod, obs)

    ## merge them all into one data frame
    results <- list(res.n, res.FAC, res.MB, res.MGE, res.NMB, res.NMGE, res.RMSE, res.r)
    results <- Reduce(function(x, y, by = type) merge(x, y, by = type, all = TRUE), results)

    results <- sortDataFrame(results, key = type)

    results

}

sortDataFrame <- function(x, key, ...) {
    ## function to sort a data frame given one or more column names (key)
    ## from http://tolstoyc.newcastle.edu.au/R/help/04/07/1076.html

    if (missing(key)) {

        rn <- rownames(x)
        if (all(rn %in% 1:nrow(x))) rn <- as.numeric(rn)
        x[order(rn, ...), , drop = FALSE]
    } else {
        x[do.call("order", c(x[key], ...)), , drop = FALSE]
    }
}

