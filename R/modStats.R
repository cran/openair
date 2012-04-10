##' Calculate common model evaluation statistics
##'
##' Function to calculate common numerical model evaluation statistics with
##' flexible conditioning
##'
##' This function is under development and currently provides some common model
##' evaluation statistics. These include (to be mathematically defined later):
##'
##' \itemize{
##'
##' \item \eqn{n}, the number of complete pairs of data.
##'
##' \item \eqn{FAC2}, fraction of predictions within a factor of two.
##'
##' \item \eqn{MB}, the mean bias.
##'
##' \item \eqn{MGE}, the mean gross error.
##'
##' \item \eqn{NMB}, the normalised mean bias.
##'
##' \item \eqn{NMGE}, the normalised mean gross error.
##'
##' \item \eqn{RMSE}, the root mean squared error.
##'
##' \item \eqn{r}, the Pearson correlation coefficient.
##'
##' \item \eqn{IOA}, the Index of Agreement based on Willmott et
##' al. (2011), which spans between -1 and +1 with values approaching
##' +1 representing better model performance.
##'
##' An IOA of 0.5, for example, indicates that the sum of the
##' error-magnitudes is one half of the sum of the observed-deviation
##' magnitudes.  When IOA = 0.0, it signifies that the sum of the
##' magnitudes of the errors and the sum of the observed-deviation
##' magnitudes are equivalent. When IOA = -0.5, it indicates that the
##' sum of the error-magnitudes is twice the sum of the perfect
##' model-deviation and observed-deviation magnitudes. Values of IOA
##' near -1.0 can mean that the model-estimated deviations about O are
##' poor estimates of the observed deviations; but, they also can mean
##' that there simply is little observed variability - so some caution
##' is needed when the IOA approaches -1.
##'
##' }
##'
##' All statistics are based on complete pairs of \code{mod} and \code{obs}.
##'
##' Conditioning is possible through setting \code{type}.
##'

##'
##' @param mydata A data frame.
##' @param mod Name of a variable in \code{mydata} that respresents modelled
##'   values.
##' @param obs Name of a variable in \code{mydata} that respresents measured
##'   values.
##' @param type \code{type} determines how the data are split i.e. conditioned,
##'   and then plotted. The default is will produce statistics using the entire
##'   data. \code{type} can be one of the built-in types as detailed in
##'   \code{cutData} e.g. "season", "year", "weekday" and so on. For example,
##'   \code{type = "season"} will produce four sets of statistics --- one for
##'   each season.
##'
##' It is also possible to choose \code{type} as another variable in the data
##'   frame. If that variable is numeric, then the data will be split into four
##'   quantiles (if possible) and labelled accordingly. If type is an existing
##'   character or factor variable, then those categories/levels will be used
##'   directly. This offers great flexibility for understanding the variation
##'   of different variables and how they depend on one another.
##'
##' More than one type can be considered e.g. \code{type = c("season",
##'   "weekday")} will produce statistics split by season and day of the week.
##' @param rank.name Simple model ranking can be carried out if
##' \code{rank.name} is supplied. \code{rank.name} will generally
##' refer to a column representing a model name, which is to
##' ranked. The ranking is based the Index of Agreement performance,
##' as that indicator is arguably the best single model performance
##' indicator available.
##'
##'
##' @param ... Other aruments to be passed to \code{cutData} e.g.
##'   \code{hemisphere = "southern"}
##' @export
##' @return Returns a data frame with model evaluation statistics.
##' @author David Carslaw
##' @references
##' Willmott, C.J., Robeson, S.M., Matsuura, K., 2011. A
##' refined index of model performance. International Journal of
##' Climatology.
##' @keywords methods
##' @examples
##'
##' ## the example below is somewhat artificial --- assuming the observed
##' ## values are given by NOx and the predicted values by NO2.
##'
##' modStats(mydata, mod = "no2", obs = "nox")
##'
##' ## evaluation stats by season
##'
##' modStats(mydata, mod = "no2", obs = "nox", type = "season")
##'
##'
modStats <- function(mydata,  mod = "mod", obs = "obs", type = "default", rank.name = NULL, ...) {
    ## function to calculate model evaluation statistics
    ## the default is to use the entire data set.
    ## Requires a field "date" and optional conditioning variables representing measured and modelled values

    ## extract variables of interest
    vars <- c(mod, obs)

    if (any(type %in%  openair:::dateTypes)) vars <- c("date", vars)

    ## check the data
    mydata <- openair:::checkPrep(mydata, vars, type, remove.calm = FALSE)

    mydata <- cutData(mydata, type, ...)

    ## calculate the various statistics
    res.n <- ddply(mydata, type, n, mod, obs)
    res.FAC <- ddply(mydata, type, FAC2, mod, obs)
    res.MB <- ddply(mydata, type, MB, mod, obs)
    res.MGE <- ddply(mydata, type, MGE, mod, obs)
    res.NMB <- ddply(mydata, type, NMB, mod, obs)
    res.NMGE <- ddply(mydata, type, NMGE, mod, obs)
    res.RMSE <- ddply(mydata, type, RMSE, mod, obs)
    res.r <- ddply(mydata, type, r, mod, obs)
    res.IOA <- ddply(mydata, type, IOA, mod, obs)

    ## merge them all into one data frame
    results <- list(res.n, res.FAC, res.MB, res.MGE, res.NMB, res.NMGE, res.RMSE, res.r, res.IOA)
    results <- Reduce(function(x, y, by = type) merge(x, y, by = type, all = TRUE), results)

    results <- sortDataFrame(results, key = type)

    ## simple ranking of models?
    if (!is.null(rank.name)) {

        types <- setdiff(type, rank.name)

        if (length(types) == 0) {
            results <- rankModels(results, rank.name)
        } else {

            results <- ddply(results, types, rankModels, rank.name = rank.name)
        }

    }

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


rankModels <- function(mydata, rank.name = "group") {

    ## sort by IOA
    mydata <- sortDataFrame(mydata, "IOA", decreasing = TRUE)
}

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
    res <- suppressWarnings(cor(x[ , mod], x[ , obs])) ## when SD=0
    data.frame(r = res)
}

##  Index of Agreement
IOA <- function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[ , c(mod, obs)])

    LHS <- sum(abs(x[, mod] - x[, obs]))
    RHS <- 2 * sum(abs(x[, obs] - mean(x[, obs])))

    if (LHS <= RHS) res <- 1 - LHS / RHS else res <- RHS / LHS - 1

    data.frame(IOA = res)
}

