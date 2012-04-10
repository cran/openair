trendLevelHour <- function(mydata, ...) {
   trendLevel(mydata, ...)
}

trendLevelWd <- function(mydata,
   pollutant = "nox",
   y = "wd", ylab = "wind direction (degrees)", ...) {
   trendLevel(mydata, y = y, ylab = ylab, ...)
}



##' trendLevel
##'
##' The trendLevel function provides a way of rapidly showing a large amount of
##' data in a condensed form. In one plot, the variation in the concentration
##' of one pollutant can to shown as a function of three other properties. The
##' default version of the plot uses y = time of day, x = month of year and
##' type = year to provide information on trends, seasonal effects and diurnal
##' variations. However, x, y and type and summarising statistics can all be
##' modified to provide a range of other similar plots.
##'
##' \code{trendLevel} allows the use of third party summarising functions via
##' the \code{statistic} option. Any additional function arguments not included
##' within a function called using \code{statistic} should be supplied as a
##' list of named parameters and sent using \code{stat.args}. For example, the
##' encoded option \code{statistic = "mean"} is equivalent to \code{statistic =
##' mean, stat.args = list(na.rm = TRUE)} or the R command \code{mean(x, na.rm=
##' TRUE)}.  Many R functions and user`s own code could be applied in a similar
##' fashion, subject to the following restrictions: the first argument sent to
##' the function must be the data series to be analysed; the name `x' cannot be
##' used for any of the extra options supplied in \code{stat.args}; and the
##' function should return the required answer as a numeric or NA. Note: If the
##' supplied function returns more than one answer, currently only the first of
##' these is retained and used by \code{trendLevel}. All other returned
##' information will be ignored without warning. If the function terminates
##' with an error when it is sent an empty data series, the option
##' \code{stat.safe.mode} should not be set to \code{FALSE} or
##' \code{trendLevel} may fail. Note: The \code{stat.safe.mode = TRUE} option
##' returns an NA without warning for empty data series.
##'
##' @aliases trendLevel trendLevelHour trendLevelWd
##' @param mydata The openair data frame to use to generate the
##'   \code{trendLevel} plot.
##' @param pollutant The name of the data series in \code{mydata} to sample to
##'   produce the \code{trendLevel} plot.
##' @param x The name of the data series to use as the \code{trendLevel}
##'   x-axis. This is used with the \code{y} and \code{type} options to bin the
##'   data before applying \code{statistic} (see below). Allowed options
##'   currently include \code{"hour"}, \code{"month"}, \code{"year"}, and
##'   \code{"wd"}. Other data series in \code{mydata} can also be used. (Note:
##'   \code{trendLevel} does not allow duplication in \code{x}, \code{y} and
##'   \code{type} options within a call.)
##' @param y,type The names of the data series to use as the \code{trendLevel}
##'   y-axis and for additional conditioning, respectively. As \code{x} above.
##' @param rotate.axis The rotation to be applied to \code{trendLevel} \code{x}
##'   and \code{y} axes. The default, \code{c(90, 0)}, rotates the x axis by 90
##'   degrees but does not rotate the y axis. (Note: If only one value is
##'   supplied, this is applied to both axes; if more than two values are
##'   supplied, only the first two are used.)
##' @param n.levels The number of levels to split \code{x}, \code{y} and
##'   \code{type} data into if numeric. The default, \code{c(10, 10, 4)}, cuts
##'   numeric \code{x} and \code{y} data into ten levels and numeric
##'   \code{type} data into four levels. (Notes: This option is ignored for
##'   date conditioning and factors.  If less than three values are supplied,
##'   three values are determined by recursion; if more than three values are
##'   supplied, only the first three are used.)
##' @param limits The colour scale range to use when generating the
##'   \code{trendLevel} plot.
##' @param cols The colour set to use to colour the \code{trendLevel} surface.
##'   \code{cols} is passed to \code{openColours} for evaluation. See
##'   \code{?openColours} for more details.
##' @param auto.text Automatic routine text formatting. \code{auto.text = TRUE}
##'   passes common \code{lattice} labelling terms (e.g. \code{xlab} for the
##'   x-axis, \code{ylab} for the y-axis and \code{main} for the title) to the
##'   plot via \code{quickText} to provide common text formatting.  The
##'   alternative \code{auto.text = FALSE} turns this option off and passes any
##'   supplied labels to the plot without modification.
##' @param key.header,key.footer Adds additional text labels above and/or below
##'   the scale key, respectively. For example, passing the options
##'   \code{key.header = "", key.footer = c("mean","nox")} adds the addition
##'   text as a scale footer. If enabled (\code{auto.text = TRUE}), these
##'   arguments are passed to the scale key (\code{drawOpenKey}) via
##'   \code{quickText} to handle formatting. The term \code{"get.stat.name"},
##'   used as the default \code{key.header} setting, is reserved and
##'   automatically adds statistic function names or defaults to \code{"level"}
##'   when unnamed functions are requested via \code{statistic}.
##' @param key.position Location where the scale key should be plotted.
##'   Allowed arguments currently include \code{"top"}, \code{"right"},
##'   \code{"bottom"} and \code{"left"}.
##' @param key Fine control of the scale key via \code{drawOpenKey}. See
##'   \code{?drawOpenKey} for further details.
##' @param statistic The statistic method to be use to summarise locally binned
##'   \code{pollutant} measurements with. Three options are currently encoded:
##'   \code{"mean"} (default), \code{"max"} and \code{"frequency"}. (Note:
##'   Functions can also be sent directly via \code{statistic}.  However, this
##'   option is still in development and should be used with caution. See
##'   Details below.)
##' @param stat.args Additional options to be used with \code{statistic} if
##'   this is a function. The extra options should be supplied as a list of
##'   named parameters. (see Details below.)
##' @param stat.safe.mode An addition protection applied when using functions
##'   direclty with \code{statistic} that most users can ignore. This option
##'   returns \code{NA} instead of running \code{statistic} on binned
##'   subsamples that are empty. Many common functions terminate with an error
##'   meassage when applied to an empty dataset. So, this options provides a
##'   mechanism to work with such functions. For a very few cases, e.g. for a
##'   function that counted missing entries, it might need to be set to
##'   \code{FALSE} (see Details below.)
##' @param drop.unused.types Hide unused/empty \code{type} conditioning cases.
##'   Some conditioning options may generate empty cases for some data sets,
##'   e.g. a hour of the day when no measurements were taken. Empty \code{x}
##'   and \code{y} cases generate 'holes' in individual plots. However, empty
##'   \code{type} cases would produce blank panels if plotted. Therefore, the
##'   default, \code{TRUE}, excludes these empty panels from the plot. The
##'   alternative \code{FALSE} plots all \code{type} panels.
##' @param ...  Addition options are passed on to \code{cutData} for
##'   \code{type} handling and \code{levelplot} in \code{lattice} for finer
##'   control of the plot itself.
##' @export
##' @return As well as generating the plot itself, \code{trendLevel} also
##'   returns an object of class ``openair''. The object includes three main
##'   components: \code{call}, the command used to generate the plot;
##'   \code{data}, the data frame of summarised information used to make the
##'   plot; and \code{plot}, the plot itself. If retained, e.g. using
##'   \code{output <- trendLevel(mydata)}, this output can be used to recover
##'   the data, reproduce or rework the original plot or undertake further
##'   analysis.
##'
##' An openair output can be manipulated using a number of generic operations,
##'   including \code{print}, \code{plot} and \code{summary}. See
##'   \code{\link{openair.generics}} for further details.
##'
##' Summary statistics can also be extracted directly using \code{results},
##'   e.g.  \code{results(object)} for \code{output <- trendLevel(mydata)}.
##' @author Karl Ropkins
##' @seealso \code{\link{openColours}} and \code{\link{drawOpenKey}} for more
##'   detailed plot control and \code{\link{openair.generics}} for output
##'   handling.
##' @keywords methods
##' @examples
##'
##'
##' #basic use
##' #default statistic = "mean"
##' trendLevel(mydata, pollutant = "nox")
##'
##' #applying same as 'own' statistic
##' my.mean <- function(x) mean(x, na.rm = TRUE)
##' trendLevel(mydata, pollutant = "nox", statistic = my.mean)
##'
##' #alternative for 'third party' statistic
##' #trendLevel(mydata, pollutant = "nox", statistic = mean,
##' #           stat.args = list(na.rm = TRUE))
##'
##'
trendLevel <- function(mydata,
    pollutant = "nox",
    x = "month", y = "hour",
    type = "year",
    rotate.axis = c(90, 0),
    n.levels = c(10, 10, 4),
    limits = c(0, 100),
    cols = "default",
    auto.text = TRUE,
    key.header = "use.stat.name",
    key.footer = pollutant,
    key.position = "right",
    key = TRUE,
    statistic = c("mean", "max", "frequency"),
    stat.args = NULL,
    stat.safe.mode = TRUE,
    drop.unused.types = TRUE,
    ##plot.nas = FALSE,
    ...)
{

    #Generic levelplot function for summarising large data sets
    #kr v.05
    #based on previous trend.level.hour and trend.level.wd functions by dcc
    #revision to handle more x, y and type options
    #revision to handle xlim and axis labels more cleanly
    #minor revision for openair colour key
    #revision for checkPrep and cutData (cutData2)

    #examples
    ##trendLevel(mydata) #old trend.level.hour
    ##trendLevel(mydata, y = "wd") #old trend.level.wd nearly
    ##trendLevel(mydata, y = "wd", ylab = "wind direction (degrees)")
    #extra control
    ##trendLevel(mydata, x="hour", y="wd", type="month")
    #extra conditioning
    #by another pollutant/measurement
    ##trendLevel(mydata, y="pm10")
    #statistic will accept function
    ##with right stat.args and stat.safe.mode
    ##statistic is try protected for local error messaging

    #suggestions
    ############
    #the xlim amd ylim handling have been removed
    #also the labelling is a less clean
    ##these all came from make.axis which was replaced by cutData
    ##suggest added a 'son of make.axis that could be tagged on the
    ##to provide these features
    #the strucutre is openair to duplicate passes to levelplot again
    ############
    #reinstate check.valid for key.position and key$space
    #to stop key.position "blackhole"
    ##e.g. trendLevel(mydata, key.pos ="anything but left, right, top or bottom typed exactly like this!")
    #

    #update notes
    ##############
    #main returned, typelab removed 24 Jan 2011


    ###############################
    #setup
    ###############################

    ## greyscale handling
    if (length(cols) == 1 && cols == "greyscale") {

        trellis.par.set(list(strip.background = list(col = "white")))
    }

    ## reset strip color on exit
    current.strip <- trellis.par.get("strip.background")
    on.exit(trellis.par.set("strip.background", current.strip))

    ##check.valid function
    check.valid <- function(a, x, y){
       if(length(x)>1) x <- x[1] #take first as default
       if(is.null(x)){
          stop(paste("\ttrendLevel does not allow 'NULL' ", a, " option.",
             "\n\t[suggest one of following: ", paste(y, collapse=", "),"]", sep="")
          , call.=FALSE)
       }
       out <- y[pmatch(x, y)] #match to options
       if(is.na(out)){
          stop(paste("\ttrendLevel could not evaluate ", a, " term '", x,
             "'.\n\t[suggest one of following: ", paste(y, collapse=", "),"]", sep="")
          , call.=FALSE)
       }
       #code here for shorten cases x != out
       out
    }

    ##extra.args
    extra.args <- list(...)

    ##label controls
    extra.args$xlab <- if("xlab" %in% names(extra.args))
                           quickText(extra.args$xlab, auto.text) else quickText(x, auto.text)
    extra.args$ylab <- if("ylab" %in% names(extra.args))
                           quickText(extra.args$ylab, auto.text) else quickText(y, auto.text)
    extra.args$main <- if("main" %in% names(extra.args))
                           quickText(extra.args$main, auto.text) else quickText("", auto.text)


    ################################
    #check lengths of x, y, type
    ################################
    #all now handled by cutData2
    #so potential for problem conditioning
    if(length(x)>1){
        warning(paste("\ttrendLevel does not allow multiple 'x' values.",
            "\n\t[ignoring all but first]", sep=""), call.=FALSE)
        x <- x[1]
        xlab <- xlab[1]
    }
    if(length(y)>1){
        warning(paste("\ttrendLevel does not allow multiple 'y' values.",
            "\n\t[ignoring all but first]", sep=""), call.=FALSE)
        y <- y[1]
        ylab <- ylab[1]
    }
    if(length(type)>2){
        ###########################
        #TO DO
        ###########################
        #type = 2
        warning(paste("\ttrendLevel allows up to two 'type' values.",
            "\n\t[ignoring all but first two]", sep=""), call.=FALSE)
        type <- type[1]
        ###################################
        #TO DO
        ###################################
        #type labels
        #check them
    }

    ###################################
    #check x, y and type do not match
    ###################################
    #this would make silly plot
    temp <- unique(c(x, y, type)[duplicated(c(x, y, type))])
    if(length(temp)>0){
        stop(paste("\ttrendLevel could not rationalise plot structure.",
            "\n\t[duplicate term(s) in pollutant, x, y, type structure]",
            "\n\t[term(s): ", paste(temp, collapse=", "),"]", sep="")
        , call.=FALSE)
    }

    #################################
    #number vector handling
    #################################
    #used for rotate.axis and n.levels
    ls.check.fun <- function(vector, vector.name, len){
        if(!is.numeric(vector)){
            warning(paste("\ttrendLevel ignored unrecognised '", vector.name, "' option.",
                "\n\t[check ?trendLevel for details]", sep=""), call.=FALSE)
            #use current default
            vector <- eval(formals(trendLevel)[[vector.name]])
        }
        if(length(vector)<len) vector <- rep(vector,len)[1:len]
        #insert default if not given
        ifelse(is.na(vector), eval(formals(trendLevel)[[vector.name]]), vector)
    }
    rotate.axis <- ls.check.fun(rotate.axis, "rotate.axis", 2)
    n.levels <- ls.check.fun(n.levels, "n.levels", 3)

    #########################
    #statistic handling
    #########################
    if(is.character(statistic) | is.function(statistic)){
        if(is.character(statistic)){
            ##########################
            #hardcoded statistic options
            ##########################
            #update both here and in formals
            statistic <- check.valid("statistic", statistic,
                                    eval(formals(trendLevel)$statistic))
            if(statistic=="mean") {
                stat.fun <- mean
                stat.args <- list(na.rm=TRUE)
            }
            if(statistic=="max") {
                stat.fun <- function(x, ...){
                                if(all(is.na(x))) { NA } else { max(x, ...) }
                            }
                stat.args <- list(na.rm=TRUE)
            }
            if(statistic=="frequency") {
                stat.fun <- function(x, ...){
                                if(all(is.na(x))) { NA } else { length(na.omit(x)) }
                            }
                stat.args <- NULL
            }
            stat.name <- statistic
        ####1
        } else {
            ########################
            #user defined function handling
            ########################
            #default unnameed stats to 'level'
            stat.name <- substitute(statistic)
            if(length(stat.name)!=1) stat.name <- "level"
            if(stat.safe.mode){
                stat.fun <- function(x, ...) {
                    if(all(is.na(x))) { NA } else { statistic(x, ...)[1] }
                }
            } else {
                stat.fun <- function(x, ...) { statistic(x, ...)[1] }
            }
        }
    } else {
        ########################
        #early end for bad stats
        ########################
        #unknown stat option
        stop(paste("\ttrendLevel could not apply statistic option '", substitute(statistic),
           "'.\n\t[suggest valid function or character vector]",
           "\n\t[currect character vectors options: '",
           paste(eval(formals(trendLevel)$statistic), collapse="', '"),"']", sep="")
        , call.=FALSE)
    }

    #############################
    #key.header, footer stat.name recovery
    #############################
    if(!is.null(key.header)) if(is.character(key.header))
        key.header <- gsub("use.stat.name", stat.name, key.header)
    if(!is.null(key.footer)) if(is.character(key.footer))
        key.footer <- gsub("use.stat.name", stat.name, key.footer)

    ############################
    #checkPrep
    ############################
    #keep date if about
    temp <- if("date" %in% names(mydata))
                c("date", pollutant) else
                    pollutant
    #all of x, y, temp need to be handled as type here
    mydata <- checkPrep(mydata, temp, type=c(x,y,type), remove.calm = FALSE)

    ############################
    #cutData
    ############################
    #get pollutant value
    #NOTE: this can be same as one of x, y, type
    #so need a temp case
    mydata$..z.xx <- mydata[,pollutant]
    #different n.levels for axis and type
    #is.axis applied for x and y
    newdata <- cutData(mydata, x, n.levels=n.levels[1], is.axis=TRUE, ...)
    newdata <- cutData(newdata, y, n.levels=n.levels[2], is.axis=TRUE, ...)
    newdata <- cutData(newdata, type, n.levels=n.levels[3], ...)
    newdata <- newdata[c("..z.xx", x,y,type)]


    ############################
    #calculate statistic
    ############################
    #temp function
    temp <- function(...){
                tapply(newdata$..z.xx, newdata[c(x,y,type)],
                    stat.fun, ...)
            }
    if(is.null(stat.args)) {
        newdata <- try(temp(), silent=TRUE)
    } else {
        newdata <- try(do.call(temp, stat.args), silent=TRUE)
    }

    ######################
    #error handling for stat
    ######################
    if(is(newdata)[1]=="try-error"){
        stop(paste("\ttrendLevel could not complete supplied statistic operation '", stat.name,
            "'.\n\t[R error below]", "\n\t", temp[1], sep="")
        , call.=FALSE)
    }

    ##############################
    #restructure new data for plot
    ##############################
    newdata <- data.frame(expand.grid(dimnames(newdata)),
                   matrix(unlist(newdata), byrow = TRUE))
    pollutant <- paste(pollutant, stat.name, sep=".")
    names(newdata)[ncol(newdata)] <- pollutant

    ##############################
    #drop unused type cases
    ##############################
    #previous code removed as part
    #requested simplification
    ##############################
    #TO DO
    ##############################
    #reimplement if requested
    #note:
    #if reinstalled not now have
    #1-2 type settings
    ##############################

    ##############################
    #plot setup
    ##############################
    temp <- paste(type, collapse = "+")
    myform <- formula(paste(pollutant, " ~ ", x, " * ", y, " | ", temp, sep = ""))

    #special case handling
    #layout for wd
    if (length(type) == 1 & type[1] == "wd" & !"layout" %in% names(extra.args)) {
        ## re-order to make sensible layout
        ## starting point code as of ManKendall
        wds <-  c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
        newdata$wd <- ordered(newdata$wd, levels = wds)
        wd.ok <- sapply(wds, function (x) {if (x %in% unique(newdata$wd)) FALSE else TRUE })
        skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
        newdata$wd <- factor(newdata$wd)
        extra.args$layout <- c(3, 3)
        if(!"skip" %in% names(extra.args))
            extra.args$skip <- skip
    }

    temp <- if(is.factor(newdata[ , type[1]]))
                levels(newdata[ , type[1]]) else unique(newdata[ , type[1]])
    temp <- sapply(temp, function(x) quickText(x, auto.text))
    if(is.factor(temp)) temp <- as.character(temp)
    strip <- strip.custom(factor.levels = temp, strip.levels=c(TRUE, FALSE), strip.names=FALSE)
    strip.left <- if(length(type)==1) {
                      FALSE
                  } else {
                      temp <- sapply(unique(newdata[ , type[2]]), function(x) quickText(x, auto.text))
                      if(is.factor(temp)) temp <- as.character(temp)
                      strip.custom(factor.levels = temp)
                  }

    scales <- list(x = list(rot = rotate.axis[1]),
                   y = list(rot = rotate.axis[2]))

    ##ie ignores formals setting because it is looking in ...
    if (missing(limits)) {
        breaks = seq(min(newdata[,pollutant], na.rm = TRUE), quantile(newdata[,pollutant],
            probs = 0.95, na.rm = TRUE), length = 101)
        if(max(breaks, na.rm=TRUE) < max(newdata[,pollutant], na.rm = TRUE)){
            breaks = seq(min(newdata[,pollutant], na.rm = TRUE), quantile(newdata[,pollutant],
                probs = 0.95, na.rm = TRUE), length = 100)
            breaks = c(breaks, max(newdata[,pollutant], na.rm = TRUE))
        }
    } else {
        breaks = seq(limits[1], limits[2], length = 101)
    }
    nlev2 = length(breaks)
    col.regions <- openColours(cols, (nlev2 - 1))

    ##key, colorkey and legend handling
    #default list
    legend <- list(col = col.regions, at = breaks, space = key.position,
                  auto.text = auto.text, footer = key.footer, header = key.header,
                  height = 1, width = 1.5, fit = "all")
    legend <- makeOpenKeyLegend(key, legend, "trendLevel")
    #turn off colorkey
    colorkey <- FALSE

    #stop overlapping labels
    yscale.lp <- function(...){
        ans <- yscale.components.default(...)
        ans$left$labels$check.overlap <- TRUE
        ans
    }
    xscale.lp <- function(...){
        ans <- xscale.components.default(...)
        ans$bottom$labels$check.overlap <- TRUE
        ans
    }

    ##############################
    #plot
    ##############################
    #note: The following listUpdate steps are used to stop this falling
    #      over with a lattice error if the user passes any of
    #      the locally defined options below as part of call.
    #      If they do reset it is obviously Caveat emptor...

    #openair defaults for plot
    levelplot.args <- list(x = myform, data = newdata, as.table = TRUE,
                      legend = legend, colorkey = colorkey,
                      at = breaks, col.regions=col.regions,
                      scales = scales,
                      yscale.components = yscale.lp,
                      xscale.components = xscale.lp,
                      par.strip.text = list(cex = 0.8),
                      strip=strip, strip.left=strip.left)
    #reset for extra.args
    levelplot.args<- listUpdate(levelplot.args, extra.args)
    plt <- do.call(levelplot, levelplot.args)

    ##############################
    #update for two levels
    ##############################
    #uses iseOuterStrips in latticeExtra
    if(length(type) > 1)
        plt <- useOuterStrips(plt, strip = strip, strip.left = strip.left)

    ##############################
    #openair output
    ##############################
    plot(plt)

    output <- list(plot = plt, data = newdata, call = match.call())
    class(output) <- "openair"
    invisible(output)

}

