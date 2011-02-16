trendLevelHour <- function(mydata, ...) {
   trendLevel(mydata, ...)
}

trendLevelWd <- function(mydata,
   pollutant = "nox", 
   y = "wd", ylab = "wind direction (degrees)", ...) {
   trendLevel(mydata, y = y, ylab = ylab, ...)
}

trendLevel <- function(mydata, 
    pollutant = "nox",
    x = "month", y = "hour",
    type = "year",
    xlab = x, ylab = y,
    main = "",
    rotate.axis = c(90, 0),
    n.levels = c(10, 10, 4), 
    limits = c(0, 100),  
    cols = "default", 
    auto.text = TRUE,
    key.header = "use.stat.name",
    key.footer = pollutant,
    key.position = "right",
    key = NULL,
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
    mydata <- checkPrep(mydata, temp, type=c(x,y,type))

    ############################
    #cutData2
    ############################
    #get pollutant value
    #NOTE: this can same as one of x, y, type
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

    temp <- sapply(unique(newdata[ , type[1]]), function(x) quickText(x, auto.text))
    strip <- strip.custom(factor.levels = temp, strip.levels=c(TRUE, FALSE), strip.names=FALSE)
    strip.left <- if(length(type)==1) {
                      FALSE
                  } else {
                      temp <- sapply(unique(newdata[ , type[2]]), function(x) quickText(x, auto.text))
                      strip.custom(factor.levels = temp)
                  }
    xlab <- quickText(xlab, auto.text) 
    ylab <- quickText(ylab, auto.text)
    main <- quickText(main, auto.text)
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

    legend <- list(col = col.regions, at = breaks, space = key.position, 
         auto.text = auto.text, footer = key.footer, header = key.header, 
         height = 1, width = 1.5, fit = "all")
    if (!is.null(key)) 
         if (is.list(key)) 
             legend[names(key)] <- key
         else warning("In trendLevel(...):\n  non-list key not exported/applied\n  [see ?drawOpenKey for key structure/options]", 
             call. = FALSE)
    legend <- list(temp = list(fun = drawOpenKey, args = list(key = legend, 
         draw = FALSE)))
    names(legend)[1] <- legend$temp$args$key$space #safer than key.position
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
    #note: (like other openair functions)
    #      this will now fall over with lattice error if the user passes any of 
    #      the preset options below as part of call
    plt <- levelplot(myform, data = newdata,
               as.table = TRUE, xlab=xlab, ylab =ylab, main = main,
               legend = legend, colorkey = colorkey, 
               at = breaks, col.regions=col.regions,
               scales = scales,
               yscale.components = yscale.lp,
               xscale.components = xscale.lp,
               par.strip.text = list(cex = 0.8),
               strip=strip, strip.left=strip.left,
               ...)

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

