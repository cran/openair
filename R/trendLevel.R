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
    typelab = NULL, 
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
   #kr v.04
   #based on previous trend.level.hour and trend.level.wd functions by dcc

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
   #check.valid handles terms using pmatch without warning
   #likewise foreshortening of xlim, ylim to dataless ranges enforced within warning
   ##could add an extra message to say: I'm treating this as that...?
   ##see allowed.cond below for preset conditioning ####2
   ##see allowed.poll for date exclusion
   ##the error messaging could be tighter
   #hour and month assume full day and year scales, respectively
   ##this could be standardised with others

   #update notes 
   ##############
   #if you add preset statistic options above
   ## add in associated stat.fun ####1
   #if you add x,y,type options
   ## make sure checkPrep, allow.cond and allow.poll do not conflict ####2, #####3, ####4
   #removed substitute from second check.valid option 
   ##calls that pass through multiple enviornments cannot handle this properly
   ##compare with pems variation
   #improve tick and xlim, ylim control,
   #hopefully now has very robust structure for passing args to levelplot
   ##also allows xlim, ylim, main, etc to be dropped from formals
   ##BUT locally controlled...
   #bug 'fix' for levelplot 'holefilling' and dropped data.
   #bug 'fix' of 0-23/1-24 labelling of hour.
   #added freq as hardcoded option
   #allowed matching poll and one of y, x, type
   #added drop.unused.types options
   #output arg dropped and outputs in style of timeVariation
 
   #to do
   ###############
   #current versions drops NAs
   ## plot.nas option being tested
   #statistic as function can produce nasty R errors
   ## preliminary error tester in development

   ####################
   #setup
   ####################

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

   ##pollutant handling
   allowed.poll <- names(mydata)[names(mydata) != "date"]
   pollutant <- check.valid("pollutant", pollutant, allowed.poll)

   #########################
   #x, y, type handling
   #########################
   ####2

   ###############################
   #think this through again sometime
   ##############################

   #######################################
   #unique to prevent poll/cond overlap wd
   #but allow poll = "wd" 
   ########################################
   allowed.cond <- c()
   if("date" %in% names(mydata)) {
      allowed.cond <- c(allowed.cond, c("hour", "month", "year"))
   }
   if("wd" %in% names(mydata)) {
      allowed.cond <- c(allowed.cond, "wd")
   }   
   #check x,y,temp are available
   temp <- unique(c(allowed.cond, allowed.poll))
   x <- check.valid("x", x, temp)
   y <- check.valid("y", y, temp)
   type <- check.valid("type", type, temp)

   ###################################
   #check x, y and type do not match
   #would make silly plot
   ###################################
   ####3
   temp <- unique(c(x, y, type)[duplicated(c(x, y, type))])
   if(length(temp)>0){
      stop(paste("\ttrendLevel could not rationalise plot structure.",
         "\n\t[duplicate term(s) in pollutant, x, y, type structure]",
         "\n\t[term(s): ", paste(temp, collapse=", "),"]", sep="")
      , call.=FALSE)
   }

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
         #function handling 
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

   ##################
   #checkPrep
   ##################
   ####4
   vars <- c()
   if(any(c("hour", "month", "year") %in% c(x, y, type))){
      vars <- c(vars, "date")
   }
   vars <- unique(c(vars, names(mydata)[names(mydata) %in% c(pollutant, x, y, type)]))
   mydata <- checkPrep(mydata, vars, "default")
   mydata <- mydata[, vars] #why?

   ################
   #get x, y, type axis objects to summarise with.
   ################
   make.axis <- function(x){
       rescale <- function(x, levels) 
           as.vector(sapply(x, function(x) {
                x <- which(tolower(levels) == tolower(x))[1] 
                if(is.na(x)) x <- 1 else x } ))
 
       if(x %in% allowed.cond){
           if(x=="year") {
               #years 
               #Note 0001 format string
               a <- as.numeric(format(mydata$date, "%Y"))
               def.lims <- c(min(a, na.rm=TRUE), max(a, na.rm=TRUE))
               labels <- def.lims[1]:def.lims[2]
               reformat <- function(x, levels){
               if(is.numeric(x)) {
                   x <- x - as.numeric(levels[1]) + 1 
                   x <- ifelse(x<1, 1, x)
                   x <- ifelse(x>length(levels), length(levels), x)         
                   levels[x]  
               } else {
                   x <- as.character(x)
                   x <- ifelse(nchar(x) < 4, paste(paste(rep("0", (4-nchar(x))), collapse=""), x, sep=""), x)
               }
           }
           labels <- reformat(as.character(labels), labels)
           a <- factor(a, levels = labels)
#           rescale <- function(x, levels) 
#                as.vector(sapply(x, function(x) {
#                              x <- which(tolower(levels) == tolower(x))[1] 
#                              if(is.na(x)) x <- 1 else x } ))
        }
        if(x=="month") {
            a <- as.numeric(format(mydata$date, "%m"))
            def.lims <- c(min(a, na.rm=TRUE), max(a, na.rm=TRUE))
            labels <- make.month.abbs()
            a <- labels[a]
            reformat <- function(x, levels){
                if(is.numeric(x)) {
                    x <- ifelse(x<1, 1, x)
                    x <- ifelse(x>length(levels), length(levels), x)         
                    levels[x]  
                } else {
                    x <- as.character(x)
                }
            }
            a <- factor(a, labels)
#           rescale <- function(x, levels) 
#                as.vector(sapply(x, function(x) {
#                              x <- which(tolower(levels) == tolower(x))[1] 
#                              if(is.na(x)) x <- 1 else x } ))
        }
        if(x=="hour") {
            #remember 0-23 not 1-24
            a <- format(mydata$date, "%H")
            labels <- as.numeric(as.character(a))
            def.lims <- c(min(labels, na.rm=TRUE), max(labels, na.rm=TRUE))  
            labels <- as.character(0:23)
            reformat <- function(x, levels){
                if(is.numeric(x)) {
                    x <- x + 1 
                    x <- ifelse(x<1, 1, x)
                    x <- ifelse(x>length(levels), length(levels), x)         
                    levels[x]  
                } else {
                    x <- as.character(x)
                    x <- ifelse(nchar(x)==1, paste("0", x, sep=""), x)
                }
            }
            labels <- reformat(labels, labels)
            a <- factor(reformat(as.character(a)), labels)
#            rescale <- function(x, levels) 
#                 as.vector(sapply(x, function(x) {
#                               x <- which(tolower(levels) == tolower(x))[1] 
#                               if(is.na(x)) x <- 1 else x } ))
        }
        if(x=="wd") {
            a <- cut(mydata$wd, seq(0, 360, 10), 
                     ordered_result = TRUE, include.lowest = TRUE, exclude=NULL)
            labels <- levels(a)
            labels <- gsub("[(]|[)]|[[]|[]]", "", labels)
            labels <- gsub("[,]", "-", labels)
            levels(a) <- labels
            reformat <- function(x, levels) x <- if(is.numeric(x)) {
                             x <- floor((x/10)+0.5)
                             x <- ifelse(x<1, 1, x)
                             x <- ifelse(x>length(levels), length(levels), x)
                             x <- levels[x] 
                        } else as.character(x)
#           rescale <- function(x, levels) 
#                 as.vector(sapply(x, function(x) {
#                               x <- which(tolower(levels) == tolower(x))[1] 
#                               if(is.na(x)) x <- 1 else x } ))
            def.lims <- range(mydata$wd, na.rm=TRUE, finite=TRUE)
        }
    } else {
        #character vectors converted to factors
        #both factors and characters as factors assume external ordering
        if(is.factor(mydata[, x]) | is.character(mydata[, x])) {
            a <- mydata[, x]
            if(is.character(mydata[,x])) a <- as.factor(mydata[, x])
            labels <- levels(a)
            def.lims <- c(1, length(labels))
            reformat <- function(x, levels) x <- if(is.numeric(x)) {
                             x <- ifelse(x<1, 1, x)
                             x <- ifelse(x>length(levels), length(levels), x)
                             x <- levels[x] 
                        } else as.character(x)
#            rescale <- function(x, levels) 
#                 as.vector(sapply(x, function(x) {
#                               x <- which(tolower(levels) == tolower(x))[1] 
#                               if(is.na(x)) x <- 1 else x } ))
        } else {
         #numeric data
         #would this work for some of others?
         #check out when more time  
            a <- mydata[, x]
            def.lims <- c(min(a, na.rm=TRUE), max(a, na.rm=TRUE))  
            a <- (cut(mydata[, x], ordered_result = TRUE, 
                       unique(quantile(mydata[, x], probs = seq(0, 1, length = 10), na.rm = TRUE)), 
                       include.lowest = TRUE))
            labels <- levels(a)
            a <- factor(a, levels=labels)
            labels <- gsub("[(]|[)]|[[]|[]]", "", labels)
            labels <- gsub("[,]", "-", labels)
            levels(a) <- labels
            reformat <- function(x, levels) x <- if(is.numeric(x)) {
                            #get level ranges
                            temp <- as.numeric(unique(unlist(strsplit(levels, "-"))))
                            #in theory nothing outside range
                            #but....
                            temp[1] <- temp[1]- temp[length(temp)]
                            temp[length(temp)] <- temp[length(temp)] + temp[length(temp)]
                            x <- as.numeric(cut(x, breaks = temp))
                            x <- levels[x] 
                        } else as.character(x)
#            rescale <- function(x, levels) 
#                 as.vector(sapply(x, function(x) {
#                               x <- which(tolower(levels) == tolower(x))[1] 
#                               if(is.na(x)) x <- 1 else x } ))     
        }
    }
    return(list(a=a, labels=labels, reformat=reformat, rescale=rescale, def.lims=def.lims))
   }

   y.f <- make.axis(y)
   x.f <- make.axis(x)
   type.f <- make.axis(type)

   ##########################
   #generate new data to plot
   ##########################

   newdata <- data.frame(x = x.f$a, y = y.f$a, type = type.f$a, 
                      value = mydata[, pollutant])

   #temp function
   temp <- function(...){
                   tapply(newdata$value, newdata[c("x","y","type")],
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
   names(newdata)[4] <- "value" 

   ####################
   #set up plot
   ####################
   #bit long-wind 
   #BUT gives fine contol of (...)
   ##allows me to reset ... info
   ##also no fall-over if users resets levelplot arg used in plot itself

   lp.args <- list(...)
   
   #forced terms
   #x, data, xlab, ylab
   lp.args$x <- value ~ x * y | type
   if(drop.unused.types) {
       lp.args$data <- subset(newdata, type %in% unique(subset(newdata, !is.na(value))$type))
       lp.args$data$type <- factor(as.character(lp.args$data$type), 
                                   levels=unique(as.character(lp.args$data$type)),
                                   ordered=TRUE)
   } else lp.args$data <- newdata

   lp.args$xlab <- quickText(xlab, auto.text) 
   lp.args$ylab <- quickText(ylab, auto.text)
   
   #general terms
   #ylim, xlim, main, as.table,
   ############################
   #ylim, xlim could be improved
   #if they work in plot
   #update would handle them
   
   if(is.null(lp.args$ylim)) 
       lp.args$ylim <- y.f$def.lims 
   lp.args$ylim <- y.f$rescale(y.f$reformat(lp.args$ylim, y.f$labels), y.f$labels)

   if(is.list(lp.args$ylim)) {
       lp.args$ylim <- y.f$def.lims
       warning("In trendLevel(...):\n  ylim not fully recognised, so reset to data range\n",
               "  [check ylim settings]", 
            call. = FALSE)
   }
   if(length(lp.args$ylim>1)){
       lp.args$ylim <- range(lp.args$ylim, na.rm=TRUE, finite=TRUE)
       lp.args$ylim[1:2] <- lp.args$ylim[1:2] + c(-0.6, +0.6)
   } 

   if(is.null(lp.args$xlim)) 
       lp.args$xlim <- x.f$def.lims 
   lp.args$xlim <- x.f$rescale(x.f$reformat(lp.args$xlim, x.f$labels), x.f$labels)
   if(is.list(lp.args$xlim)) {
       lp.args$xlim <- x.f$def.lims
       warning("In trendLevel(...):\n  xlim not fully recognised, so reset to data range\n",
               "  [check xlim settings]", 
            call. = FALSE)
   }
   if(length(lp.args$xlim>1)){
       lp.args$xlim <- range(lp.args$xlim, na.rm=TRUE, finite=TRUE)
       lp.args$xlim[1:2] <- lp.args$xlim[1:2] + c(-0.6, +0.6)
   } 
   if(!is.null(lp.args$main)) lp.args$main <- quickText(lp.args$main, auto.text)
   if(is.null(lp.args$as.table)) lp.args$as.table <- TRUE

   #axis handling
   # via y and x scale.components
   yscale.lp <- function(...){
       #no.ticks
       #not handled by factors in normal lattice
       temp <- if(!is.null(lp.args$scales$tic)) lp.args$scales$tic else 
                   if(is.null(lp.args$scales$y$tic)) 5 else lp.args$scales$y$tic
       ans <- yscale.components.default(...)
       temp <- pretty(ans$num.limit, temp) 
       temp <- unique(as.integer(temp))
       temp <- temp[temp > 0]
       ans$left$ticks$at <- temp 
       ans$left$labels$at <- temp
       ans$left$labels$labels <- y.f$labels[temp]
       ans$left$labels$check.overlap <- TRUE 
       ans
   }
   xscale.lp <- function(...){
       #no.ticks
       #not handled by factors in normal lattice
       temp <- if(!is.null(lp.args$scales$tic)) lp.args$scales$tic else 
                   if(is.null(lp.args$scales$x$tic)) 5 else lp.args$scales$x$tic
       ans <- xscale.components.default(...)
       temp <- pretty(ans$num.limit, temp) 
       temp <- unique(as.integer(temp))
       temp <- temp[temp > 0]
       ans$bottom$ticks$at <- temp 
       ans$bottom$labels$at <- temp
       ans$bottom$labels$labels <- paste(" ", x.f$labels[temp], " ", sep="")
       ans$bottom$labels$check.overlap <- TRUE
       ans
   }
   if(is.null(lp.args$yscale.components)) lp.args$yscale.components <- yscale.lp
   if(is.null(lp.args$xscale.components)) lp.args$xscale.components <- xscale.lp

   #strip
   #################
   #handles user strip modifications
   #alignment of quicktext and conditioning label
   #also systematic for factors and shingles
   #################
   if(is.null(lp.args$strip)){
       lp.args$strip <- function(..., var.name, strip.levels, strip.names, sep) 
                       strip.default(..., var.name = quickText(paste(typelab, levels(lp.args$data$type)[which.packet()], sep =" "), auto.text), 
                       strip.levels = c(FALSE, FALSE), strip.names=c(TRUE,TRUE), sep="")
   }
  
   #col.scale, col, etc.
   ######################
   #this next bit may STILL need fixing
   ######################
   #does not work expected
   ##ie ignores formals setting because it is looking in ...
   if (missing(limits)) {
       breaks = seq(min(newdata$value, na.rm = TRUE), quantile(newdata$value, 
           probs = 0.95, na.rm = TRUE), length = 101)
       if(max(breaks, na.rm=TRUE) < max(newdata$value, na.rm = TRUE)){
           breaks = seq(min(newdata$value, na.rm = TRUE), quantile(newdata$value, 
               probs = 0.95, na.rm = TRUE), length = 100)
           breaks = c(breaks, max(newdata$value, na.rm = TRUE))
       }
   } else {
       breaks = seq(limits[1], limits[2], length = 101)
   }
   nlev2 = length(breaks)
   if(is.null(lp.args$at)) lp.args$at <- breaks 
   if(is.null(lp.args$col.regions)) lp.args$col.regions <- openColours(cols, (nlev2 - 1))

   #scale key setup
   #colorkey reset
   #################
   legend <- list(col = lp.args$col.regions, at = lp.args$at, space = key.position, 
        auto.text = auto.text, footer = key.footer, header = key.header, 
        height = 1, width = 1.5, fit = "all")
   if (!is.null(key)) 
        if (is.list(key)) 
            legend[names(key)] <- key
        else warning("In trendLevel(...):\n  non-list key not exported/applied\n  [see ?drawOpenKey for key structure/options]", 
            call. = FALSE)
   legend <- list(temp = list(fun = drawOpenKey, args = list(key = legend, 
        draw = FALSE)))
   names(legend)[1] <- key.position
   lp.args$legend <- legend
   if(is.null(lp.args$colorkey)) lp.args$colorkey <- FALSE

   ####################
   #plot call
   ####################
   plt <- do.call(levelplot, lp.args)

   ####################
   #output
   ####################   
   plot(plt)
   newdata <- newdata[,c("x", "y", "type", "value")]
   names(newdata) <- c(x, y, type, paste(pollutant, stat.name, sep="."))
   invisible(list(data=newdata, plot = plt))

}

