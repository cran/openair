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
    main = "",
    strip = NULL, 
    auto.text = TRUE,
    key.header = "use.stat.name",
    key.footer = pollutant,
    key.position = "right",
    key = NULL,
    statistic = c("mean", "max"),
    stat.args = NULL,
    stat.safe.mode = TRUE,
    ##plot.nas = FALSE,
    output = c("data", "graph"),
    ...) 
{

   #Generic levelplot function for summarising large data sets
   #kr v.03
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
   ##could add an extra message to say: I'm treating this as that...?
   ##see allowed.cond below for preset conditioning
   ####2
   ##see allowed.poll for date exclusion
   ##the error messaging could be tighter
   #hour and month assume full day and year scales, respectively
   ##this could be standardised with others
   #ticks and bins are hardcoded and sometimes messy
   ##could have local control (through scales?) 

   #update notes 
   ##############
   #if you add preset statistic options above
   ## add in associated stat.fun ####1
   #if you add x,y,type options
   ## make sure checkPrep, allow.cond and allow.poll do not conflict ####4
   #removed substitute from second check.valid option 
   ##multi enviornment calls cannot handle this properly
   ## compare with pems variation

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

   #output handling
   output <- check.valid("output", output, eval(formals(trendLevel)$output))

   ##pollutant handling
   allowed.poll <- names(mydata)[names(mydata) != "date"]
   pollutant <- check.valid("pollutant", pollutant, allowed.poll)

   ##x, y, type handling
   ####2
   #unique to prevent poll/cond overlap wd
   #but allow poll = "wd" 

   allowed.cond <- c()
   if("date" %in% names(mydata)) {
      allowed.cond <- c(allowed.cond, c("hour", "month", "year"))
   }
   if("wd" %in% names(mydata)) {
      allowed.cond <- c(allowed.cond, "wd")
   }   

   #############################
   #check x,y,temp are available
   #############################
   temp <- unique(c(allowed.cond, allowed.poll))
   x <- check.valid("x", x, temp)
   y <- check.valid("y", y, temp)
   type <- check.valid("type", type, temp)

   #############################
   #check pollutant, x, y and type do not match
   ##including pollutant in case we allow pollutants as conditioning options later
   #############################
   ####3
   temp <- unique(c(pollutant, x, y, type)[duplicated(c(pollutant, x, y, type))])
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
         #character settings
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
         stat.name <- statistic
      ####1
      } else {
         #functions
         ##robustly handle functions that return more than 1 value
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
      stop(paste("\ttrendLevel could not apply statistic option '", substitute(statistic),
         "'.\n\t[suggest valid function or character vector]", 
         "\n\t[currect character vectors options: '", 
         paste(eval(formals(trendLevel)$statistic), collapse="', '"),"']", sep="")
      , call.=FALSE)
   }
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
   if(x %in% allowed.cond){
      if(x=="year") {
         a <- as.factor(format(mydata$date, "%Y"))
         b <- levels(a)
         a <- as.numeric(as.factor(a))
         labels.at <- pretty(a, 4)
         labels.at <- subset(labels.at, labels.at > 1 & labels.at < length(b))
         labels <- b
      }
      if(x=="month") {
         a <- format(mydata$date, "%m")
         labels.at <- c(1, 4, 7, 10)
         labels <- make.month.abbs()
         b <- make.month.abbs()[as.numeric(levels(as.factor(as.numeric(a))))]
      }
      if(x=="hour") {
         a <- format(mydata$date, "%H")
         labels.at <- pretty(a, 4)
         b <- levels(as.factor(as.numeric(a)))
         labels.at <- subset(labels.at, labels.at > 1 & labels.at < length(b))
         labels <- b  
       }
      if(x=="wd") {
         a <- cut(mydata$wd, seq(0, 360, 10), ordered_result = TRUE, include.lowest = TRUE, exclude=NULL)
         b <- levels(a)
         a <- as.numeric(a)
         labels.at <- c(9,18,27)
         labels <- gsub("[(]|[)]|[[]|[]]", "", b)
         labels <- gsub("[,]", "-", labels)
      }
   } else {
      if(is.factor(mydata[, x]) | is.character(mydata[, x])) {
         a <- as.factor(mydata[, x])
         b <- levels(a)
         #b <- as.numeric(a)
         a <- as.factor(as.numeric(a))
         labels.at <- pretty(a, 4)
         labels.at <- subset(labels.at, labels.at > 1 & labels.at < length(b))
         labels <- b
      } else {
         a <- cut(mydata[, x], ordered_result = TRUE, 
                    unique(quantile(mydata[, x], probs = seq(0, 1, length = 10), na.rm = TRUE)), 
                    include.lowest = TRUE, labels = FALSE)
         a <- as.factor(a)
         b <- levels(cut(mydata[, x], ordered_result = TRUE, 
                    unique(quantile(mydata[, x], probs = seq(0, 1, length = 10), na.rm = TRUE)), 
                    include.lowest = TRUE))
         labels.at <- pretty(a, 4)
         labels.at <- subset(labels.at, labels.at > 1 & labels.at < length(b))
         labels <- gsub("[(]|[)]|[[]|[]]", "", b)
         labels <- gsub("[,]", "-", labels)
      }
   }
      return(list(a=a, labels=labels, labels.at = labels.at, b = b))
   }

   y.f <- make.axis(y)
   x.f <- make.axis(x)
   type.f <- make.axis(type)

   ################################
   #restructure/aggregate data
   ################################
   ##use do.call/function to handle stat.args
   ##using ... may cause conflicts 
   ##So, here that is reserved for graphic terms to lattice 
   temp.fun <- function(...){
                  aggregate(mydata[, pollutant], 
                  list(paste(make.axis(type)$a, x.f$a, sep="-"), y.f$a), 
                  stat.fun, ...)
               }
   if(is.null(stat.args)) {
       newdata <- try(temp.fun(), silent=TRUE)
   } else { 
       newdata <- try(do.call(temp.fun, stat.args), silent=TRUE)
   }

   ######################
   #error handling for stat
   ######################
   if(is(newdata)[1]=="try-error"){      
      stop(paste("\ttrendLevel could not complete supplied statistic operation '", stat.name,
         "'.\n\t[R error below]", "\n\t", newdata[1], sep="")
      , call.=FALSE)
   }

   names(newdata) <- c("type", "y", "value")
   temp <- unlist(strsplit(newdata$type,"-"))
   newdata$x <- temp[seq(2, length(temp), by = 2)]
   newdata$type <- temp[seq(1, length(temp), by = 2)]

   ##########################
   #na handling
   ##########################
   #to do
   #currently excludes all x, y, type na cases
   #would like a handling option
   #######
   #current not working properly OK for all but y axis
   #y axis problem with aggregate
   #######
   #if(plot.nas==TRUE){
   #   if(any(newdata$type=="NA")){
   #      newdata$type <- ifelse(newdata$type=="NA", as.character(length(type.f$b)+1), newdata$type)
   #      type.f$labels.at <- c(type.f$labels.at, length(type.f$b)+1)
   #      type.f$b <- c(type.f$b, "NA")
   #      type.f$labels <- c(type.f$labels, "NA")
   #   }
   #   if(any(newdata$x=="NA")){
   #      newdata$x <- ifelse(newdata$x=="NA", as.character(length(x.f$b)+1), newdata$x)
   #      x.f$labels.at <- c(x.f$labels.at, length(x.f$b)+1)
   #      x.f$b <- c(x.f$b, "NA")
   #      x.f$labels <- c(x.f$labels, "NA")
   #   }
   #   if(any(is.na(newdata$y))){
   #      newdata$y <- ifelse(is.na(newdata$y), length(y.f$b)+1, newdata$y)
   #      y.f$labels.at <- c(y.f$labels.at, length(y.f$b)+1)
   #      y.f$b <- c(y.f$b, "NA")
   #      y.f$labels <- c(y.f$labels, "NA")
   #   }   
   #} else {
   newdata <- subset(newdata, x!="NA" & y!="NA" & type!="NA")
   #}

   ##################
   #setup new data for plot
   ##################
   newdata$x <- as.numeric(newdata$x)
   newdata$y <- as.numeric(newdata$y)
   newdata$type <- as.factor(as.numeric(newdata$type))
   levels(newdata$type) <- type.f$labels[1:length(levels(newdata$type))]
 
   ######################
   #this next bit may need fixing
   ######################
   #does work as code would suggest
   ##ie ignores formals setting because it is looking in ...
   if (missing(limits)) {
       breaks = seq(min(newdata$value, na.rm = TRUE), quantile(newdata$value, 
           probs = 0.95, na.rm = TRUE), length = 100)
   } else {
       breaks = seq(limits[1], limits[2], length = 100)
   }
   breaks = c(breaks, max(newdata$value, na.rm = TRUE))
   nlev2 = length(breaks)
   col.scale = breaks
   col <- openColours(cols, (nlev2 - 1))
   ##########################

   #################
   #handles user strip modifications
   #alignment of quicktext and conditioning label
   #also systematic for factors and shingles
   #################
   if(is.null(strip)){
       strip <- function(..., var.name, strip.levels, strip.names, sep) 
                       strip.default(..., var.name = quickText(paste(typelab, type.f$b[which.packet()], sep =" "), auto.text), 
                       strip.levels = c(FALSE, FALSE), strip.names=c(TRUE,TRUE), sep="")
   }


   #################
   #scale key setup
   #################
   legend <- list(col = col, at = col.scale, space = key.position, 
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

   #################
   #plot levelplot
   #################
   plt <- levelplot(value ~ x * y | type, data = newdata, main = quickText(main, 
                    auto.text), as.table = TRUE, col.regions = col, at = col.scale,
                    xlab = quickText(xlab, auto.text), ylab = quickText(ylab, auto.text),
                    strip = strip,
                    scales = list(
                       x = list(labels = x.f$labels[x.f$labels.at], at = x.f$labels.at),
                       y = list(labels = y.f$labels[y.f$labels.at], at = y.f$labels.at))
                    , colorkey = FALSE, legend=legend, ...
   )

   if(output=="data") {
      plot(plt)

      #################
      #tidy data for export
      #################
      ##could use $levels not $b
      ##for tidier but non-R format bins
      newdata$x <- as.factor(newdata$x)
      newdata$y <- as.factor(newdata$y)
      levels(newdata$x) <- x.f$b
      levels(newdata$y) <- y.f$b
      levels(newdata$type) <- type.f$b
      newdata <- newdata[,c("x", "y", "type", "value")]
      names(newdata) <- c(x, y, type, paste(pollutant, stat.name, sep="."))
      invisible(newdata)
   } else {
      plot(plt)
   }
}

