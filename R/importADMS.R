importADMS <- function(file=file.choose(), file.type="unknown", 
                       drop.case = TRUE, drop.input.dates = TRUE, keep.units = TRUE, 
                       simplify.names = TRUE, test.file.structure = TRUE, 
                       drop.delim = TRUE, add.prefixes = TRUE,
                       names = NULL,
                       ...)
{

#importADMS 
#v0.2 kr
#parent with four daughters (below)
#Bgd, Mop and Met import methods and simplifyNamesADMS 

if(substitute(file)=="simplify.names") { return(simplifyNamesADMS(names)) }

if(file.type=="unknown"){
  file.type <- tolower(substr(file, nchar(file)-3, nchar(file)))
  if(substr(file.type,1,1)=="."){
    file.type <- substr(file.type,2,4)
  } else {
    stop("File extension not recognised\n       [If valid ADMS file, try setting file.type to one of: bgd, mop or met]", 
                call. = FALSE) 
  }
}

if(file.type=="bgd") { 
  return(importADMSBgd(
    file=file, drop.case = drop.case, drop.input.dates = drop.input.dates, 
    keep.units = keep.units, simplify.names = simplify.names, 
    test.file.structure = test.file.structure,
    drop.delim = drop.delim, add.prefixes = add.prefixes,
    ...
  )) 
}
if(file.type=="mop") { 
  return(importADMSMop(
    file=file, drop.case = drop.case, drop.input.dates = drop.input.dates, 
    keep.units = keep.units, simplify.names = simplify.names, 
    test.file.structure = test.file.structure,
    drop.delim = drop.delim, add.prefixes = add.prefixes,
    ...
  )) 
}
if(file.type=="met") { 
  return(importADMSMet(
    file=file, drop.case = drop.case, drop.input.dates = drop.input.dates, 
    keep.units = keep.units, simplify.names = simplify.names, 
    test.file.structure = test.file.structure,
    drop.delim = drop.delim, add.prefixes = add.prefixes,
    ...
  )) 
}
if(file.type=="pst") { 
  return(importADMSPst(
    file=file, drop.case = drop.case, drop.input.dates = drop.input.dates, 
    keep.units = keep.units, simplify.names = simplify.names, 
    test.file.structure = test.file.structure,
    drop.delim = drop.delim, add.prefixes = add.prefixes,
    ...
  )) 
}

stop("File extension not recognised\n       [If valid ADMS file, try setting file.type to one of: bgd, mop, met or pst]", 
  call. = FALSE) 
}

###############
##daughter
##importADMSBgd

importADMSBgd <- function(file=file.choose()
    , drop.case=TRUE, drop.input.dates=TRUE
    , keep.units=TRUE, simplify.names=TRUE
    , test.file.structure=TRUE
    , drop.delim = TRUE, add.prefixes = TRUE    
    , ...
){
    bgd <- readLines(file, n = -1)
    bgd <- sub('[[:space:]]+$', '', bgd) #strip out tail spaces
    
    loc.start <- which(tolower(bgd) == "backgroundversion2")
    if(test.file.structure){
        if(length(loc.start)==0){
            stop("File not recognised ADMS.bgd structure\n       [please contact openair if valid]", 
                call. = FALSE)
        }
    }
    if(length(loc.start) > 1){
        warning("Multiple possible variable starts, taking last\n       [please contact openair problems encountered]", 
            call. = FALSE)
        loc.start <- loc.start[length(loc.start)]
    }
    no.var <- suppressWarnings(as.numeric(bgd[loc.start + 1]))[1]
    if(test.file.structure & is.na(no.var)) {
        stop("File not recognised ADMS.bgd structure\n       [please contact openair if valid]", 
            call. = FALSE)
    }
    variables <- bgd[(loc.start + 2) : (loc.start + 1 + no.var)]

    if(simplify.names) {variables <- simplifyNamesADMS(variables)}

    #drop messy name handling
    variables <- gsub("[.][.]", ".", variables)
    variables <- gsub("^[.]", "", variables)

    if(drop.case) { variables <- tolower(variables) }

    units.start <- which(substr(bgd, 1, 6) == "UNITS:")
    if(length(units.start)==0){
        warning("Data units not extracted from ADMS.bgd\n       [please contact file structure if problems encountered]", 
            call. = FALSE)
        units <- "units: undefined"
    } 
    if(length(units.start) > 1){
        warning("Multiple possible unit starts, taking last\n       [please contact openair problems encountered]", 
            call. = FALSE)
        units.start <- units.start[length(units.start)]
    }
    units <- bgd[(units.start + 1) : (units.start + no.var)]
    if(length(units)==0){
        units <- "units: undefined"
    } else {
        units <- paste("units: ",paste(units, sep = "", collapse = ", "), sep="")
    }
    data.start <- which(substr(bgd, 1, 5) == "DATA:")
    if(length(data.start)==0){
        stop("Data start not not located ADMS.bgd\n       [please contact file structure if problems encountered]", 
            call. = FALSE)
    }
    if(length(data.start) > 1){
        warning("Multiple possible data starts, taking last\n       [please contact openair problems encountered]", 
            call. = FALSE)
        data.start <- data.start[length(data.start)]
    }

    ans <- read.csv(file, header = FALSE, skip = data.start 
        , na.strings = c("", "NA", "-999", "-999.0") 
        , ...)
    ans[] <- lapply(ans, function(x) { replace(x, x == -999, NA) })
    ########################
    #screening for missing data
    #confirm formats, if they get any with bgd files, etc.
    #might not be necessary

    date<- paste(ans[,1], ans[,2], ans[,3], sep = "-")
    date <- as.POSIXct(strptime(date, format = "%Y-%j-%H"), "GMT")
    ans <- cbind(date = date, ans)
    if(length(variables) != ncol(ans) - 4){
        warning("Variable data mismatch, taking shortest\n       [please contact if openair problems encountered]", 
            call. = FALSE)
        variables <- variables[1: min(c(length(variables), ncol(ans) - 4), na.rm = TRUE)]
        ans <- ans[, 1:(length(variables) + 4)]
    }

    names(ans) <- c("date", "bgd.year", "bgd.day", "bgd.hour", variables)
    if(drop.input.dates==TRUE){
        ans <- ans[, c(1, 5:ncol(ans))]
    } 
    if(keep.units) { 
        comment(ans) <- c(comment(ans), units)
    }

    #error handling for bad days
    ids <- which(is.na(ans$date))
    if (length(ids) > 0) {
        if(length(ids)==nrow(ans)) {
            stop("Invalid date (and time) format requested\n       [compare openair import settings and data structure]"
                , call. = FALSE)
        }
        ans <- ans[-ids, ]
        reply <- paste("Missing dates detected, removing", length(ids), "line", sep=" ")
        if(length(ids) > 1) { reply <- paste(reply, "s", sep = "") }
        warning(reply, call. = FALSE)
    }
    print(unlist(sapply(ans, class)))
    ans
}

###############
##daughter
##importADMSMet

importADMSMet <- function (file = file.choose() 
    , drop.case=TRUE, drop.input.dates=TRUE
    , keep.units=TRUE, simplify.names=TRUE
    , test.file.structure=TRUE
    , drop.delim = TRUE, add.prefixes = TRUE    
    , ...
){
    met <- readLines(file, n = -1)
    met <- sub('[[:space:]]+$', '', met) #strip out tail spaces
    loc.start <- which(met == "VARIABLES:")
    if(test.file.structure){
        if(length(loc.start)==0){
            stop("File not recognised ADMS.met structure\n       [please contact openair if valid]", 
                call. = FALSE)
        }
    }
    if(length(loc.start) > 1){
        warning("Multiple possible variable starts, taking last\n       [please contact openair problems encountered]", 
            call. = FALSE)
        loc.start <- loc.start[length(loc.start)]
    }
    variables <- suppressWarnings(as.numeric(met[loc.start + 1]))[1]
    if(test.file.structure & is.na(variables)) {
        stop("File not recognised ADMS.met structure\n       [please contact openair if valid]", 
            call. = FALSE)
    }
    variables <- met[(loc.start + 2) : (loc.start + 1 + variables)]

    data.start <- which(met == "DATA:")
    if(test.file.structure){
        if(length(data.start)==0){
            stop("File not recognised ADMS.met structure\n       [please contact openair if valid]", 
                call. = FALSE)
        }
    }
    if(length(data.start) > 1){
        warning("Multiple possible data starts, taking last\n       [please contact openair if problems encountered]", 
            call. = FALSE)
        data.start <- data.start[length(data.start)]
    }

    met <- read.csv(file, skip = data.start, header = FALSE, na.strings = c("-999", 
        "-999.0"))
    met[] <- lapply(met, function(x) {
        replace(x, x == -999, NA)
    })
    met <- met[, sapply(met, function(x) !all(is.na(x)))]
    if(length(variables) != ncol(met)){
        warning("Variable data mismatch, taking shortest\n       [please contact if openair problems encountered]", 
            call. = FALSE)
        variables <- variables[1: min(c(length(variables), ncol(met)), na.rm = TRUE)]
        met <- met[, 1:length(variables)]
    }
    names(met) <- make.names(variables, unique = TRUE)

    #multiple year day hour name options
    fun.temp <- function(x, y, z){
        if(all(!y %in% names(x))){
            stop(paste(z, 
                " not extracted\n       [please contact openair if valid file]", 
                sep = ""), call. = FALSE)
        } 
        ans <- x[, y[y %in% names(x)]]
        if(!is.null(ncol(ans))) { ans <- ans[, 1] }
        ans
    }
    year <- fun.temp(met, c("YEAR"), "year")
    day <- fun.temp(met, c("DAY", "TDAY"), "day")
    hour <- fun.temp(met, c("HOUR", "THOUR"), "hour")

    met <- cbind(date = paste(year, day, hour, sep = "-"), met)
    met$date <- as.POSIXct(strptime(met$date, format = "%Y-%j-%H"), 
        "GMT")
    if (drop.input.dates) {
        met <- met[, !names(met) %in% 
            c("YEAR", "TDAY", "THOUR", "DAY", "HOUR", "MONTH", "DAY.OF.MONTH")]
    }

    if (simplify.names) {
        names(met) <- simplifyNamesADMS(names(met))
    }


    #drop messy name handling
    names(met) <- gsub("[.][.]", ".", names(met))
    names(met) <- gsub("^[.]", "", names(met))

    if (drop.case) {
        names(met) <- tolower(names(met))
    }
    met[] <- lapply(met, function(x) {
        replace(x, x == -999, NA)
    })
    ids <- which(is.na(met$date))
    if (length(ids) > 0) {
        if (length(ids) == nrow(met)) {
            stop("Invalid date (and time) format requested\n       [compare openair import settings and data structure]", 
                call. = FALSE)
        }
        met <- met[-ids, ]
        reply <- paste("Missing dates detected, removing", length(ids), 
            "line", sep = " ")
        if (length(ids) > 1) {
            reply <- paste(reply, "s", sep = "")
        }
        warning(reply, call. = FALSE)
    }
    print(unlist(sapply(met, class)))
    met
}

###############
##daughter
##importADMSMop

importADMSMop <- function(file=file.choose()
    , drop.case=TRUE, drop.input.dates=TRUE
    , keep.units=TRUE, simplify.names=TRUE
    , test.file.structure=TRUE
    , drop.delim = TRUE, add.prefixes = TRUE    
    , ...
){

#problem 
#mismatch in file header line end with lr; data lines end with comma then lr
#########
#written a catch for this

#problem
#no obvious file structure for testing
########
#provisional tester based on delim names

#problem
#r handling of x(y) names and x: names is messy
############
#added tidy to correct for this
#might need to rationalise names

#problem
#file contains lots of same names, input and processed
##############
#added an add.prefixes option to handle this

#problem 
#no keep.units options
##############
#no option to use

######################
#code

#read top line/data headers
check.names <- read.csv(file, header=FALSE, nrow=1, ...)
check.names <- make.names(as.vector(apply(check.names, 1, as.character)))
##tidy () handling; renaming x(y) as x.y. is messy
check.names <- ifelse(
    substr(check.names,nchar(check.names),nchar(check.names))=="."
    , substr(check.names,1,nchar(check.names)-1)
    , check.names
) 
##tidy 1/LMN
check.names <- gsub("X1.LMO", "RECIP.LMO", check.names)

x.1 <- which(check.names=="INPUT_DATA")
x.2 <- which(check.names=="PROCESSED_DATA")

if(test.file.structure){
  #check for delim columns
  if(length(x.1)==0 | length(x.2)==0){
    stop("File not recognised ADMS.mop structure\n       [please contact openair if valid]"
      , call. = FALSE
    )
  }
}

#read in data
ans <- read.csv(file, header=FALSE, skip=1
    , na.strings = c("", "NA", "-999", "-999.0")
    , ...
) 
ans[] <- lapply(ans, function(x) { replace(x, x == -999, NA) })

##check for extra empty column
if(length(ans[,ncol(ans)][!is.na(ans[,ncol(ans)])])==0) {
    ans <- ans[,1:(ncol(ans)-1)]
}
if(ncol(ans)!=length(check.names)){
    warning("Unexpected name/data mismatch, handled pragmatically\n       [compare openair import settings and data structure]"
        , call. = FALSE
    )
}

if(simplify.names) check.names <- simplifyNamesADMS(check.names)

##restructure names and data according to arguments and put together
if(is.logical(add.prefixes)==TRUE){
    if(add.prefixes==TRUE){
        check.names[(x.2[1]+1): length(check.names)] <- paste("PROCESS", check.names[(x.2[1]+1): length(check.names)], sep=".")
    }
} else {
    if(length(add.prefixes)==1){
        check.names[(x.2[1]+1): length(check.names)] <- paste(add.prefixes[1], check.names[(x.2[1]+1): length(check.names)], sep=".")
    } else {
        if(length(add.prefixes)>1){
            check.names[(x.1[1]+4): (x.2[1]-1)] <- paste(add.prefixes[1], check.names[(x.1[1]+4): (x.2[1]-1)], sep=".")
            check.names[(x.2[1]+1): length(check.names)] <- paste(add.prefixes[2], check.names[(x.2[1]+1): length(check.names)], sep=".")
        } else {
            warning("Unexpected add.prefixes option, option treated as FALSE\n       [check openair import settings]"
                , call. = FALSE)
        }
    }
}

names(ans) <- make.names(check.names, unique=TRUE)

##reset wd 0 to 360
##get current PHI terminology
temp <- if(simplify.names) simplifyNamesADMS("PHI") else "PHI" 
temp <- if(length(add.prefixes)>1) paste(add.prefixes[1], temp, sep=".") else temp
if(temp %in% names(ans)) {
    ans[, temp][ans[, temp]==0] <- 360
    warning("Zero wind directions encountered, resetting to 360"
        , call. = FALSE)
}

#if(simplify.names){
#    names(ans) <- simplifyNamesADMS(names(ans))
#}

#drop messy name handling
names(ans) <- gsub("[.][.]", ".", names(ans))
names(ans) <- gsub("^[.]", "", names(ans))

date <- paste(ans$TYEAR, ans$TDAY, ans$THOUR, sep = "-")
date <- as.POSIXct(strptime(date, format = "%Y-%j-%H"), "GMT")
if(drop.input.dates==TRUE){
    ans <- ans[,!names(ans) %in% c("TYEAR", "TDAY", "THOUR")]
}

if(drop.delim==TRUE){
    ans <- ans[,!names(ans) %in% c("PROCESSED_DATA", "INPUT_DATA")]
} 
ans <- cbind(date=date,ans)

if(drop.case==TRUE) {  
    names(ans) <- tolower(names(ans))
}

#error handling for bad days
ids <- which(is.na(ans$date))
if (length(ids) > 0) {
    if(length(ids)==nrow(ans)) {
        stop("Invalid date (and time) format requested\n       [compare openair import settings and data structure]"
            , call. = FALSE
        )
    }
    ans <- ans[-ids, ]
    reply <- paste("Missing dates detected, removing", length(ids), "line", sep=" ")
    if(length(ids)>1) { reply <- paste(reply,"s",sep="") }
    warning(reply, call. = FALSE)
}

print(unlist(sapply(ans, class)))
ans
}


###############
#daughter
#importADMSPst
#kr v0.1

importADMSPst <- function(file=file.choose()
    , drop.case=TRUE, drop.input.dates=TRUE
    , keep.units=TRUE, simplify.names=TRUE
    , test.file.structure=TRUE
    , drop.delim = TRUE, add.prefixes = TRUE    
    , ...
){

#notes
#########
#used the header/data dimension mismatcher handler from importADMSMop
#maybe not be needed.
#########
#no obvious file structure for testing
#provisional tester checks Hour, Day, Year and Receptor.name  in file names
#NEED to confirm these are standard, case specific, etc.
#########
#units are recovered from names row


#problems
#########
#currently dropping and ignoring Time(s)
#talk to Matt about how it works/is used
#########
#Not my name simplifications on Conc terms may need work
#talk to Matt/David 


######################
#code

#read top line/data headers
check.names <- read.csv(file, header=FALSE, nrow=1, ...)
check.names <- make.names(as.vector(apply(check.names, 1, as.character)))

#test structure
if(test.file.structure){
   temp <- c("Hour", "Day", "Year", "Receptor.name")
   test <- temp[temp %in% check.names]
   if(!identical(temp, test))
    stop("File not recognised ADMS.pst structure\n       [please contact openair if valid]"
      , call. = FALSE)
}

#read in data
ans <- read.csv(file, header=FALSE, skip=1
    , na.strings = c("", "NA", "-999", "-999.0")
    , ...
) 
ans[] <- lapply(ans, function(x) { replace(x, x == -999, NA) })

#match up data and names
if(ncol(ans)!=length(check.names)){
    warning("Unexpected name/data mismatch, handled pragmatically\n       [compare openair import settings and data structure]"
        , call. = FALSE
    )
}

names(ans) <- make.names(check.names, unique=TRUE)

date <- paste(ans$Year, ans$Day, ans$Hour, sep = "-")
date <- as.POSIXct(strptime(date, format = "%Y-%j-%H"), "GMT")

if(drop.input.dates==TRUE){
    ans <- ans[,!names(ans) %in% c("Year", "Day", "Hour", "Time.s.")]
}

#recover units from names
units <- rep(NA, ncol(ans))
units[grep("^.[.]m", names(ans))] <- "m"
units[grep("[.]ug.m3[.]", names(ans))] <- "ug/m3"
units[grep("[.]ug/m3[.]", names(ans))] <- "ug/m3"
units[grep("[.]ppb[.]", names(ans))] <- "ppb"
units[grep("[.]ppm[.]", names(ans))] <- "ppm"
if(length(na.omit(units))==0)
     units <- "units: unknown" else  
     units <- paste("units: ",paste(units, sep = "", collapse = ", "), sep="")

if(simplify.names){
    names(ans) <- simplifyNamesADMS(names(ans))
}

ans <- cbind(date=date,ans)

if(drop.case==TRUE) {  
    names(ans) <- tolower(names(ans))
}

comment(ans) <- c(comment(ans), units)

#error handling for bad days
ids <- which(is.na(ans$date))
if (length(ids) > 0) {
    if(length(ids)==nrow(ans)) {
        stop("Invalid date (and time) format requested\n       [compare openair import settings and data structure]"
            , call. = FALSE
        )
    }
    ans <- ans[-ids, ]
    reply <- paste("Missing dates detected, removing", length(ids), "line", sep=" ")
    if(length(ids)>1) { reply <- paste(reply,"s",sep="") }
    warning(reply, call. = FALSE)
}

print(unlist(sapply(ans, class)))
ans
}


###############
##daughter
##simplifyNamesADMS

simplifyNamesADMS <- function(names=NULL){
   #simplify.names lookup table for import.adms functions
   #v0.2 kr
   #handles as inputs (don't use after drop.case option)
   #names=NULL returns simplification operation summary

   if(is.null(names)) {
       message("Simplification operation summary")
       message("[ADMS => R => OPENAIR]:")
       fun.temp <- function(x,y,z){
           temp <- c(y, make.names(y), z)
           message(paste("\t", paste(temp, collapse=" => "), sep=""))
           temp <- data.frame(cbind(adms.input = temp[1], r.handling = temp[2], simplify.names = temp[3]),
                   stringsAsFactors = FALSE)
           x <- rbind(x,temp)
           x 
       }
       fun.temp.2 <- function(x,y,z, y.name) x
   } else {
       names <- make.names(names)
       fun.temp <- function(x,y,z){
           x[which(x == make.names(y))] <- z
           x
       }
       fun.temp.2 <- function(x,y,z, y.name) {
           x <- if(y.name) gsub(make.names(y),z,x) else (gsub(y,z,x))
       }
   }

   ############
   #update list
   ############
   #1/LMO
       names <- fun.temp(names, "1/LMO", "RECIP.LMO")
   #1/MONIN-OBUKHOV LENGTH
       names <- fun.temp(names, "1/MONIN-OBUKHOV LENGTH", "RECIP.LMO")
   #ALBEDO(D)
       names <- fun.temp(names, "ALBEDO(D)", "ALBEDO.DISP")
       names <- fun.temp(names, "ALBEDO (D)", "ALBEDO.DISP")
   #ALBEDO(DISP)
       names <- fun.temp(names, "ALBEDO(DISP)", "ALBEDO.DISP")
       names <- fun.temp(names, "ALBEDO (DISP)", "ALBEDO.DISP")
   #ALBEDO (DISPERSION AREA)
       names <- fun.temp(names, "ALBEDO (DISPERSION AREA)", "ALBEDO.DISP")
   #ALBEDO(M)
       names <- fun.temp(names, "ALBEDO(M)", "ALBEDO.MET")
       names <- fun.temp(names, "ALBEDO (M)", "ALBEDO.MET")
   #ALBEDO(MET)
       names <- fun.temp(names, "ALBEDO(MET)", "ALBEDO.MET")
       names <- fun.temp(names, "ALBEDO (MET)", "ALBEDO.MET")
   #ALBEDO (MET SITE)
       names <- fun.temp(names, "ALBEDO (MET SITE)", "ALBEDO.MET")
   #ALPHA
   ##########
   ##conflict
   ##########
   ##both alpha.disp and alpha.met seem to have been abbrev. to alpha
   #ALPHA(D) 
       names <- fun.temp(names, "ALPHA(D)", "ALPHA.DISP")
       names <- fun.temp(names, "ALPHA (D)", "ALPHA.DISP")
   #ALPHA(DISP)
       names <- fun.temp(names, "ALPHA(DISP)", "ALPHA.DISP")
       names <- fun.temp(names, "ALPHA (DISP)", "ALPHA.DISP")
   #ALPHA(M)
       names <- fun.temp(names, "ALPHA(M)", "ALPHA.MET")
       names <- fun.temp(names, "ALPHA (M)", "ALPHA.MET")
   #ALPHA(MET)
       names <- fun.temp(names, "ALPHA(MET)", "ALPHA.MET")
       names <- fun.temp(names, "ALPHA (MET)", "ALPHA.MET")
    #BL DEPTH
       names <- fun.temp(names, "BL DEPTH", "H")
   #BOUNDARY LAYER DEPTH
       names <- fun.temp(names, "BOUNDARY LAYER DEPTH", "H")
   #BUOYANCY FREQUENCY ABOVE BOUNDARY LAYER
       names <- fun.temp(names, "BUOYANCY FREQUENCY ABOVE BOUNDARY LAYER", "NU")
   #BUTADIENE
   #CL
   #CLOUD
       names <- fun.temp(names, "CLOUD", "CL")
   #CLOUD AMOUNT (OKTAS)
       names <- fun.temp(names, "CLOUD AMOUNT (OKTAS)", "CL")

   #Conc|ppb|[NAME]|All sources|-| 1hr
      names <- fun.temp(names, "Conc|ppb|[NAME]|[SOURCES]|-| 1hr", "[NAME].[SOURCES]")
   #Conc|ppm|[NAME]|All sources|-| 1hr
      names <- fun.temp(names, "Conc|ppm|[NAME]|[SOURCES]|-| 1hr", "[NAME].[SOURCES]")      
   #Conc|ug/m³|[NAME]|All sources|-| 1hr
      names <- fun.temp(names, "Conc|ug/m3|[NAME]|[SOURCES]|-| 1hr", "[NAME].[SOURCES]")
   #Conc|ug/m3|[NAME]|All sources|-| 1hr
      names <- fun.temp(names, "Conc|ug/m3|[NAME]|[SOURCES]|-| 1hr", "[NAME].[SOURCES]")
    
   #general for above 
      names <- fun.temp.2(names, "Conc|ppb|", "", TRUE)
      names <- fun.temp.2(names, "Conc|ppm|", "", TRUE)
      names <- fun.temp.2(names, "Conc|ug/m3|", "", TRUE)
      names <- fun.temp.2(names, "Conc|ug/m3|", "", TRUE)
      names <- fun.temp.2(names, "[.][.][.][.]1hr", "", FALSE)

   #D(RELATIVE HUMIDITY)/DZ ABOVE BOUNDARY LAYER (PERCENT/M)
       names <- fun.temp(names, "D(RELATIVE HUMIDITY)/DZ ABOVE BOUNDARY LAYER (PERCENT/M)", "DRHDZU")
   #DAY
   #DELTAPHI
       names <- fun.temp(names, "DELTAPHI", "DELTA.WD")
   #DELTAT
       names <- fun.temp(names, "DELTAT", "DELTA.T")
       names <- fun.temp(names, "DELTA T", "DELTA.T")
   #DELTATHETA 
       names <- fun.temp(names, "DELTATHETA", "DELTA.THETA")
       names <- fun.temp(names, "DELTA THETA", "DELTA.THETA")
   #DIRN CHANGE
       names <- fun.temp(names, "DIRN CHANGE", "DELTA.WD")
   #DRH/DZ
       names <- fun.temp(names, "DRH/DZ", "DRHDZU")
   #DRHDZU
   #FR
   #FREQUENCY
   #FTHETA0
   #GEOSTROPHIC MINUS SURFACE WIND DIRECTION (DEGREES)
       names <- fun.temp(names, "GEOSTROPHIC MINUS SURFACE WIND DIRECTION (DEGREES)", "DELTA.WD")
   #H
   #HEAT FLUX
       names <- fun.temp(names, "HEAT FLUX", "FTHETA0")
   #HOUR
   #HOURS
   #INCOMING SOLAR RADIATION
       names <- fun.temp(names, "INCOMING SOLAR RADIATION", "K")
   #INPUT_DATA:
   #K
   #LAMBDAE
   #LATENT HEAT FLUX
       names <- fun.temp(names, "LATENT HEAT FLUX", "LAMBDAE")
   #LAT HT FLUX
       names <- fun.temp(names, "LAT HT FLUX", "LAMBDAE")
   #MODIFIED PRIESTLEY-TAYLOR PARAMETER (DISPERSION AREA)
       names <- fun.temp(names, "MODIFIED PRIESTLEY-TAYLOR PARAMETER (DISPERSION AREA)", "ALPHA.DISP")
   #MODIFIED PRIESTLEY-TAYLOR PARAMETER (MET SITE)
       names <- fun.temp(names, "MODIFIED PRIESTLEY-TAYLOR PARAMETER (MET SITE)", "ALPHA.MET")
   #MONTHS
   #N ABOVE BL
       names <- fun.temp(names, "N ABOVE BL", "NU")
   #NO2
   #NOx
   #NU
   #P
   #PM10
   #PM2.5
   #O3
   #Q0
   #PHI
       names <- fun.temp(names, "PHI", "WD")
   #PHI0
       names <- fun.temp(names, "PHI0", "WD.0")
   #PHIG
       names <- fun.temp(names, "PHIG", "WD.G")
   #PHISEC
       names <- fun.temp(names, "PHISEC", "WD.SEC")
   #PRECIP
       names <- fun.temp(names, "PRECIP", "P")
   #PRECIPITATION RATE (MM/HOUR)
       names <- fun.temp(names, "PRECIPITATION RATE (MM/HOUR)", "P")
   #PROCESSED_DATA:
   #R
       names <- fun.temp(names, "R", "ALBEDO.MET")
   #RECIPLMO
       names <- fun.temp(names, "RECIPLMO", "RECIP.LMO")
   #RELATIVE HUMIDITY ABOVE BOUNDARY LAYER (PERCENT)
       names <- fun.temp(names, "RELATIVE HUMIDITY ABOVE BOUNDARY LAYER (PERCENT)", "RHU")
   #RH ABOVE BL
       names <- fun.temp(names, "RH ABOVE BL", "RHU")
   #RHU
   #RHUM
       names <- fun.temp(names, "RHUM", "RHU")
   #ROUGHNESS LENGTH (DISPERSION AREA)
       names <- fun.temp(names, "ROUGHNESS LENGTH (DISPERSION AREA)", "Z0.DISP")
   #ROUGHNESS LENGTH (MET SITE)
       names <- fun.temp(names, "ROUGHNESS LENGTH (MET SITE)", "Z0.MET")
   #RUN
   #S HUMIDITY
       names <- fun.temp(names, "S HUMIDITY", "SHU")   
   #SEA SURFACE TEMPERATURE (C)
       names <- fun.temp(names, "SEA SURFACE TEMPERATURE (C)", "TSEA")
   #SEA TEMP
       names <- fun.temp(names, "SEA TEMP", "TSEA")
   #SENSIBLE HEAT FLUX
       names <- fun.temp(names, "SENSIBLE HEAT FLUX", "FTHETA0")
   #SIGMATHETA
       names <- fun.temp(names, "SIGMATHETA", "SIGMA.THETA") 
       names <- fun.temp(names, "SIGMA THETA", "SIGMA.THETA") 
   #SIGMA THETA (DEGREES)
       names <- fun.temp(names, "SIGMA THETA (DEGREES)", "SIGMA.THETA") 
   #SO2
   #SOLAR RAD
       names <- fun.temp(names, "SOLAR RAD", "K") 
   #SPECIFIC HUMIDITY
       names <- fun.temp(names, "SPECIFIC HUMIDITY", "SHU") 
   #T0C
       names <- fun.temp(names, "T0C", "TEMP") 
   #TDAY
   #TEMPERATURE
       names <- fun.temp(names, "TEMPERATURE", "TEMP") 
   #TEMPERATURE (C)
       names <- fun.temp(names, "TEMPERATURE (C)", "TEMP") 
   #TEMPERATURE JUMP ACROSS BOUNDARY LAYER TOP
       names <- fun.temp(names, "TEMPERATURE JUMP ACROSS BOUNDARY LAYER TOP", "DELTA.THETA") 
   #THOUR
   #TEMPERATURE OVER LAND MINUS SEA SURFACE TEMPERATURE
       names <- fun.temp(names, "TEMPERATURE OVER LAND MINUS SEA SURFACE TEMPERATURE", "DELTA.T")
   #TSEA
   #TYEAR
   #U
       names <- fun.temp(names, "U", "WS")
   #UG
       names <- fun.temp(names, "UG", "WS.G")
   #UGSTAR
       names <- fun.temp(names, "UGSTAR", "WS.GSTAR")
   #USTAR
       names <- fun.temp(names, "USTAR", "WS.STAR")
   #WIND DIRN
       names <- fun.temp(names, "WIND DIRN", "WD")
   #WIND DIRECTION (DEGREES)
       names <- fun.temp(names, "WIND DIRECTION (DEGREES)", "WD")
   #WIND HEIGHT
       names <- fun.temp(names, "WIND HEIGHT", "WIND.HEIGHT")
   #WIND MEASUREMENT HEIGHT
       names <- fun.temp(names, "WIND MEASUREMENT HEIGHT", "WIND.HEIGHT")
   #WIND SPEED
       names <- fun.temp(names, "WIND SPEED", "WS")
   #WSTAR
   #X(m)
     names <- fun.temp(names, "X(m)", "X")
   #Y(m)
     names <- fun.temp(names, "Y(m)", "Y")
   #YEAR
   #Z(m)
     names <- fun.temp(names, "Z(m)", "Z")
   #Z0(D)
       names <- fun.temp(names, "Z0(D)", "Z0.DISP")
       names <- fun.temp(names, "Z0 (D)", "Z0.DISP")
   #Z0(DISP)
       names <- fun.temp(names, "Z0(DISP)", "Z0.DISP")
       names <- fun.temp(names, "Z0 (DISP)", "Z0.DISP")
  #Z0(M)
       names <- fun.temp(names, "Z0(M)", "Z0.MET")
       names <- fun.temp(names, "Z0 (M)", "Z0.MET")
  #Z0(MET)
       names <- fun.temp(names, "Z0(MET)", "Z0.MET")
       names <- fun.temp(names, "Z0 (MET)", "Z0.MET")

   ########
   #outputs
   ########
   invisible(names)
}




