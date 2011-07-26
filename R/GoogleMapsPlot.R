#####################
#openair map plotting
#####################
#working
#
#kr 07/07/2011 v 

#######################
#file contains 
#scripts for:
#######################
#function: GoogleMapsPlot 
#function: panel.GoogleMaps
#function: panel.GoogleMapsRaster
#

######################
#notes
######################
#GoogleMapsPlot 
#requires/loads RgoogleMaps (openair suggests)
#requires/loads Rgdal (NEEDS to be openair suggests)
#THIS MAY CHANGE re RgoogleMaps depends
#
######################
#map.panels TRUE/FALSE
#selects panel.GoogleMapsRaster or panel.GoogleMaps
#structure of both panel...(map)
#one arg, map; no extra args allowed
#no checking that map is valid 
#map is modification of RgoogleMaps output
#


#####################
#to do
#####################
#cols, cex, pch handling
#openair panels??
##e.g. panel.bubbleplot,
#could we make this control the key type?
#because the bubble key might want to be different?
#


#####################
#suggests
#####################
#handle more than one pollutant?
#more control of key to allow different key types via same command
#



#####################
#####################
##FUNCTIONS
#####################
#####################


#################################
##GoogleMapsPlot
#################################

GoogleMapsPlot <- function(mydata, 
         latitude = "latitude", longitude = "longitude", type = "default",
         xlim, ylim, pollutant = NULL, cols = "default", limits = c(0,100),
         cex = pollutant, pch = NULL, cex.range =c(1,10),
         xlab = longitude, ylab = latitude, main = "",
         map = NULL, map.raster = TRUE, map.cols = NULL, 
         aspect = 1, as.table = TRUE, plot.type = "xy",  
         key = NULL, key.position = "right",
         key.header = "", key.footer = pollutant,
         auto.text = TRUE, ...
){

#googleMapsPlot
#openair flavour
#karl 2011-07-07

#to do
#########################
#same type and something else conflict
#


#uses 
#RgoogleMaps MapBackground, etc.
    stopifnot(require("RgoogleMaps"))

##################
#need to confirm this has not changed
#check depends with Markus
#################
#stopifnot(require("rgdal")) 

    ##########################
    #greyscale handling/setup
    ##########################
    if (length(cols) == 1 && cols == "greyscale") {
        #strip and special greyscale
        current.strip <- trellis.par.get("strip.background")
        trellis.par.set(list(strip.background = list(col = "white")))
        if(is.null(map.cols))
            map.cols <- "greyscale"
    }

    ##########################
    #misc set ups, checks, etc
    ##########################

    #robust args handling
    extra.args <- list(...)

    #map panel
    map.panel <- if(map.raster)     
                     panel.GoogleMapsRaster else panel.GoogleMaps

    #plot.type
    ##predefined cases
    if(is.character(plot.type) && plot.type == "xy")
        plot.type <- panel.xyplot
    if(is.character(plot.type) && plot.type == "level")
        plot.type <- panel.levelplot
        
    if(!is.function(plot.type)){
        warning(paste("GoogleMapsPlot did not recognise 'plot.type'",
            "\n\t[applying default]", sep=""), call.=FALSE)
        plot.type <- panel.xyplot
    }
    
    #pollutant only 1 allowed
    #see suggestions
    if(length(pollutant) > 1){
        warning(paste("GoogleMapsPlot only allows one 'pollutant'",
            "\n\t[ignoring all but first]", sep=""), call.=FALSE)
        pollutant <- pollutant[1]
    }

    #type upto 2 levels
    if(length(type) > 2){
        warning(paste("GoogleMapsPlot allows up to two 'type' values",
            "\n\t[ignoring all but first two]", sep=""), call.=FALSE)
        type <- type[1:2]
    }

    ############################
    #checkPrep
    ############################
    #get cex and pch if characters
    temp <- na.omit(sapply(list(cex, pch), function(x){
                      if("character" %in% is(x)) x else NA}))
   
    #keep date if about
    temp <- if("date" %in% names(mydata))
                c("date", pollutant, temp) else
                    c(pollutant, temp)
    #all of x, y, temp need to be handled as type here
    mydata <- checkPrep(mydata, temp, type=c(latitude, longitude, type), 
                        remove.calm = FALSE)

    ############################
    #type, cutData handling
    ############################
    newdata <- cutData(mydata, type, ...)

    ############################
    #pollutant, cols, etc.
    ############################

    #z pollutant if set else default
    z <- if(is.null(pollutant))
             1 else mydata[, pollutant]

    #cex.range setup
    if(is.null(cex.range))
        cex.range <- FALSE
    if(is.logical(cex.range) && cex.range)
        cex.range <- c(1,10)
    if(is.numeric(cex.range)){
        temp <- range(cex.range, na.rm = TRUE, finite = TRUE)
        cex.range <- if(length(na.omit(temp)) < 2)
                         FALSE else temp
    }

    #cex default 
    if(is.null(cex)){
        cex <- if(is.numeric(cex.range)) mean(cex.range) else 1
    } else {
       if(is.character(cex)){
           cex <- as.numeric(mydata[, cex[1]])
           temp <- range(cex, na.rm = TRUE, finite = TRUE)
           my.range <- if(length(na.omit(temp)) < 2)
                           FALSE else temp
           if(is.numeric(cex.range)){
               if(my.range[1] == my.range[2]){
                   cex[cex == my.range[1]] <- mean(cex.range)
               } else {
                   temp <- (cex.range[2]-cex.range[1]) / (my.range[2]-my.range[1])
                   cex <- cex.range[1] + (cex - my.range[1]) * temp
               }
           }
       }
    }

    #pch handling
    if(is.null(pch))
        pch <- 20
    if(is.character(pch))
        pch <- as.numeric(mydata[, pch[1]])

    #cols handling
    #belt and braces
    if(is.null(cols))
        cols <- "default"
    #if map.cols and cols same use darker range
    col.range <- if(identical(map.cols, cols)) 
                     openColours(cols, 156)[56:156] else openColours(cols, 101)

    if(missing(limits)){
        breaks <- seq(min(z, na.rm = TRUE), quantile(z, probs = 0.95, na.rm = TRUE), length = 101)
            if(max(breaks, na.rm=TRUE) < max(z, na.rm = TRUE)){
                breaks <- seq(min(z, na.rm = TRUE), quantile(z, probs = 0.95, na.rm = TRUE), length = 100)
                breaks <- c(breaks, max(z, na.rm = TRUE))
        }
    } else {
        breaks <- seq(limits[1], limits[2], length = 101)
    }

    temp <- range(breaks)
    mycols <- ifelse(z <= temp[2] & z >= temp[1], z, NA)
    if(temp[1] == temp[2]){
        mycols[!is.na(mycols)] <- col.range[50]
        breaks <- do.breaks(c(temp[1] - 1, temp[1] + 1), 101)
    } else {
        mycols <- col.range[cut(mycols, c(breaks[1] - 1, breaks), labels = FALSE)]
    }
   
    ################
    #add in drawOpenKey
    ################

    #if key null but scalable data present
    #force key
    if(is.null(key))
        if(length(unique(z))>1) 
            key = TRUE

    #make legend using defaults
    legend <- list(col = col.range, at = breaks, space = key.position,
                  auto.text = auto.text, footer = key.footer, header = key.header,
                  height = 1, width = 1.5, fit = "all")
    legend <- makeOpenKeyLegend(key, legend, "trendLevel")

    ###############
    #main map call
    ###############

    #get map not suppled
    if(is.null(map)){

        #get xlims from data/local if not set
        temp.y <- if(missing(ylim)) 
                      mydata[, latitude] else ylim
        temp.x <- if(missing(xlim)) 
                      mydata[, longitude] else xlim

        #get names of args that MapBackground can handle
        temp <- unique(c(names(formals(MapBackground)), 
                         names(formals(GetMap.bbox)),
                         names(formals(GetMap))))

        #override some RgoogleMaps defaults
        map <- list(lon = temp.x, lat = temp.y, destfile = "XtempX.png", 
                     size = c(640,640))

        ##update my defaults with relevant ones in call
        map <- listUpdate(map, extra.args, subset.b = temp)

        #use MapBackground and list of allowed args
        map <- try(do.call(MapBackground, map), silent = TRUE)
        if(is(map)[1] == "try-error")
            stop(paste("\tGoogleMapsPlot could not apply supplied lat, lon and RgoogleMap args",
                       "\n\t[check call settings and data source]", sep = ""),
                 call.=FALSE)
    } 
       
    #get xlim, ylim from map if not supplied
    #use map lims to reset borders for plot
    #(is larger than data range 
    #and already done by RgoogleMaps!)

    if(missing(xlim))
        xlim <- c(map$BBOX$ll[2], map$BBOX$ur[2])
    if(missing(ylim))
        ylim <- c(map$BBOX$ll[1], map$BBOX$ur[1])   
 
    ra <- dim(map$myTile) 

    ##############
    #recolor map 
    #############
    if(!is.null(map.cols)){
        #if map.cols and cols same use lighten map range
        temp <- length(attr(map[[4]], "COL"))
        attr(map[[4]], "COL") <- if(identical(map.cols, cols)) 
                                     openColours(map.cols, 7 * temp)[1:temp] else 
                                     openColours(map.cols, temp)
    }

    #################
    #restructure map for panel
    #################
    #NOTE:
    #after any map recolor
    #################
    #NOTE: this assumes
    #imagematrix, png
    map$myTile <- matrix(attr(map$myTile, "COL")[map$myTile],
                          nrow = ra[1], ncol = ra[2]    
                  )
    map$myTile <- t(map$myTile)
    map$myTile <- map$myTile[nrow(map$myTile):1,]

###############
#temp addition
###############
#while testing xlim/ylim
###############
    map$xlim <- xlim
    map$ylim <- ylim
#########


    #############################
    #plot set up
    #############################

    #xyplot formula
    myform <- paste(latitude, " ~ ", longitude, sep = "")
    if(length(type[type!="default"]) > 0)
        myform <- paste(myform, " | ", paste(type, collapse = "+"), sep = "")
    myform <- formula(myform)

    #labels
    main <- quickText(main, auto.text)
    xlab <- quickText(xlab, auto.text)
    ylab <- quickText(ylab, auto.text)

    ##############################
    #plot
    ##############################

    plt <- xyplot(myform, data = mydata, z = z, 
                  cex = cex, pch = pch, xlim = xlim, ylim = ylim, 
                  col = mycols, aspect = aspect, as.table = as.table, 
                  at = breaks, col.regions = col.range,
                  main = main, xlab = xlab, ylab = ylab,
                  panel = function(x, y, subscripts, at, col.regions, ...){ 
                                   map.panel(map)
                                   temp <- list(...)
                                   if(!is.null(subscripts)){
                                        temp <- lapply(temp, function(x) 
                                        x <- if(length(x)>1) x[subscripts] else x )
                                        subscripts <- 1:length(subscripts)
                                   }
                                   temp <- listUpdate(
                                               list(x = x, y = y, z = temp$z, at = at, 
                                                    col.regions = col.regions, subscripts = subscripts), 
                                               temp)
                                   do.call(plot.type, temp)
                  }, legend = legend, ...
    )

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
    #reset if greyscale
    if (length(cols) == 1 && cols == "greyscale")
        trellis.par.set("strip.background", current.strip)
    output <- list(plot = plt, data = list(data = mydata, map = map), call = match.call())
    class(output) <- "openair"
    invisible(output)
}


#################################
##panel.GoogleMapsRaster
#################################
#raster map panel
#

panel.GoogleMapsRaster <- function(map){
    grid.raster(map$myTile, 
         x = unit(map$BBOX$ll[2], "native"), y = unit(map$BBOX$ll[1], "native"),
         width = unit(map$BBOX$ur[2] - map$BBOX$ll[2], "native"),  
         height = unit(map$BBOX$ur[1] - map$BBOX$ll[1], "native"), 
         just = c("left", "bottom")
    )
}



#################################
##panel.GoogleMaps
#################################
#non-raster map panel
#

panel.GoogleMaps <- function(map){
    
    #there has to be a better way

    ra <- dim(map$myTile) 
    map.col <- as.vector(map$myTile[1:ra[1], 1:ra[2]])
    map.lon <- rep(seq(map$BBOX$ll[2], map$BBOX$ur[2], 
                       length.out = ra[1]), 
                   each = ra[2])
    map.lat <- rep(seq(map$BBOX$ur[1], map$BBOX$ll[1], 
                       length.out = ra[2]), 
                   time = ra[1])
    width <- (map$BBOX$ll[2] - map$BBOX$ur[2]) / ra[1]
    height <- (map$BBOX$ll[1] - map$BBOX$ur[1]) / ra[2]

    panel.rect(x = map.lon, y = map.lat, 
               width = width, height = height, 
               col = map.col, border = map.col)
}



