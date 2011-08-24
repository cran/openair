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
#function: openairMapManager 
#function: panel.GoogleMaps
#function: panel.GoogleMapsRaster
#

######################
#notes
######################
#GoogleMapsPlot 
#requires/loads RgoogleMaps (openair suggests)
#RgoogleMaps version 1.1.9.13 (not 10)
#requires png package for png
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
         xlim, ylim, pollutant = NULL, labels = NULL, cols = "default", 
         limits = c(0,100), cex = pollutant, pch = NULL, cex.range =c(2,10),
         xlab = longitude, ylab = latitude, main = "",
         map = NULL, map.raster = TRUE, map.cols = NULL, 
         aspect = 1, as.table = TRUE, plot.type = "xy", 
         plot.transparent = FALSE,
         key = NULL, key.position = "right",
         key.header = "", key.footer = pollutant,
         auto.text = TRUE, ...
){

#googleMapsPlot
#openair flavour
#karl 2011-08-04

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
#rgdal/png depending on RgoogleMaps version
#so let RgoogleMaps handle its depends
#

##########################
#column assignment in args
###########################
#below code assumes 
#col assignment by col
#######################
#could have characters or numerics?
#to allow assignment by col number
#

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

    #labels only 1 allowed
    #can be vector or list

    if(is.list(labels)){
        temp <- labels$labels
        label2 <- if(length(temp) > 0) 
                        labels$labels else NULL
    } else label2 <- if(length(labels) > 0)
                         labels else NULL

    if(length(label2) > 1)
        warning(paste("GoogleMapsPlot only allows one 'labels' source",
                "\n\t[ignoring all but first]", sep=""), call.=FALSE)
            label2 <- label2[1]

    ############################
    #checkPrep
    ############################
    #get cex, pch, labels if characters
    #using label2 incase list element

    temp <- na.omit(sapply(list(cex, pch, label2), function(x){
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
             rep(1, nrow(newdata)) else mydata[, pollutant]

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

    #make transparent

    if(is.logical(plot.transparent) && plot.transparent)
        plot.transparent <- 0.5
    
    if(is.numeric(plot.transparent)){
        if(any(plot.transparent < 0) | any(plot.transparent > 1)){
            warning(paste("GoogleMapsPlot could sensibly apply requested 'plot.transparency'",
                          "\n\t[Sugest numeric in range 0 to 1]", 
                          "\n\t[resetting value(s) to default, 0.5]",
                    sep=""), call.=FALSE)
            plot.transparent[plot.transparent > 1] <- 0.5
            plot.transparent[plot.transparent < 0] <- 0.5
        }
        col.range <- col2rgb(col.range)
        col.range <- rgb(col.range[1,], col.range[2,], col.range[3,], 
                      alpha = plot.transparent * 255, max = 255)
    }

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
    #labels handling
    ################
    
    #get labels source

#note 
#currently make labels even if not there
#then don't plot if labels$labels NULL
#rethink?

    if(!is.null(label2))
        label2 <- mydata[, label2]    
    
    #default label style

    temp <- list(labels = label2, cex = 0.75, col = "red", lwd=2)
    labels <- if(is.list(labels)) 
                  listUpdate(temp, labels[names(labels) != "labels"]) else temp 
    
    #check for label formatting
    temp <- labels$fun
    if(!is.null(temp) && is.function(temp))
        labels$labels <- temp(labels$labels)

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

        temp2 <- try(qbbox(lat = temp.y, lon = temp.x), silent = TRUE)
        if(is(temp2)[1] == "try-error")
            stop(paste("\tGoogleMapsPlot could not apply supplied lat, lon combination",
                       "\n\t[check call settings and data source]", sep = ""),
                 call.=FALSE)

        #override some RgoogleMaps defaults
        map <- list(lon = temp2$lonR, lat = temp2$latR, destfile = "XtempX.png", 
                     maptype = "terrain", size = c(640,640))

        ##update my defaults with relevant ones in call
        map <- listUpdate(map, extra.args, subset.b = temp)

        #use MapBackground and list of allowed args
        map <- try(do.call(GetMap.bbox, map), silent = TRUE)
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
 
     map <- openairMapManager(map)

###############
#temp addition
###############
#while testing xlim/ylim
###############

    map$xlim <- xlim
    map$ylim <- ylim

    ra <- dim(map$myTile) 

    ##############
    #recolor map 
    #############
    if(!is.null(map.cols)){

        #if map.cols and cols same use lighten map range
        if(identical(map.cols, cols))
            map.cols <- if(length(map.cols) == 1 && map.cols == "greyscale")
                            openColours(c("white", grey(0.65)), 10) else 
                            openColours(map.cols, 7)[1:2]

        #make an intensity scale
        temp <- apply(col2rgb(map$myTile), 2, prod)

        #reset cols in frame
        map$myTile <- level.colors(temp, pretty(temp, 200), openColours(map.cols, 200))
        dim(map$myTile) <- ra[1:2]
    }


    #############################
    #plot set up
    #############################

    #xyplot formula
    myform <- paste(latitude, " ~ ", longitude, sep = "")
    if(length(type[type!="default"]) > 0)
        myform <- paste(myform, " | ", paste(type, collapse = "+"), sep = "")
    myform <- formula(myform)


#labels handling

    #labels via quickText
    main <- quickText(main, auto.text)
    xlab <- quickText(xlab, auto.text)
    ylab <- quickText(ylab, auto.text)

#check quickText space addition
#can we drop it?

    if(!is.null(labels$labels))
             labels$labels <- sapply(labels$labels, function(x){ 
                                                        x <- paste(" ", x, sep = "")
                                                        quickText(x, auto.text)})



    ##############################
    #plot
    ##############################

    plt <- xyplot(myform, data = mydata, z = z, 
                  cex = cex, pch = pch, xlim = xlim, ylim = ylim, 
                  col = mycols, aspect = aspect, as.table = as.table, 
                  at = breaks, col.regions = col.range, 
                  main = main, xlab = xlab, ylab = ylab, labels = labels,
                  panel = function(x, y, subscripts, at, col.regions, ...){ 
                                   map.panel(map)
                                   temp <- list(...)
                                   if(!is.null(subscripts)){
                                        temp <- lapply(temp, function(x) 
                                        x <- if(length(x)>1) x[subscripts] else x )

                                        labels <- lapply(labels, function(x) 
                                        x <- if(length(x)>1) x[subscripts] else x )

                                        subscripts <- 1:length(subscripts)
                                   }
                                   temp <- listUpdate(
                                               list(x = x, y = y, z = temp$z, at = at, 
                                                    col.regions = col.regions, subscripts = subscripts), 
                                               temp)
                                   do.call(plot.type, temp)


                                   labels <- listUpdate(
                                               list(x = x, y = y, subscripts = subscripts), 
                                               labels)
                                   if(!is.null(labels$labels))
                                       do.call(ltext, labels)

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
#RgoogleMaps version handler
#IN DEVELOPMENT
#################################
#this was introduced on introduction of 
#RgoogleMaps 1.1.9.10-13
#rgdal/png dependent change
#at same time the structures map 
#structures changed 
#

openairMapManager <- function(map){

    #set up
    ra <- dim(map$myTile)
    
    #version test
    tempfun <- function(x, pck = "RgoogleMaps") 
                  compareVersion(packageDescription(pck)$Version, x)

    #if RgoogleMaps version between 1.1.5 and 1.1.9.13 
    #currently don't know structure
    if(tempfun("1.1.9.13") < 0){
        warning(paste("GoogleMapsPlot may not be able to support this version of 'RgoogleMaps'",
                      "\n\t[You may encounter problems]", 
                      "\n\t[If so, suggest updating RgoogleMaps or contacting openair]",
                sep=""), call.=FALSE)

        #NOTE: this assumes
        #imagematrix, png
        #NOT tested for jgp
        map$myTile <- matrix(attr(map$myTile, "COL")[map$myTile],
                             nrow = ra[1], ncol = ra[2]    
                      )
        map$myTile <- t(map$myTile)
        map$myTile <- map$myTile[nrow(map$myTile):1,]
        attr(map$myTile, "type") <- "openair"
        return(map)
    }

    if(length(ra) > 2){
       
        if(ra[3] == 4 & attr(map$myTile, "type") == "rgb"){
            map$myTile <- rgb(map$myTile[, , 1], map$myTile[, , 2], 
                              map$myTile[, , 3], map$myTile[, , 4])
            dim(map$myTile) <- ra[1:2]
            attr(map$myTile, "type") <- "openair"
            return(map)
        }

        if(ra[3] == 3 & attr(map$myTile, "type") == "rgb"){
            map$myTile <- rgb(map$myTile[, , 1], map$myTile[, , 2], 
                              map$myTile[, , 3])
            dim(map$myTile) <- ra[1:2]
            attr(map$myTile, "type") <- "openair"
            return(map)
        }

        if(ra[3] == 1 & attr(map$myTile, "type") == "grey"){
            map$myTile <- grey(map$myTile[, , 1])
            dim(map$myTile) <- ra[1:2]
            attr(map$myTile, "type") <- "openair"
            return(map)
        }
    } 

    warning(paste("GoogleMapsPlot encountered unexpected 'RgoogleMaps' output",
                  "\n\t[You may encounter problems or some options may not be supported]", 
                  "\n\t[If so, suggest updating RgoogleMaps or contacting openair]",
            sep=""), call.=FALSE)
    return(map)

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

    #both the rect handling
    #and the map.col generation 
    #need thinking about

    if(attr(map$myTile, "type") != "openair")
        map <- openairMapManager(map)

    ra <- dim(map$myTile) 
    map.col <- map$myTile

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



