drawOpenKey <- function (key, draw = FALSE, vp = NULL) {

########################################
#drawOpenKey v0.2
########################################
#drawOpenKey is a modification of:
#draw.colorkey
#
#original code from lattice, reference:
#Deepayan Sarkar (2010). lattice: Lattice Graphics.
#R package version 0.18-5.
#http://r-forge.r-project.org/projects/lattice/
#
#additional code by Karl Ropkins, allows:
#some crude header and footer labelling
#text formatting by openair::quickText
#addition plot style and layout control
########################################

########################################
#The help, advice and extreme patience
#of Deepayan Sarkar are also gratefully 
#acknowledged
########################################

    ################
    #quick end if key obviously not right
    ################
    if (!is.list(key))
        stop("In drawOpenKey(...) key must be a list", 
            call. = FALSE) 

    ################
    #special case
    #windRose colour key
    ################
    if(is.null(key$at)){
        if(is.null(key$labels)){
            stop("In drawOpenKey(...) neither 'at' nor 'labels' in key",
                "\n\tplease suppied at least one",
                call. = FALSE)
        } else {
            if(is.list(key$labels)){
               if(is.null(key$labels$labels))
                    stop("In drawOpenKey(...) unable to recover missing 'at' in key",
                        "\n\tplease check 'labels' structure or add 'at'",
                        call. = FALSE)
                key$at <- 0:length(key$labels$labels)
                if(is.null(key$labels$at)) {
                    key$labels$at <- 1:length(key$labels$labels) - 0.5
                } 
            } else { 
                key$at <- 0:length(key$labels)
                key$labels <- list(labels = key$labels,
                                   at = 1:length(key$labels) - 0.5)
            }
        }
    }

    ################
    #process key 
    #modification of sk
    ################
    process.key <- function(col = regions$col, alpha = regions$alpha, 
        at, tick.number = 7, width = 2, height = 1, space = "right",
        plot.style = c("ticks", "border"), 
        ...) {
        regions <- trellis.par.get("regions")
        list(col = col, alpha = alpha, at = at, tick.number = tick.number, 
            width = width, height = height, space = space,
            plot.style = plot.style, 
            ...)
    }    
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")
    key <- do.call("process.key", key)

    ###############
    #test space 
    #otherwise drops without creating key.gf
    #COULD default to one?
    ###############
    temp <- c("right", "left", "top", "bottom")
    key$space <- pmatch(key$space, temp)
    if (is.na(key$space)) {
        stop(" In drawOpenKey(...):", "\n\tspace argument in key not recognised", 
            "\n\tplease use one or abbreviation of:\n\t\"", paste(temp, 
                sep = "", collapse = "\", \""), "\"", call. = FALSE)
    }
    key$space <- temp[key$space]

    ###############
    #original sk key handling
    #with
    #modified error messaging
    ###############
    check.overlap <- TRUE
    key$at <- sort(key$at)
    numcol <- length(key$at) - 1
    key$col <- level.colors(x = seq_len(numcol) - 0.5, at = seq_len(numcol + 
        1) - 1, col.regions = key$col, colors = TRUE)
    atrange <- range(key$at, finite = TRUE)
    scat <- as.numeric(key$at)
    reccentre <- (scat[-1] + scat[-length(scat)])/2
    recdim <- diff(scat)
    cex <- axis.text$cex
    col <- axis.text$col
    font <- axis.text$font
    fontfamily <- axis.text$fontfamily
    fontface <- axis.text$fontface
    rot <- 0
    if (is.null(key$lab)) {
        at <- pretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- format(at, trim = TRUE)
    } else if ((is.character(key$lab) | is.expression(key$lab) | is.numeric(key$lab))  
           && length(key$lab) == length(key$at)) {
        check.overlap <- FALSE
        at <- key$at
        labels <- key$lab
    } else if (is.list(key$lab)) {
        at <- if (!is.null(key$lab$at)) 
            key$lab$at
        else pretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- if (!is.null(key$lab$lab)) {
            check.overlap <- FALSE
            key$lab$lab
    } else format(at, trim = TRUE)
        if (!is.null(key$lab$cex)) 
            cex <- key$lab$cex
        if (!is.null(key$lab$col)) 
            col <- key$lab$col
        if (!is.null(key$lab$font)) 
            font <- key$lab$font
        if (!is.null(key$lab$fontface)) 
            fontface <- key$lab$fontface
        if (!is.null(key$lab$fontfamily)) 
            fontfamily <- key$lab$fontfamily
        if (!is.null(key$lab$rot)) 
            rot <- key$lab$rot
    } else { 
    stop("In drawOpenKey(...) unexpected labels structure in key",
            "\n\tplease check 'labels' structure",
            "\n\tor see 'labels' in ?drawOpenKey", 
            call. = FALSE)    }
    labscat <- at
    rot <- 0

    #############
    #header set up
    #############
    if(is.null(key$hea)) 
        key$hea <- list(header="")
    if(is.character(key$hea) | is.numeric(key$hea) | is.expression(key$hea) ) 
        key$hea <- list(header=key$hea)
    if(is.list(key$hea)){
        h.text <- if(is.null(key$hea$hea)) "" else key$hea$hea
        h.tweaks <- if(is.null(key$hea$twe)) c("gap", "balance") else key$hea$twe
        h.auto.text <- if(is.null(key$hea$auto.text)) TRUE else key$hea$auto.text
        h.slot <- if(is.null(key$hea$slot)) 0.05 else key$hea$slot
    } else {
    stop("In drawOpenKey(...) unexpected header structure in key",
            "\n\tplease check 'header' structure",
            "\n\tor see 'header' in ?drawOpenKey", 
            call. = FALSE)    
    }

    ############
    #footer setup
    ############
    if(is.null(key$foo)) 
        key$foo <- list(footer="")
    if(is.character(key$foo) | is.numeric(key$foo) | is.expression(key$foo) ) 
        key$foo <- list(footer=key$foo)
    if(is.list(key$foo)){
        f.text <- if(is.null(key$foo$foo)) "" else key$foo$foo
        f.tweaks <- if(is.null(key$foo$twe)) c("gap", "balance") else key$foo$twe
        f.auto.text <- if(is.null(key$foo$auto.text)) TRUE else key$foo$auto.text
        f.slot <- if(is.null(key$foo$slot)) 0.05 else key$foo$slot
    } else {
        stop("In drawOpenKey(...) unexpected footer structure in key",
            "\n\tplease check 'footer' structure",
            "\n\tor see 'footer' in ?drawOpenKey", 
            call. = FALSE)    
    }

    #################
    #higher level handling
    #auto.text, slot, tweak, 
    #################
    if(!is.null(key$auto.text)) {
        if(is.logical(key$auto.text)){
            h.auto.text <- key$auto.text
            f.auto.text <- key$auto.text
        }
    }
    if(!is.null(key$slot)) {
        if(is.numeric(key$slot)){
            h.slot <- key$slot
            f.slot <- key$slot
        }
    }
    if(!is.null(key$twe)){
        if(is.vector(key$twe)){
            h.tweaks <- key$twe
            f.tweaks <- key$twe
        }
    }

    ###############
    #size text boxes, balance and gap
    #for
    #top and bottom only
    ###############
    h.text <- if(length(h.text) < 3)  c(rep("", 3-length(h.text)), h.text) else
        h.text[1:3] 
    h.slots <- ifelse(as.character(h.text) != "", h.slot, 0)
    f.text <- c(f.text, rep("", 3))[1:3]
    f.slots <- ifelse(as.character(f.text) != "", f.slot, 0)
    if(sum(h.slots) > sum(f.slots) & "balance" %in% f.tweaks) 
        f.slots[3] <- f.slots[3] + sum(h.slots) - sum(f.slots)
    if(sum(f.slots) > sum(h.slots) & "balance" %in% h.tweaks) 
        h.slots[1] <- h.slots[1] + sum(f.slots) - sum(h.slots)
    g.slots <- c(if("gap" %in% h.tweaks & sum(c(h.slots,f.slots))>0) h.slot else 0,
                   if("gap" %in% f.tweaks & sum(c(h.slots,f.slots))>0) f.slot else 0)

    #############  
    #scale fit
    #scale, soft and all
    #default all
    #############
    s.slot <- 1 - sum(c(h.slots,f.slots,g.slots))
    s.offsets <- c(0, 0)
    if(!is.null(key$fit)) {
        if(is.character(key$fit)){
           if(key$fit=="soft")
               s.slot <- 1 - (sum(c(h.slots,f.slots,g.slots))/2)
           if(key$fit=="scale"){
               s.slot <- 1
               s.offsets <- c(sum(c(h.slots,g.slots[1])),
                              sum(c(f.slots,g.slots[2])))
            } 
        } else {
            stop("In drawOpenKey(...) unexpected fit structure in key",
                "\n\tplease check 'fit' structure",
                "\n\tor see 'fit' in ?drawOpenKey", 
                call. = FALSE)    
        }
    }

    ############
    #paddle style
    #recwd rescaling
    #############
    recwd <- if("paddle" %in% key$plot.style) 
        recwd <- seq(0.2, 1, length.out = length(key$at) - 1) else 
            recwd <- rep(1, length(key$at) - 1)
   
    #####################
    #right scale
    #size checks text see sac struff
    #positions
    #adds ticks and borders if requested    
    #####################
    if (key$space == "right") {
        h.text <- if(is.character(h.text))
             lapply(h.text, function(x) quickText(x, h.auto.text)) else
             list(h.text[1], h.text[2], h.text[3])
        f.text <- if(is.character(f.text))
             lapply(f.text, function(x) quickText(x, h.auto.text)) else
             list(f.text[1], f.text[2], f.text[3])
        #sac stuff handles spacing needed for headers, scale and footers
        sac.text <- c(labels, f.text[[1]], f.text[[2]], f.text[[3]], 
                              h.text[[1]], h.text[[2]], h.text[[3]])
        SacGrob <- textGrob(label = sac.text, x = rep(0, length(sac.text)), 
            y = at, vp = viewport(yscale = atrange), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == -90) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
        heights.x <- c(((1 -key$height)/2) - (key$height*s.offsets[1]), 
            key$height * h.slots[1], key$height * h.slots[2], key$height * h.slots[3],
            key$height * g.slots[1], (key$height * s.slot), key$height * g.slots[2], 
            key$height * f.slots[1], key$height * f.slots[2], key$height * f.slots[3],
            ((1 -key$height)/2) - (key$height*s.offsets[2]))
        heights.units <- rep("null", 11)
        temp <- if("ticks" %in% key$plot.style) 0.6 else 0.3
        widths.x <- c(0.6 * key$width, temp, 1)
        widths.units <- c("lines", "lines", "grobwidth")
        widths.data <- list(NULL, NULL, SacGrob)
        key.layout <- grid.layout(nrow = 11, ncol = 3, heights = unit(heights.x, 
            heights.units), widths = unit(widths.x, widths.units, 
            data = widths.data), respect = TRUE)
        key.gf <- frameGrob(layout = key.layout, vp = vp)
        add.header.footer <- function(key.gf, text, key.row, key.col){
            keyGrob <- textGrob(label = text, x = c(0), 
            y = c(0.5), vp = viewport(yscale = c(0,1)), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == -90) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
            placeGrob(key.gf, keyGrob, row = key.row, col = key.col)
        }
        key.gf <- add.header.footer(key.gf, h.text[[1]], 2, 3)
        key.gf <- add.header.footer(key.gf, h.text[[2]], 3, 3)
        key.gf <- add.header.footer(key.gf, h.text[[3]], 4, 3)
        key.gf <- add.header.footer(key.gf, f.text[[1]], 8, 3)
        key.gf <- add.header.footer(key.gf, f.text[[2]], 9, 3)
        key.gf <- add.header.footer(key.gf, f.text[[3]], 10, 3)
        key.gf <- placeGrob(key.gf, textGrob(label = labels, x = rep(0, length(at)), 
            y = at, vp = viewport(yscale = atrange), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == -90) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                font))), row = 6, col = 3)
        key.gf <- placeGrob(key.gf, rectGrob(x = rep(0.5, length(reccentre)), 
            y = reccentre, default.units = "native", vp = viewport(yscale = atrange), 
            height = recdim, width = recwd, gp = gpar(fill = key$col, col = "transparent", 
                alpha = key$alpha)), row = 6, col = 1)
        if("border" %in% key$plot.style)
            key.gf <- placeGrob(frame = key.gf, rectGrob(gp = gpar(col = axis.line$col, 
                lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, 
                fill = "transparent")), row = 6, col = 1)
        if("ticks" %in% key$plot.style)
            key.gf <- placeGrob(frame = key.gf, segmentsGrob(x0 = rep(0, 
                length(labscat)), y0 = labscat, x1 = rep(0.4, length(labscat)), 
                y1 = labscat, vp = viewport(yscale = atrange), default.units = "native", 
                gp = gpar(col = axis.line$col, lty = axis.line$lty, 
                    lwd = axis.line$lwd)), row = 6, col = 2)
    }

    #####################
    #left scale
    #size checks text see sac struff
    #positions
    #adds ticks and borders if requested    
    #####################
    else if (key$space == "left") { 
        h.text <- if(is.character(h.text))
             lapply(h.text, function(x) quickText(x, h.auto.text)) else
             list(h.text[1], h.text[2], h.text[3])
        f.text <- if(is.character(f.text))
             lapply(f.text, function(x) quickText(x, h.auto.text)) else
             list(f.text[1], f.text[2], f.text[3])
        #sac stuff handles spacing needed for headers, scale and footers
        sac.text <- c(labels, f.text[[1]], f.text[[2]], f.text[[3]], 
                              h.text[[1]], h.text[[2]], h.text[[3]])
        SacGrob <- textGrob(label = sac.text, x = rep(0, length(sac.text)), 
            y = at, vp = viewport(yscale = atrange), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 90) 
                c("center", "bottom")
            else c("right", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
        heights.x <- c(((1 -key$height)/2) - (key$height*s.offsets[1]), 
            key$height * h.slots[1], key$height * h.slots[2], key$height * h.slots[3],
            key$height * g.slots[1], (key$height * s.slot), key$height * g.slots[2], 
            key$height * f.slots[1], key$height * f.slots[2], key$height * f.slots[3],
            ((1 -key$height)/2) - (key$height*s.offsets[2]))
        heights.units <- rep("null", 11)
        temp <- if("ticks" %in% key$plot.style) 0.6 else 0.3
        widths.x <- c(1, temp, 0.6 * key$width)
        widths.units <- c("grobwidth", "lines", "lines")
        widths.data <- list(SacGrob, NULL, NULL)
        key.layout <- grid.layout(nrow = 11, ncol = 3, heights = unit(heights.x, 
            heights.units), widths = unit(widths.x, widths.units, 
            data = widths.data), respect = TRUE)
        key.gf <- frameGrob(layout = key.layout, vp = vp)
        add.header.footer <- function(key.gf, text, key.row, key.col){
            keyGrob <- textGrob(label = text, x = c(1), 
            y = c(0.5), vp = viewport(yscale = c(0,1)), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 90) 
                c("center", "bottom")
            else c("right", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
            placeGrob(key.gf, keyGrob, row = key.row, col = key.col)
        }
        key.gf <- add.header.footer(key.gf, h.text[[1]], 2, 1)
        key.gf <- add.header.footer(key.gf, h.text[[2]], 3, 1)
        key.gf <- add.header.footer(key.gf, h.text[[3]], 4, 1)
        key.gf <- add.header.footer(key.gf, f.text[[1]], 8, 1)
        key.gf <- add.header.footer(key.gf, f.text[[2]], 9, 1)
        key.gf <- add.header.footer(key.gf, f.text[[3]], 10, 1)
        key.gf <- placeGrob(key.gf, 
            textGrob(label = labels, x = rep(1, length(at)), 
                y = at, vp = viewport(yscale = atrange), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 90) 
                c("center", "bottom")
            else c("right", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
                , row = 6, col = 1)
        key.gf <- placeGrob(key.gf, rectGrob(x = rep(0.5, length(reccentre)), 
            y = reccentre, default.units = "native", vp = viewport(yscale = atrange), 
            height = recdim, width = recwd, gp = gpar(fill = key$col, col = "transparent", 
                alpha = key$alpha)), row = 6, col = 3)
        if("border" %in% key$plot.style)
            key.gf <- placeGrob(frame = key.gf, rectGrob(gp = gpar(col = axis.line$col, 
                lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, 
                fill = "transparent")), row = 6, col = 3)
        if("ticks" %in% key$plot.style)
            key.gf <- placeGrob(frame = key.gf, segmentsGrob(x0 = rep(0.5, 
                length(labscat)), y0 = labscat, x1 = rep(1, length(labscat)), 
                y1 = labscat, vp = viewport(yscale = atrange), default.units = "native", 
                gp = gpar(col = axis.line$col, lty = axis.line$lty, 
                    lwd = axis.line$lwd)), row = 6, col = 2)
    }

    #####################
    #top scale
    #positions
    #adds ticks and borders if requested    
    #####################
    else if (key$space == "top") {
       f.text <- f.text[as.character(f.text) != ""]
       f.text <- if(is.character(f.text)) quickText(paste(f.text, collapse="  "), f.auto.text) else
               as.expression(parse(text=paste(f.text, collapse="~~")))
       h.text <- h.text[as.character(h.text) != ""]
       h.text <- if(is.character(h.text)) quickText(paste(h.text, collapse="  "), f.auto.text) else
               as.expression(parse(text=paste(h.text, collapse="~~")))
       labelsGrob <- textGrob(label = labels, y = rep(0, length(at)), 
            x = at, vp = viewport(xscale = atrange), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 0) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
        keyGrob <- textGrob(label = f.text, y = c(0), 
            x = c(0.5), vp = viewport(xscale = c(0,1)), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 0) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
        keyGrob2 <- textGrob(label = h.text, y = c(0), 
            x = c(0.5), vp = viewport(xscale = c(0,1)), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 0) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
        widths.x <- c((1 - key$height)/2, key$height, (1 - key$height)/2)
        widths.units <- rep("null", 3)
        temp <- c(0,0,0,0.3)
        if(sum(f.slots[1:2])>0 & sum(h.slots[2:3])==0)
               temp[1:3] <- c(0,1,1.5)
        if(sum(f.slots[1:2])==0 & sum(h.slots[2:3])>0)
               temp[1:3] <- c(1,0,1.5)
        if(sum(f.slots[1:2])>0 & sum(h.slots[2:3])>0)
               temp[1:3] <- c(1,1.5,1.5)
        if(sum(f.slots[1:2])==0 & sum(h.slots[2:3])==0)
               temp[1:3] <- c(1,1,1)
        if("ticks" %in% key$plot.style)
               temp[4] <- 0.6
        heights.x <- c(temp[1], temp[2], temp[3], temp[4], 0.6 * key$width)
        heights.units <- c("grobheight", "grobheight", "grobheight", "lines", "lines")
        heights.data <- list(keyGrob2, keyGrob, labelsGrob, NULL, NULL)
        key.layout <- grid.layout(nrow = 5, ncol = 3, heights = unit(heights.x, 
            heights.units, data = heights.data), widths = unit(widths.x, 
            widths.units), respect = TRUE)
        key.gf <- frameGrob(layout = key.layout, vp = vp)
        key.gf <- placeGrob(key.gf, rectGrob(y = rep(0.5, length(reccentre)), 
            x = reccentre, default.units = "native", vp = viewport(xscale = atrange), 
            width = recdim, height = recwd, gp = gpar(fill = key$col, col = "transparent", 
                alpha = key$alpha)), row = 5, col = 2)
        if("border" %in% key$plot.style)
            key.gf <- placeGrob(frame = key.gf, rectGrob(gp = gpar(col = axis.line$col, 
                lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, 
                fill = "transparent")), row = 5, col = 2)
        if("ticks" %in% key$plot.style)
            key.gf <- placeGrob(frame = key.gf, segmentsGrob(y0 = rep(0, 
                length(labscat)), x0 = labscat, y1 = rep(0.4, length(labscat)), 
                x1 = labscat, vp = viewport(xscale = atrange), default.units = "native", 
                gp = gpar(col = axis.line$col, lty = axis.line$lty, 
                    lwd = axis.line$lwd)), row = 4, col = 2)
        key.gf <- placeGrob(key.gf, labelsGrob, row = 3, col = 2)
        key.gf <- placeGrob(key.gf, keyGrob, row = 2, col = 2)
        key.gf <- placeGrob(key.gf, keyGrob2, row = 1, col = 2)
    }

    #####################
    #bottom scale
    #positions
    #adds ticks and borders if requested    
    #####################
    else if (key$space == "bottom") {
       f.text <- f.text[as.character(f.text) != ""]
       f.text <- if(is.character(f.text)) quickText(paste(f.text, collapse="  "), f.auto.text) else
               as.expression(parse(text=paste(f.text, collapse="~~")))
       h.text <- h.text[as.character(h.text) != ""]
       h.text <- if(is.character(h.text)) quickText(paste(h.text, collapse="  "), f.auto.text) else
               as.expression(parse(text=paste(h.text, collapse="~~")))
       temp <- c(0.3,1,0,0)
       if(sum(f.slots[1:2])>0 & sum(h.slots[2:3])==0)
              temp[2:4] <- c(1,1,1.5)
       if(sum(f.slots[1:2])==0 & sum(h.slots[2:3])>0)
              temp[2:4] <- c(1,1.5,1)
       if(sum(f.slots[1:2])>0 & sum(h.slots[2:3])>0)
              temp[2:4] <- c(1,1.5,1.5)
       if(sum(f.slots[1:2])==0 & sum(h.slots[2:3])==0)
              temp[2:4] <- c(1,1,1)
       if("ticks" %in% key$plot.style)
              temp[1] <- 0.6
       labelsGrob <- textGrob(label = labels, y = rep(0, length(at)), 
            x = at, vp = viewport(xscale = atrange), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 0) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
        keyGrob <- textGrob(label = h.text, y = c(0), 
            x = c(0.5), vp = viewport(xscale = c(0,1)), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 0) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
        keyGrob2 <- textGrob(label = f.text, y = c(0), 
            x = c(0.5), vp = viewport(xscale = c(0,1)), default.units = "native", 
            check.overlap = check.overlap, just = if (rot == 0) 
                c("center", "bottom")
            else c("left", "center"), rot = rot, gp = gpar(col = col, 
                cex = cex, fontfamily = fontfamily, fontface = lattice:::chooseFace(fontface, 
                  font)))
        widths.x <- c((1 - key$height)/2, key$height, (1 - key$height)/2)
        widths.units <- rep("null", 3)
        heights.x <- c(0.6 * key$width, temp[1], temp[2], temp[3], temp[4])
        heights.units <- c("lines", "lines", "grobheight", "grobheight", "grobheight")
        heights.data <- list(NULL, NULL, labelsGrob, keyGrob, keyGrob2)
        key.layout <- grid.layout(nrow = 5, ncol = 3, heights = unit(heights.x, 
            heights.units, data = heights.data), widths = unit(widths.x, 
            widths.units), respect = TRUE)
        key.gf <- frameGrob(layout = key.layout, vp = vp)
        key.gf <- placeGrob(key.gf, rectGrob(y = rep(0.5, length(reccentre)), 
            x = reccentre, default.units = "native", vp = viewport(xscale = atrange), 
            width = recdim, height = recwd, gp = gpar(fill = key$col, col = "transparent", 
                alpha = key$alpha)), row = 1, col = 2)
       if("ticks" %in% key$plot.style)
            key.gf <- placeGrob(frame = key.gf, segmentsGrob(y0 = rep(1, 
                length(labscat)), x0 = labscat, y1 = rep(0.6, length(labscat)), 
                x1 = labscat, vp = viewport(xscale = atrange), default.units = "native", 
                gp = gpar(col = axis.line$col, lty = axis.line$lty, 
                    lwd = axis.line$lwd)), row = 2, col = 2)
       if("border" %in% key$plot.style)
            key.gf <- placeGrob(frame = key.gf, rectGrob(gp = gpar(col = axis.line$col, 
                lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, 
                fill = "transparent")), row = 1, col = 2)
        key.gf <- placeGrob(key.gf, labelsGrob, row = 3, col = 2)
        key.gf <- placeGrob(key.gf, keyGrob, row = 4, col = 2)
        key.gf <- placeGrob(key.gf, keyGrob2, row = 5, col = 2)
    }

    ##############
    #outputs    
    ##############
    if (draw) 
        grid.draw(key.gf)
    key.gf
}
