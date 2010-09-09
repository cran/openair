calcFno2 <- function(input,
                      tau = 60,
                      plot = TRUE,
                      user.fno2,
                      main = "",
                      xlab = "year", ...) {
{

  
    
    ## function to prepare data ######################################################
    prepare <- function(input) {

        input$temp <- input$temp + 273

        ## different date components
        input <- input[order(input$date), ]
        year <- as.numeric(format(input$date, "%Y"))
        month <- as.numeric(format(input$date, "%m"))
        jd <- as.numeric(format(input$date, "%j"))
        hour <- as.numeric(format(input$date, "%H"))
        weekday <- format(input$date, "%A")

        nox.v <- input$nox - input$back_nox ## estimated road contribution to NOx
        SLAT = 52 ## latitute

        a <- 23.45 * (2 * pi / 360) * sin((jd + 284) * 2 * pi / 365)

        s <- sin(2 * pi * SLAT / 360) * sin(a) + cos(2 * pi * SLAT / 360) *
            cos(a) * cos((hour - 12) * 2 * pi / 24)

        solar <- (990 * s - 30) * (1 - 0.75 * (input$cl / 8) ^ 3.4)

        jno2 <- ifelse(solar < 0, 0, 0.0008 * exp(-10/solar) + 0.0000074*solar)

        k <-  0.04405*exp(-1370/input$temp)

        r <- jno2/k

        cbind(input, year, month, jd, weekday, hour, jno2, k, r, nox.v)

    }
################################################################################

    calc.error <- function(user.fno2, nox, no2, nox.v, k, r, jno2, back_no2, back_o3, type) {

        ## measured NOX-NO2 relationship
        bin1 <- cut(nox, breaks = 200)
        bin.meas <- aggregate(no2, list(bin1), mean, na.rm = TRUE)

        ## first calculate new hourly means with t, f-NO2
        d <- 1/(k * tau)
        no2.v <-  nox.v * user.fno2
        no2.n <- no2.v + back_no2
        no2.o <- no2.n + back_o3
        b <- nox + no2.o + r + d
        no2.pred <- 0.5 * (b - (b^2 - 4 * (nox * no2.o + no2.n * d)) ^ 0.5)

        o3 <- (jno2 * tau * no2.pred + back_o3)/(k * tau * (nox - no2.pred) + 1)

        ## make NOx-NO2 relationship
        bin.mod <- aggregate(no2.pred, list(bin1), mean, na.rm = TRUE)
        error <- sum(bin.meas$x - bin.mod$x, na.rm = TRUE)^2

        ## return different results depending on use
        switch(type,
               err = error,
               conc = data.frame(no2 = no2.pred, o3 = o3)
               )
    }

    ## plot results ##################################################################
    plot.fno2 <- function(results,...) {

        theplot <- scatterPlot(results, x = "date", y = "fno2", ylab = "f-NO2 (%)") 
        print(theplot)
    }

###plots orginal monthly NO2 and predicted with  ###############################
    plot.no2 <- function(input, res,...) {
        input <- subset(input, select = c(date, no2))
        input <- timeAverage(input, "month")
        input$variable <- "measured"

        res <- subset(res, select = c(date, no2))        
        res <- timeAverage(res, "month")
        res$variable <- "predicted"

        results <- rbind(input, res)

        scatterPlot(results, x = "date", y = "no2", type = "variable", group = TRUE)

    }

    ## start of code#################################################################
     ## if cl or temp are missing, fill in with default values

    ## check to see if cloud and temperature are present; if not, set default values
    if(!any(names(input) %in% "temp"))  input$temp <- 11
    if(!any(names(input) %in% "cl"))  input$cl <- 4.5

    input <- checkPrep(input, Names = c("date", "nox", "no2", "back_no2",
                               "back_nox", "back_o3", "cl", "temp"), "default")
    input <- na.omit(input)
    input.all <- prepare(input)  ## process input data
    input.all <- subset(input.all, nox.v > 0)  ## process only +ve increments

     if(missing(user.fno2)) {

        fun.opt <- function(x)  {
            optimize(calc.error, c(0, 0.5), x$nox, x$no2,
                     x$nox.v, x$k, x$r, x$jno2, x$back_no2, x$back_o3, "err")
        }
        ## split data frame by year/month
        input.part <- split(input.all, input.all[c("month", "year")])

        input.part <- input.part[which(lapply(input.part, nrow) > 50)]  #need at least 50 hours

        fno2 <- sapply(input.part, function(x) fun.opt(x)$minimum)

        dates <- lapply(input.part, function(x) format(x$date[1], "%Y-%m"))
        dates <- do.call(rbind, dates)

        dates <- as.Date(paste(dates, "-01", sep = ""))

        results <- data.frame(date = dates, fno2 = 100 * fno2) ## retrun estimates

        ## calculate O3 based on best estimate of f-no2
        results$date <- as.POSIXct(results$date, "GMT")
        input.all <- merge(input.all, results, all = TRUE)

        ## copy down f-NO2 for each month
        input.all$fno2 <- na.locf(input.all$fno2, na.rm = FALSE)
        input.all$fno2<- input.all$fno2 / 100

        ## now calculate o3
        hourly <- calc.error(input.all$fno2, input.all$nox, input.all$no2,
                             input.all$nox.v, input.all$k, input.all$r,
                             input.all$jno2, input.all$back_no2,
                             input.all$back_o3, "conc")

        ids <- which(input$date %in% input.all$date)  ## indices with data

        hourly <- cbind(date = input.all$date, nox = input.all$nox, hourly)

        gaps <- data.frame(date = input$date[-ids], nox = input$nox[-ids],
                           no2 = input$no2[-ids], o3 = NA)

        hourly <- rbind(hourly, gaps)
        hourly <- hourly[order(hourly$date), ]

        if (plot) print(plot.fno2(results,...))
        results <- list(results = results, hourly = hourly)
        invisible(results)

    } else {  ## calculate NO2 concentrations based on user input for whole series
        res <- calc.error(user.fno2, input.all$nox, input.all$no2,
                          input.all$nox.v, input.all$k, input.all$r,
                          input.all$jno2, input.all$back_no2,
                          input.all$back_o3, "conc")

        ids <- which(input$date %in% input.all$date)  ## indices with data

        res <- cbind(date = input.all$date, res)

        gaps <- data.frame(date = input$date[-ids], no2 = input$no2[-ids], o3 = NA)

        res <- rbind(res, gaps)
        res <- res[order(res$date), ]
        if (plot) print(plot.no2(input.all, res,...))
        invisible(res)
    }
}
}


