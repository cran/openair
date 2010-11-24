#####################
#openair.generics
#####################
#S3
#

#crude def
openair <- function(x){
    class(x) <- "openair"
    x
}

is.openair <- function(x, full.test=TRUE, ...){

   #####################
   #is.openair - two level tester
   #####################
   #kr 23/10/2010 v 0.0.1

   #####################
   #to do
   #####################
   #make test more robust?

   #standard test
   output <- is(x)[1]=="openair"
   #full.test
   if(full.test & output==TRUE){
       output <- all(c("plot", "data", "call") %in% names(x))
   }
   #output
   output
}

summary.openair <- function(object, subset = "all", ...){

   #################
   #summary.openair
   #################
   #kr 03/11/2010 v 0.0.2

   #################
   #to do
   #################
   #tidy messy deadend for is.openair test
   #

   if(!is.openair(object)) return(invisible(NULL))

   message("\nopenair object created by:\n\t", deparse(object$call))

   test <- object$data$subsets

   if(is.null(test)){
      if(!is.null(subset) && subset != "all"){
          warning("In summary(...): subset option ignored,",
                  "\n\t[subset requested from openair object without data subsets]", 
                  call. = FALSE)
      }
      ans <- summary(object$data,...)
      message("")
      print(ans)
      message("")
      return(invisible(ans))
   }

   if(is.null(subset) || subset=="all") {
        message("\ncontains ", length(test), " data frame(s):")
        message("\t$data$", paste(test, collapse=", $data$", sep=", $data$"), "\n")
        temp <- test
   } else {
        temp <- subset[subset %in% test]
        if(length(temp) < 1){ 
             message("")
             stop("In summary(...): requested subset(s) not found", 
                  "\n\t[suggest one (or more) of: ", paste(test, collapse=", ", sep=", "), "]",
                  call. = FALSE)
        }

        message("\nrequested data subset(s):")
        message("\t$data$", paste(subset, collapse=", $data$", sep = ", $data$"), "\n")
        
        if(length(temp) < length(subset))
            warning("In summary(...): some requested subset(s) not found, so ignored",
                    "\n\t[ignored subset(s): ", paste(subset[subset != temp], collapse = ", ", sep = ", "),"]", 
                    "\n\t[available subset(s): ", paste(test, collapse = ", ", sep = ", "),"]", 
                    call. = FALSE)
   }

   ans <- sapply(temp, function(y){
       summary(object$data[[y]], ...) }, 
       simplify = FALSE, USE.NAMES = TRUE
   )

   #########################
   #strictly consistent structure 
   #but messy
   #########################
   ##ans <- list(data = ans)

   print(ans)
   return(invisible(ans))

}

plot.openair <- function(x, subset = "all", silent=TRUE, ...){

   #################
   #plot.openair
   #################
   #kr 03/11/2010 v 0.0.2

   #################
   #to do
   #################

   if(!is.openair(x)) return(invisible(NULL))

   if(!silent){
       message("\nopenair object created by: \n\t", deparse(x$call), "\n")
   }

   test <- x$plot$subsets

   if(is.null(test)){
      if(!is.null(subset) && subset != "all"){
          warning("In plot(...): subset option ignored,",
                  "\n\t[subset requested from openair object without data subsets]", 
                  call. = FALSE)
      }
      return(plot(x$plot))
   }


   if(is.null(subset) || subset == "all"){
      test <- x$main.plot
      if(is.null(test)){
          message("")
          stop("In plot(...): bad openair object structure", 
             "\n\t[please contact openair admin if valid]",
             call. = FALSE)
      }
      return(x$main.plot())   
   } 

   #only plot 1, 1st valid
   temp <- subset[subset %in% test]
   if(length(temp) < 1){ 
        message("")
        stop("In plot(...): requested subset not found", 
             "\n\t[suggest one of: ", paste(test, collapse=", ", sep=", "), "]",
             call. = FALSE)
   }

   if(length(temp)<length(subset)){
        warning("In plot(...): multiple subsets requested and some not found, using first valid", 
            call. = FALSE)
   } else {
       if(length(temp)>1){
            warning("In plot(...): multiple subsets requested, using first valid", 
                call. = FALSE)
       }
   } 

   temp <- temp[1]
   test <- x$ind.plot
   if(is.null(test)) return(x$plot[[temp]]) else 
       return(x$ind.plot(x$plot[[temp]]))
}

print.openair <- function(x, silent = FALSE, plot = TRUE, ...){

   #################
   #print.openair
   #################
   #kr 03/11/2010 v 0.0.2

   #################
   #to do
   #################

   if(!is.openair(x)) return(invisible(NULL)) 
      #must have call, data and plot elements

   if(!silent){

      message("\nopenair object created by:\n\t", deparse(x$call))
      
      message("\nthis contains:")
      
      test <- x$data$subsets
      if(is.null(test)){
          message("\ta single data frame:\n\t$data [with no subset structure]")
      } else {
        message("\t", length(test), " data frame(s):")
        message("\t$data$", paste(test, collapse=", $data$", sep=", $data$"))
      }

      test <- x$plot$subsets
      if(is.null(test)){
          message("\ta single plot object:\n\t$plot [with no subset structure]")
      } else {
        message("\t", length(test), " plot objects(s):")
        message("\t$plot$", paste(test, collapse=", $plot$", sep=", $plot$"), "\n")
      }

      message("") #tidy

   }
   
   if(plot) plot(x, silent = TRUE, ...)
}

names.openair <- function(x, ...){

    #stuff we own up to...
    vis.elements <- c("data", "plot", "call")
    #make names non-recursive
    class(x) <- "not-openair"
    names(x)[names(x) %in% vis.elements]

}


