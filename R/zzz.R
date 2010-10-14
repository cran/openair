.onLoad <- function (...) {

open.info <- system.file("Meta", "package.rds", package = "openair") 
if (!nzchar(open.info)) 
    stop("'openair' is incorrectly installed. Please reinstall.", domain = NA)
open.info <- .readRDS(open.info)

all.pcks <- c(names(open.info$Depends),names(open.info$Imports))
   all.pcks <- unique(all.pcks)

ins.pcks <- .packages(all.available = TRUE)

open.test <- 0

#packages loaded as library
req.pcks <- c("proto", "grid", "plyr", "reshape",  "lattice", "RColorBrewer")
    for (pck in req.pcks) {
        if(pck %in% ins.pcks) {
            suppressWarnings(require(pck, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
        } else {
            message(paste("\nWARNING unable to load required package '", pck, "'", sep = ""))
            message("\t'openair' may not run correctly without this")
            message("\trecommend (re)installing")  
            open.test <- open.test + 1
        }
    }    

#rest loaded as namespaces in openair
req.pcks <- all.pcks[!all.pcks %in% "proto"]
   for (pck in req.pcks) {
       if(pck %in% ins.pcks & any(packageHasNamespace(pck, .libPaths()))) {
           suppressWarnings(namespaceImport("openair", pck))
       } else {
           message(paste("\nWARNING unable to namespace required package '", pck, "'", sep = ""))
           message("\t'openair' may not run correctly without this")
           message("\trecommend (re)installing")
           open.test <- open.test + 1          
       }
   }    

message(paste("\nThis is openair version ", packageDescription("openair", field = "Version")), sep = "")

if(open.test==0){
  message("\topenair and dependents correctly installed \n")
} else {
  message("\topenair only managed a partial install, \n\tso it may not work correctly.")
  r1 <- paste("\t", open.test, " problem", sep="")
  if(open.test>1) r1 <- paste(r1, "s", sep="")
  if(open.test<2) r1 <- paste(r1, " was", sep="") else r1 <- paste(r1, " were", sep="")
  message(paste(r1, " encountered adding dependents", sep=""))
  r1 <- "\tPlease scroll back through list to see WARNING"
  if(open.test>1) r1 <- paste(r1, "S", sep="")
  message(r1)
}

  #message("\ntype openair.news() for recent updates")

 # message("\nPlease cite both R and openair if you find them useful.")
 # message("\ttype citation() for how to cite R")
  message("\ttype citation(\"openair\") for how to cite openair")
     
}

#openair.news <- function(){
#  message("\nrecent updates to openair include:")
#  message("\nopenair namespace introduced")
#  message("\nNEW function calendarPlot introduced")
#  message("\ttype '?calendarPlot' for details")
#  message("\nEXTRA functionality added to polarPlot")
#  message("\tplot surface uncertainity estimation")
#  message("\ttype '?polarPlot' for details")
#  message("\n")
#}  


