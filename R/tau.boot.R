# TODO: Add comment
# 
# Author: tradcc
###############################################################################


tau.boot <- function(y){
#
#   Compute Kendall's tau plus a 1-alpha confidence interval
#   using the method recommended by Long and Cliff (1997).
#
	x <- 1: length(y)
	xdif <- outer(x, x, FUN = "-")
	ydif <- outer(y, y, FUN = "-")
	tv <- sign(xdif) * sign(ydif)
	dbar <- apply(tv, 1, mean)
	n <- length(x)
	tau <- sum(tv) / (n * (n - 1))
	
	list(cor = tau)
}

