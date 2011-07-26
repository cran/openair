# TODO: Add comment
# 
# Author: Rand Wilcox
###############################################################################


tau <- function(x, y, alpha = 0.05){
#
#   Compute Kendall's tau plus a 1-alpha confidence interval
#   using the method recommended by Long and Cliff (1997).
#
	xdif <- outer(x, x, FUN = "-")
	ydif <- outer(y, y, FUN = "-")
	tv <- sign(xdif) * sign(ydif)
	dbar <- apply(tv, 1, mean)
	n <- length(x)
	tau <- sum(tv) / (n * (n - 1))
	A <- sum((dbar - tau)^2) / (n - 1)
	B <- (n * (n - 1) * (-1) * tau^2 + sum(tv^2)) / (n^2 - n - 1)
	C <- (4 * (n - 2) * A + 2 * B)/(n * (n - 1))
	crit <- qnorm(alpha / 2)
	cilow <- tau + crit * sqrt(C)
	cihi <- tau - crit * sqrt(C)
	test <- tau / sqrt((2 * (2 * n + 5))/(9 * n * (n - 1)))
	siglevel <- 2 * (1 - pnorm(abs(test)))
	list(cor = tau, ci = c(cilow, cihi), siglevel = siglevel)
}
