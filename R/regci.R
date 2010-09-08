# TODO: Add comment
# 
# Author: Rand Wilcox and DCC (block bootstrap)
###############################################################################


regci <- function(x, y, regfun = tsp1reg, nboot = 599, alpha = 0.05, SEED = TRUE,
		pr = TRUE, xout = FALSE, outfun = out, autocor = autocor,...){
#
#   Compute a .95 confidence interval for each of the parameters of
#   a linear regression equation. The default regression method is
#   the Theil-Sen estimator.
#
#   When using the least squares estimator, and when n<250, use
#   lsfitci instead.
#
#   The predictor values are assumed to be in the n by p matrix x.
#   The default number of bootstrap samples is nboot=599
#
#   regfun can be any s-plus function that returns the coefficients in
#   the vector regfun$coef, the first element of which contains the
#   estimated intercept, the second element contains the estimated of
#   the first predictor, etc.
#
	x <- as.matrix(x)
	p1 <- ncol(x) + 1
	p <- ncol(x)
	xy <- cbind(x, y)
	xy <- elimna(xy)
	x <- xy[, 1:p]
	y <- xy[, p1]
	if(xout){
		m <- cbind(x,y)
		flag <-outfun(x, plotit = FALSE)$keep
		m <- m[flag, ]
		x <- m[, 1:p]
		y <- m[, p1]
	}
	x <- as.matrix(x)
	if(SEED)set.seed(2) # set seed of random number generator so that
#             results can be duplicated.
	if(pr)print("Taking bootstrap samples. Please wait...")
	#data <- matrix(sample(length(y), size = length(y) * nboot, replace = TRUE), 
	#		nrow = nboot)
	#length of block set to l^(1/3)
	# Buhlmann and Kunsch 1994 report
	block.length <- 1
	if(autocor) block.length <- round(length(y)^(1/3))
	data <- samp.boot.block(length(y), nboot, block.length)
	
	#bvec <- apply(data, 1, regboot, x, y, regfun,...)
	bvec <- apply(data, 2, regboot, x, y, regfun,...)
# bvec is a p+1 by nboot matrix. The first row
#                     contains the bootstrap intercepts, the second row
#                     contains the bootstrap values for first predictor, etc.
	regci <- matrix(0, p1, 2)
	VAL <- c("intercept", rep("X", ncol(x)))
	dimnames(regci) <- list(VAL, c("ci.low", "ci.up"))
	ilow <- round((alpha / 2) * nboot)
	ihi <- nboot - ilow
	ilow <- ilow + 1
	se <- NA
	pvec <- NA
	for(i in 1:p1){
		bsort <- sort(bvec[i, ])
		# DCC added na.rm
		pvec[i] <- (sum(bvec[i, ] < 0, na.rm = TRUE) + 0.5 * sum(bvec[i, ] == 0, na.rm = TRUE)) / nboot
		if(pvec[i] > 0.5)pvec[i] <- 1-pvec[i]
		regci[i, 1] <- bsort[ilow]
		regci[i, 2] <- bsort[ihi]
		se[i] <- sqrt(var(bvec[i, ]))
	}
	pvec <- 2 * pvec
	if(pr){
		#print("First row of regci is the confidence interval for the intercept,")
		#print("the second row is the confidence interval for the first slope, etc.")
	}
	list(regci = regci, p.value = pvec, se = se)
}
