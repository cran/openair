# TODO: Add comment
# 
# Author: Rand Wilcox
###############################################################################


tsp1reg <- function(x, y, plotit = FALSE){
#
# Compute the Theil-Sen regression estimator.
# Only a single predictor is allowed in this version
#
	temp <- matrix(c(x,y), ncol = 2)
	temp <- elimna(temp)     # Remove any pairs with missing values
	x <- temp[,1]
	y <- temp[,2]
	ord <- order(x)
	xs <- x[ord]
	ys <- y[ord]
	vec1 <- outer(ys, ys, "-")
	vec2 <- outer(xs, xs, "-")
	v1 <- vec1[vec2 > 0]
	v2 <- vec2[vec2 > 0]
	slope <- median(v1 / v2)
	coef <- median(y) - slope * median(x)
	names(coef) <- "Intercept"
	coef <-c(coef, slope)
	if(plotit){
		plot(x, y, xlab="X", ylab="Y")
		abline(coef)
	}
	res <- y - slope * x - coef[1]
	list(coef = coef, residuals = res)
}
