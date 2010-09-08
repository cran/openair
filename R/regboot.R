# TODO: Add comment
# 
# Author: Rand Wilcox
###############################################################################


regboot <- function(isub, x, y, regfun,...){
#
#  Perform regression using x[isub] to predict y[isub]
#  isub is a vector of length n,
#  a bootstrap sample from the sequence of integers
#  1, 2, 3, ..., n
#
#  This function is used by other functions when computing
#  bootstrap estimates.
#
#  regfun is some regression method already stored in S-PLUS
#  It is assumed that regfun$coef contains the  intercept and slope
#  estimates produced by regfun.  The regression methods written for
#  this  book, plus regression functions in S-PLUS, have this property.
#
#  x is assumed to be a matrix containing values of the predictors.
#
	xmat <- matrix(x[isub,], nrow(x), ncol(x))
	vals <- regfun(xmat, y[isub],...)$coef
	vals
}
