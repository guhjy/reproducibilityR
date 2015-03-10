###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 25, 2015
###############################################################################


#' Generating data
#' 
#' \code{generateData} Generates data based on a user specified distribution.
#' 
#' Details
#' 
#' @param n Number of observations to generate.
#' @param Q_W Function for generating W.
#' @param g_A Function for generating A.
#' @param Q_Y Function for generating Y.
#' @param abar Whether to generate a post-intervention distribution (and if so, the specified one), or to just generate the observed distribution.
#' @return \code{generateData} returns a \code{data.frame} object, which contains realized observations from the user specified distribution.
#' 
#' @export
generateData = function(n, Q_W, g_A, Q_Y, abar=NULL) {
	w = Q_W(n)
	if(is.null(abar)) {
		a = rbinom(n, size=1, prob=g_A(w))
	} else if(length(abar==1)) {
		a = abar
	} else {
		stop("'abar' needs to be NULL or of length 1")
	}
	y = rbinom(n, size=1, prob=Q_Y(w,a))
	
	return(data.frame(w,a,y))
}
