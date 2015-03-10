###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 26, 2015
###############################################################################


#' Inverse Probability Treatment Weighted (IPTW) Estimator
#' 
#' \code{iptw} Returns the estimate of E Y_a using the estimator popularized by Robins et al.
#' 
#' Requires that g_a be fit manually outside of the function.
#' 
#' @param abar Treatment of interest
#' @param a Indicator of treatment assignment
#' @param g_a Estimate of g(A|W)
#' @param y Indicator of outcome observed
#' @return \code{iptw} returns the IPTW estimate.
#' 
#' @export
iptw = function(abar, a, g_a, y) {
	eq = ifelse(a==abar,1,0)/g_a * y
	return(mean(eq))
}

