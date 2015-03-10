###############################################################################
# Description: Add comment
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 26, 2015
###############################################################################


#' Augmented Inverse Probability Treatment Weighted (A-IPTW) Estimator
#' 
#' \code{aiptw} Returns the estimate of E Y_a using the efficient estimator developed by Robins et al.
#' 
#' Requires that both g_a and Q_aw be fit manually outside of the function.
#' 
#' @param abar Treatment of interest
#' @param a Indicator of treatment assignment
#' @param g_a Estimate of g(A|W)
#' @param Q_aw Estimate of Q(Y|A,W)
#' @param y Indicator of outcome observed
#' @return \code{aiptw} returns the A-IPTW estimate.
#' 
#' @export
aiptw = function(abar, a, g_a, Q_aw, y) {
	eq = ifelse(a==abar,1,0)/g_a * (y - Q_aw) + Q_aw
	return(mean(eq))
}

