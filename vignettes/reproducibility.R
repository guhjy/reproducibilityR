## ----R_options, echo=FALSE, cache=FALSE, results="hide", warning=FALSE, include=FALSE----
options(scipen = 6, digits = 4)
rm(list=ls())

## ----density, echo=FALSE, include=TRUE, fig.width=9, fig.height=3, out.width='5in'----
require(reproducibility)
data(P_a)
data(results)
psi_0 = mean(P_a$y)
par(mfcol=c(1,3))
plot(density(results[,4], bw=.0075), xlim=c(0,.5), xlab="Estimate", main=paste("g-comp (variance = ", round(var(results[,4]),4), ")", sep=""))
abline(v=psi_0, col=2)
plot(density(results[,5], bw=.0075), xlim=c(0,.5), xlab="Estimate", main=paste("IPTW (variance = ", round(var(results[,5]),4), ")", sep=""))
abline(v=psi_0, col=2)
plot(density(results[,6], bw=.0075), xlim=c(0,.5), xlab="Estimate", main=paste("A-IPTW (variance = ", round(var(results[,6]),4), ")", sep=""))
abline(v=psi_0, col=2)

