###############################################################################
# Description: Implements simulation to show that the augmented IPTW is more 
#			   efficient than the more general IPTW. Also cf G-computation.
#
# Author: Linh Tran <tranlm@berkeley.edu>
# Date: Feb 25, 2015
###############################################################################

#########################
## SIMULATION APPROACH ##
#########################
#1) Generate data
#2) Implement estimator(s) being studied
#3) Gather and present results

########################
## PRACTICUM APPROACH ##
########################
#1) Load, clean, and organize data
#2) Run various models/analyses
#3) Gather and present results


#############
## OPTIONS ##
#############
setwd("~/Dropbox/06-Spring-2015/reproducibility")


##############
## PACKAGES ##
##############
load.packages = c('devtools', 'parallel', 'magrittr')
tmp = lapply(load.packages, require, character.only=T)
cat(sum(unlist(tmp)), "of", length(tmp), "packages loaded successfully\n")
load_all("~/Dropbox/06-Spring-2015/reproducibility")
#document("~/Dropbox/06-Spring-2015/reproducibility")
#use_vignette("presentation")
#check("~/Dropbox/06-Spring-2015/reproducibility")
#build("~/Dropbox/06-Spring-2015/reproducibility")
#install.packages("~/Dropbox/06-Spring-2015/reproducibility_0.0.0.9000.tar.gz", type="source", repos=NULL)


######################
## GLOBAL VARIABLES ##
######################
samp.size 	= 500
sim.size	= 1000
abar		= 1
ncores		= 8

## TRUE P ##
Q_W    = function(n) rnorm(n, mean=0, sd=0.75)
g_A    = function(w) plogis(-0.5 - 1.5*w)
Q_Y    = function(w, a) plogis(-0.75 + 1.2*w - 0.75*a)


##############
## TRUE PSI ##
##############
set.seed(1000)
P_a = generateData(n=1000000, Q_W=Q_W, g_A=g_A, Q_Y=Q_Y, abar=abar)
save(P_a, file="./data/P_a.RData")
psi_0 = mean(P_a$y)
cat("True psi_0 =", psi_0, "\n")


################
## SIMULATION ##
################
set.seed(1)
results = mclapply(1:sim.size, function(x) {

	###################
	## GENERATE DATA ##
	###################
	P_n = generateData(n=samp.size, Q_W=Q_W, g_A=g_A, Q_Y=Q_Y)
	P_na = P_n
	P_na$a = abar
	
	##########################
	## IMPLEMENT ESTIMATORS ##
	##########################
	## g(A|W) ##
	g_An_unadj = glm(a ~ 1, data=P_n, family="binomial")
	g_An_true = glm(a ~ w, data=P_n, family="binomial")
	g_abar_unadj = predict(g_An_unadj, newdata=P_na, type="response")
	g_abar_true = predict(g_An_true, newdata=P_na, type="response")
	## Q(Y|A,W) ##
	Q_aw_unadj = glm(y ~ a, data=P_n, family="binomial")
	Q_aw_true = glm(y ~ w + a, data=P_n, family="binomial")
	Q_abar_unadj = predict(Q_aw_unadj, newdata=P_na, type="response")
	Q_abar_true = predict(Q_aw_true, newdata=P_na, type="response")
	## G-COMP ##
	psi_unadj.gcomp = mean(Q_abar_unadj)
	psi_n.gcomp = mean(Q_abar_true)
	## IPTW ##
	psi_unadj.iptw = iptw(abar=abar, a=P_n$a, g_a=g_abar_unadj, y=P_n$y)
	psi_n.iptw = iptw(abar=abar, a=P_n$a, g_a=g_abar_true, y=P_n$y)
	## A-IPTW ##
	psi_unadj.aiptw = aiptw(abar=abar, a=P_n$a, g_a=g_abar_unadj, Q_aw=Q_abar_unadj, y=P_n$y)
	psi_n.aiptw = aiptw(abar=abar, a=P_n$a, g_a=g_abar_unadj, Q_aw=Q_abar_true, y=P_n$y)
	
	####################
	## RETURN RESULTS ##
	####################
	foo = c(gcomp.unadj=psi_unadj.gcomp, iptw.unadj=psi_unadj.iptw, aiptw.unadj=psi_unadj.aiptw, gcomp.true=psi_n.gcomp, iptw.true=psi_n.iptw, aiptw.true=psi_n.aiptw)
	return(foo)
	
}, mc.cores=ncores) %>% do.call("rbind", .)
save(results, file="./data/results.RData")


#####################
## PRESENT RESULTS ##
#####################
summary(results)
apply(results, 2, var)

## HISTOGRAMS ##
pdf("./vignettes/histograms.pdf")
par(mfrow=c(2,3))
hist(results[,1], xlim=c(.0,.5), xlab="Estimate", main="Unadjusted g-comp")
hist(results[,2], xlim=c(.0,.5), xlab="Estimate", main="Unadjusted IPTW")
hist(results[,3], xlim=c(.0,.5), xlab="Estimate", main="Unadjusted A-IPTW")
hist(results[,4], xlim=c(.0,.5), xlab="Estimate", main="Adjusted g-comp")
hist(results[,5], xlim=c(.0,.5), xlab="Estimate", main="Adjusted IPTW")
hist(results[,6], xlim=c(.0,.5), xlab="Estimate", main="Adjusted A-IPTW")
dev.off()


