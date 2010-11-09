# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3


evaluateROCR <- function(model, p, a, x) {
	if (! require(ROCR)) {
		stop('ROCR package not found')
	}
	
	if (!missing(x)) {
		p <- predict(model, data.frame(xyValues(x, p)))
		a <- predict(model, data.frame(xyValues(x, a)))
	} else if (is.vector(p) & is.vector(a)) {
			# do nothing
	} else {
		p <- predict(model, data.frame(p))
		a <- predict(model, data.frame(a))
	}
	p <- na.omit(p)
	a <- na.omit(a)
	if (length(p) < 1) { stop('no valid presence (p) values') }
	if (length(a) < 1) { stop('no valid absence (a) values') }
	predictions = c(p, a)
	labels = c( rep(1, length(p)), rep(0, length(a)) )
	pred <- prediction( predictions, labels)
	return(pred)
}


evaluate <- function(p, a, model, x=NULL, tr) {
	if (!missing(x)) {
		p <- predict(model, data.frame(xyValues(x, p)))
		a <- predict(model, data.frame(xyValues(x, a)))
	} else if (is.vector(p) & is.vector(a)) {
			# do nothing
	} else {
		p <- predict(model, data.frame(p))
		a <- predict(model, data.frame(a))
	}
	p <- na.omit(p)
	a <- na.omit(a)

	if (missing(tr)) {
		pr = range(c(0,p,1))
		pr <- 1:50 * (pr[2]-pr[1])/50
		ar <- as.vector(quantile(c(p, a), 0:50/50))
		tr = sort(unique(round(c(pr,ar), 3)))
	}
	
	np <- length(p)
	na <- length(a)
	if (na == 0 | np == 0) {
		stop('cannot evaluate a model without absence and presence data that are not NA')
	}
	N <- na + np

	xc <- new('ModelEvaluation')
	xc@presence = p
	xc@absence = a
		
	mv <- wilcox.test(p, a)
	xc@pauc <- mv$p.value
	xc@auc <- as.vector(mv$statistic) / (na * np)
	cr <- cor.test(c(p,a), 	c(rep(1, length(p)), rep(0, length(a))) )
	xc@cor <- cr$estimate
	xc@pcor <- cr$p.value
	
	res <- matrix(ncol=4, nrow=length(tr))
	colnames(res) <- c('tp', 'fp', 'fn', 'tn')
	xc@t <- tr
	for (i in 1:length(tr)) {
		res[i,1] <- length(p[p>=tr[i]])  # a  true positives
		res[i,2] <- length(a[a>=tr[i]])  # b  false positives
		res[i,3] <- length(p[p<tr[i]])    # c  false negatives
		res[i,4] <- length(a[a<tr[i]])    # d  true negatives
	}
	xc@confusion = res
	a = res[,1]
	b = res[,2]
	c = res[,3]
	d = res[,4]
# after Fielding and Bell	
	xc@np <- as.integer(np)
	xc@na <- as.integer(na)
	xc@prevalence = a + c / N
	xc@ODP = b + d / N
	xc@CCR = a + d / N
	xc@TPR = a / (a + c)
	xc@TNR = d / (b + d)
	xc@FPR = b / (b + d)
	xc@FNR = c/(a + c)
	xc@PPP = a/(a + b)
	xc@NPP = d/(c + d)
	xc@MCR = (b + c)/N
	xc@OR = (a*d)/(c*b)

	prA = (a+d)/N
	prY = (a+b)/N * (a+c)/N
	prN = (c+d)/N * (b+d)/N
	prE = prY + prN
	xc@kappa = (prA - prE) / (1-prE)
	return(xc)
}




setMethod ('show' , 'ModelEvaluation', 
	function(object) {
		cat('class            :' , class(object), '\n')
		cat('n presences      :' , object@np, '\n')
		cat('n absences       :' , object@na, '\n')
		cat('AUC              :' , object@auc,'\n')
#		cat('p(AUC)      :' , object@pauc,'\n')
		cat('cor              :' , object@cor,'\n')
#		cat('p(cor)      :' , object@pcor,'\n')
#		cat('prevalence  :' , object@prevalence,'\n')
#		cat('overallDiagnosticPower :', object@overallDiagnosticPower,'\n')
#		cat('correctClassificationRate :', object@correctClassificationRate,'\n')
#		cat('sensitivity :', object@sensitivity,'\n')
#		cat('specificity :', object@specificity,'\n')
#		cat('falsePositiveRate :', object@falsePositiveRate,'\n')
#		cat('falseNegativeRate :', object@falseNegativeRate,'\n')
#		cat('PPP :', object@PPP,'\n')
#		cat('NPP :', object@NPP,'\n')
#		cat('misclassificationRate :', object@misclassificationRate,'\n')
#		cat('oddsRatio :', object@oddsRatio,'\n')
#		cat('kappa :', object@kappa,'\n')

		cat('TPR+TNR threshold:', object@t[which.max(object@TPR + object@TNR)], '\n')
	}
)	


