# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2009
# Version 1
# Licence GPL v3

prepareData <- function(p, b, x, factors) {
	p <- xyValues(x, p)
	b <- xyValues(x, b)
	pb <- data.frame(  cbind(pb=c(rep(1, nrow(p)), rep(0, nrow(b))), rbind(p, b)) )
	if (!missing(factors)) {
		for (f in factors) {
			pb[,f] = factor(pb[,f])
		}
	}
	return(pb)
}

