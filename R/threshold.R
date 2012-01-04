# Author: Robert J. Hijmans
# Date :  October 2012
# Version 1.0
# Licence GPL v3



if (!isGeneric("threshold")) {
	setGeneric("threshold", function(x, ...)
		standardGeneric("threshold"))
}	


setMethod('threshold', signature(x='ModelEvaluation'),
	function(x, stat='', ...) {
		r <- list()
		# maximum kappa
		r$kappa <- x@t[which.max(x@kappa)]
		# maximum sum of the sensitivity (true positive rate) and specificity (true negative rate)
		r$spec_sens <- x@t[which.max(x@TPR + x@TNR)]
		# no omission
		r$no_omission <- x@t[max(which(x@confusion[, 'fn'] == 0))]
		# etc
		
		r <- data.frame(r)
		rownames(r) <- 'thresholds'
		if (stat != '') {
			stopifnot (stat %in% c('', 'kappa', 'spec_sens', 'no_omission')) 
			r[, stat]
		} else {
			r
		}
	}
)

