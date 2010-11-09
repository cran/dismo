# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

setClass('Mahalanobis',
	contains = 'DistModel',
	representation (
		cov = 'matrix' 
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("mahal")) {
	setGeneric("mahal", function(x, p, ...)
		standardGeneric("mahal"))
}	

setMethod('mahal', signature(x='matrix', p='missing'), 
	function(x, p, ...) {
		m <- new('Mahalanobis')
		
		x = na.omit(x)
		if (ncol(x) == 0) {	stop('no usable variables') 	}
		if (nrow(x) < 2) {	stop('insufficient records') 	}
		
		
		m@presence <- x
		m@cov <- var(x)
		m
	}
)

setMethod('mahal', signature(x='data.frame', p='missing'), 
	function(x, p, ...) {
		for (i in ncol(x):1) {
			if (is.factor(x[,i])) {
				warning('variable "', colnames(x)[i], '" was removed because it is a factor (categorical)')
				x <- x[, -i]
			}
		}
		if (ncol(x) == 0) {	stop('no usable variables') 	}
		mahal(as.matrix(x))
	}
)

setMethod('mahal', signature(x='Raster', p='matrix'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		mahal(m)
	}
)

setMethod('mahal', signature(x='Raster', p='data.frame'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		mahal(m)
	}
)

setMethod('mahal', signature(x='Raster', p='SpatialPoints'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		mahal(m)
	}
)

