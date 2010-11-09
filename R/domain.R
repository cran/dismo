# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3



setClass('Domain',
	contains = 'DistModel',
	representation (
		range='vector'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("domain")) {
	setGeneric("domain", function(x, p, ...)
		standardGeneric("domain"))
}	


setMethod('domain', signature(x='Raster', p='matrix'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		domain(m)
	}
)

setMethod('domain', signature(x='Raster', p='data.frame'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		
		domain(m)
	}
)

setMethod('domain', signature(x='data.frame', p='missing'), 
	function(x, p, ...) {

		for (i in ncol(x):1) {
			if (is.factor(x[,i])) {
				warning('variable "', colnames(x)[i], '" was removed because it is a factor (categorical)')
				x <- x[, -i]
			}
		}
	
		domain(as.matrix(x))
	}
)

setMethod('domain', signature(x='matrix', p='missing'), 
	function(x, p, ...) {
		
		x = na.omit(x)
		
		if (ncol(x) == 0) {	stop('no usable variables') 	}
		if (nrow(x) < 2) {	stop('insufficient records') 	}
		
		r <- apply(x, 2, FUN=function(x){range(x, na.rm=TRUE)})

		d <- new('Domain')
		d@presence <- x
		d@range <-  abs(r[2,] - r[1,])
		norange = which(d@range == 0)
		if (length(norange) > 0) {
			for (i in length(norange):1) {
				index = norange[i]
				warning('variable "', colnames(d@presence)[index], '" was removed because it has no variation for the training points')
				d@presence <- d@presence[, -index]
				d@range <- d@range[-index]
			}
		}
		if (ncol(d@presence) == 0) {
			stop('no usable variables')
		}
		d
	}
)

setMethod('domain', signature(x='Raster', p='SpatialPoints'), 
	function(x, p, ...) {
		m <- xyValues(x, p)
		domain(m)
	}
)


