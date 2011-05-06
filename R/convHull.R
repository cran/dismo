# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3


setClass('ConvexHull',
	contains = 'DistModel',
	representation (
		hull='SpatialPolygonsDataFrame'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("convHull")) {
	setGeneric("convHull", function(p, ...)
		standardGeneric("convHull"))
}	


setMethod('convHull', signature(p='data.frame'), 
	function(p, n=1, ...) {
		ch <- new('ConvexHull')
		ch@presence <- p
		ch@hull <- .generateHulls(p, n)
		return(ch)
	}
)


setMethod('convHull', signature(p='matrix'), 
	function(p, ...) {
		convHull(as.data.frame(p), ...)
	}
)

setMethod('convHull', signature(p='SpatialPoints'), 
	function(p, ...) {
		convHull(coordinates(p), ...)
	}
)

