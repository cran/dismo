# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3


setClass('GeographicDistance',
	contains = 'DistModel',
	representation (
		lonlat='logical'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("geoDist")) {
	setGeneric("geoDist", function(p, ...)
		standardGeneric("geoDist"))
}	


setMethod('geoDist', signature(p='data.frame'), 
	function(p, a, lonlat, ...) {
		gd <- new('GeographicDistance')
		gd@presence <- p
		if (! missing(a)) {
			gd@absence <- a
		}
		gd@lonlat <- lonlat
		return(gd)
	}
)


setMethod('geoDist', signature(p='matrix'), 
	function(p, a, lonlat, ...) {
		p <- as.data.frame(p)
		if (missing(a)) { 
			geoDist(p, lonlat=lonlat, ...) 
		} else {
			geoDist(p, a=as.matrix(a), lonlat=lonlat, ...)
		}
	}
)

setMethod('geoDist', signature(p='SpatialPoints'), 
	function(p, a, lonlat, ...) {
		if (missing(lonlat)) {
			lonlat <- isLonLat(p)
		}
		p <- coordinates(p)
		if (missing(a)) { 
			geoDist(p, lonlat=lonlat, ...) 
		} else {
			geoDist(p, a=coordinates(a), lonlat=lonlat, ...)
		}
	}
)

