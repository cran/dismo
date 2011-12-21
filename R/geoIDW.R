# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.0
# Licence GPL v3


setClass('InvDistWeightModel',
	contains = 'DistModel',
	representation (
		model ='list'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)

if (!isGeneric("geoIDW")) {
	setGeneric("geoIDW", function(p, a, ...)
		standardGeneric("geoIDW"))
}	

setMethod('geoIDW', signature(p='matrix', a='matrix'), 
	function(p, a, ...) {
		v <- new('InvDistWeightModel')
		v@model <- list( .idw(p[,1:2,drop=FALSE], a[,1:2,drop=FALSE]) )
		return(v)
	}
)

setMethod('geoIDW', signature(p='data.frame', a='data.frame'), 
	function(p, a, ...) {
		geoIDW(as.matrix(p), as.matrix(a), ...)
	}
)


setMethod('geoIDW', signature(p='SpatialPoints', a='SpatialPoints'), 
	function(p, a, ...) {
		geoIDW(coordinates(p), coordinates(a), ...)
	}
)


# adapted from code by Carson Farmer
# http://www.carsonfarmer.com/?p=455
.idw <- function(p, a){
	if (!require(gstat)) { stop('you need to first install the "gstat" package') }

	rownames(p) <- NULL
	rownames(a) <- NULL
	xy <- rbind(p,a)
	pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))
	paxy <- unique(cbind(pa, xy)) 
	paxy[duplicated(paxy[, 2:3]),1] = 1  # duplicates are present
	paxy = data.frame(unique(paxy))
	colnames(paxy) = c('pa', 'x', 'y')
	
## inverse distance weighted interpolation with gstat
	gs <- gstat(id = "pa", formula = pa~1, locations = ~x+y, data=paxy, nmax=7)
	return(gs)
}

