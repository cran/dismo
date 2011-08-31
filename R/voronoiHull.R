# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.0
# Licence GPL v3


setClass('VoronoiHull',
	contains = 'DistModel',
	representation (
		polygons ='SpatialPolygonsDataFrame'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)

if (!isGeneric("voronoiHull")) {
	setGeneric("voronoiHull", function(p, a, ...)
		standardGeneric("voronoiHull"))
}	

setMethod('voronoiHull', signature(p='matrix', a='matrix'), 
	function(p, a, ...) {
		v <- new('VoronoiHull')
		v@polygons <- .voronoi(p[,1:2,drop=FALSE], a[,1:2,drop=FALSE])
		v@presence <- data.frame(p)
		v@absence <- data.frame(a)
		return(v)
	}
)

setMethod('voronoiHull', signature(p='data.frame', a='data.frame'), 
	function(p, a, ...) {
		voronoiHull(as.matrix(p), as.matrix(a), ...)
	}
)


setMethod('voronoiHull', signature(p='SpatialPoints', a='SpatialPoints'), 
	function(p, a, ...) {
		voronoiHull(coordinates(p), coordinates(a), ...)
	}
)


# adapted from code by Carson Farmer
# http://www.carsonfarmer.com/?p=455
.voronoi <- function(p, a){

	if (!require(deldir)) { stop('you need to first install the "deldir" package') }

	xy = rbind(p,a)
	pa = c(rep(1, nrow(p)), rep(0, nrow(a)))
	paxy = unique(cbind(pa, xy)) 
	paxy[duplicated(paxy[, 2:3]),1] = 1  # duplicates are present
	paxy = unique(paxy)
	
	z <- deldir(xy[,1], xy[,2])
	w <- tile.list(z)
	polys <- vector(mode='list', length=length(w))

	for (i in seq(along=polys)) {
		pcrds <- cbind(w[[i]]$x, w[[i]]$y)
		pcrds <- rbind(pcrds, pcrds[1,])
		polys[[i]] <- Polygons(list(Polygon(pcrds)), as.character(i))
	}
	
	polys <- SpatialPolygons(polys)
	
	#if (require(rgeos)) {
	#	p <- polys[pa==1]
	#	a <- polys[pa==0]
	#	p <- gUnionCascaded(p)
	#	a <- gUnionCascaded(a)
	#	a@polygons[[1]]@ID = "2"
	#	polys <- SpatialPolygons(list(p@polygons[[1]], a@polygons[[1]]))
	#	pa <- data.frame(pa=c(1,0))
	#	polys <- SpatialPolygonsDataFrame(polys, data=data.frame(pa))
	#} else {
		polys <- SpatialPolygonsDataFrame(polys, data=data.frame(pa))
	#}
	
	return(polys)
}


setMethod("plot", signature(x='VoronoiHull', y='missing'), 
	function(x, ...) {
		plot(x@polygons[x@polygons@data[,1]==1, ], ...)
	}
)
