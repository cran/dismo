# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3




.avgDist <- function(xy, lonlat=TRUE, r=6378137) {
	xy <- unique(xy)
	xy <- as.matrix(xy[,1:2])
	d <- matrix(nrow=nrow(xy), ncol=nrow(xy))
	if (lonlat) {
		for (i in 1:nrow(xy)) { 
			d[,i] <- pointDistance(xy[i,], xy, longlat=TRUE, r=r)
		}
	} else {
		for (i in 1:nrow(xy)) { 
			d[,i] <- pointDistance(xy[i,], xy, type='Euclidean', r=r)
		}
	}
	mean(apply(d, 1, median))
}


.generateCircles <- function(xy, d, n=360, lonlat=TRUE, r=6378137 ) {
	if (missing(d)) {
		d <- .avgDist(xy, lonlat=lonlat, r=r) / 2
	}
	xy <- as.matrix(xy[,1:2])
	n <- max(4, round(n))
	toRad <- pi/180
	brng <- 1:n * 360/n
	brng <- brng * toRad
	if (lonlat) { xy = xy * toRad }
	pols <- list()
	for (i in 1:nrow(xy)) {
		p <- xy[i, ]
		if (lonlat) {
			lon1 <- p[1]
			lat1 <- p[2]
			lat2 <- asin(sin(lat1) * cos(d/r) + cos(lat1) * sin(d/r) * cos(brng))
			lon2 <- lon1 + atan2(sin(brng) * sin(d/r) * cos(lat1), cos(d/r) - sin(lat1) * sin(lat2))
			lon2 <- (lon2 + pi)%%(2 * pi) - pi
			lon2[is.nan(lon2)] <- NA
			lat2[is.nan(lat2)] <- NA
			res <- cbind(lon2, lat2)/toRad
			colnames(res) <- c("lon", "lat")
		} else {
			x2 <- p[1] + d * cos(brng)
			y2 <- p[2] + d * sin(brng)
			res <- cbind(x2, y2)
			colnames(res) <- c("x", "y")
		}
		res <- rbind(res, res[1,])
		pols <- c(pols, Polygons(list(Polygon( res )), i))		
	}
	return( SpatialPolygons( pols ) )
}


setClass('CirclesRange',
	contains = 'DistModel',
	representation (
		circles='SpatialPolygons'
	),	
	prototype (	
	),
	validity = function(object)	{
		return(TRUE)
	}
)


if (!isGeneric("circles")) {
	setGeneric("circles", function(p, ...)
		standardGeneric("circles"))
}	


setMethod('circles', signature(p='data.frame'), 
	function(p, d, lonlat, ...) {
		ci <- new('CirclesRange')
		ci@presence <- p
		ci@circles <- .generateCircles(p, d=d, lonlat=lonlat)
		return(ci)
	}
)


setMethod('circles', signature(p='matrix'), 
	function(p, lonlat, ...) {
		circles(as.data.frame(p), lonlat=lonlat, ...)
	}
)

setMethod('circles', signature(p='SpatialPoints'), 
	function(p, lonlat, ...) {
		if (missing(lonlat)) lonlat <- isLonLat(p)
		circles(coordinates(p), lonlat=lonlat, ...)
	}
)

