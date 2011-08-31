# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : April 2010
# Version 0.1
# Licence GPL v3


setMethod('predict', signature(object='VoronoiHull'), 
	function(object, x, ext=NULL, filename='', mask=FALSE, progress='text', ...) {
	
		if ( extends(class(x), 'Raster'))  {
			if (! mask) {
				x <- raster(x)
			}
			if (! is.null(ext)) { 
				x <- crop(x, ext) 
			}
			
			xx <- rasterize(object@polygons, raster(x), field=1, fun='max', mask=FALSE, update=FALSE, getCover=FALSE, silent=TRUE, progress=progress)
			if (mask) {
				xx <- mask(xx, x)
			}
			return(xx)
			
		} else {
		
			if (! inherits(x, 'SpatialPoints') )  {
				x <- data.frame(x[,1:2])
				colnames(x) = c('x', 'y')
				coordinates(x) = ~ x + y
			}
			
			v <- .pointsInPolygons(x, object@polygons) 			
			return(v)
		}
	}
)

