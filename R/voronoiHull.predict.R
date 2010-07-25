# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : April 2010
# Version 0.1
# Licence GPL v3


setMethod('predict', signature(object='VoronoiHull'), 
	function(object, x, ext=NULL, filename='', mask=FALSE, progress='text', ...) {
	
		if ( extends(class(x), 'Raster'))  {
			if (! mask) {
				x = raster(x)
			}
			if (! is.null(ext)) { 
				x = crop(x, ext) 
			}
			
			if (mask) {
				xx = polygonsToRaster(object@hull, raster(x), field=-1, overlap='max', mask=FALSE, updateRaster=FALSE, updateValue="NA", getCover=FALSE, silent=TRUE, progress=progress)
				xx <- mask(xx, x, filename=filename, progress=progress, ...)
			} else {
				xx = polygonsToRaster(object@hull, raster(x), filename=filename, field=-1, overlap='max', mask=FALSE, updateRaster=FALSE, updateValue="NA", getCover=FALSE, silent=TRUE, progress=progress, ...)
			}
			xx <- calc(xx, fun=fun, filename=filename, progress=progress, ...)
			return(xx)
			
		} else {
		
			if (! inherits(x, 'SpatialPoints') )  {
				x = data.frame(x[,1:2])
				colnames(x) = c('x', 'y')
				coordinates(x) = ~ x + y
			}
			
			v <- .pointsInPolygons(x, object@hull[object@hull@data[,1]==1,], max) 
			
			return(v)
		}
	}
)

