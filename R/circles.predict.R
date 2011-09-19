# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : Febrary 2010
# Version 0.1
# Licence GPL v3


setMethod('predict', signature(object='CirclesRange'), 
	function(object, x, ext=NULL, filename='', mask=FALSE, progress='text', ...) {
	
		if ( extends(class(x), 'Raster'))  {
			if (! mask) {
				x = raster(x)
			}
			if (! is.null(ext)) { 
				x = crop(x, ext) 
			}
			xx <- rasterize(object@polygons, raster(x), field=-1, fun='sum', mask=FALSE, update=FALSE, updateValue="NA", getCover=FALSE, silent=TRUE, progress=progress)
			if (mask) {
				xx <- mask(xx, x)
			}
			#nc <- length(object@polygons@polygons) 
			#fun <- function(x){x / nc }
			#xx <- calc(xx, fun=fun, filename=filename, progress=progress, ...)
			return(xx)
		} else {
			if (! inherits(x, 'SpatialPoints') )  {
				x = data.frame(x[,1:2])
				colnames(x) = c('x', 'y')
				coordinates(x) = ~ x + y
			}
			#v <- .pointsInPolygons(x, object@polygons, sum) / length(object@polygons@polygons) 
			v <- .pointsInPolygons(x, object@polygons, sum) 
			return(v)
		}
	}
)

