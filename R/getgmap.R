

.getgmap <- function(x, zoom, type='satellite') {
	r <- raster(x)
	dim(r) <- c(10,10)
	p = as(r, 'SpatialPolygons')
	res = list()
	for (i in 1: length(row.names(p))) {
		res[[i]] = gmap(extent(p[i,]), type='satellite', scale=2)
	}

	res$tolerance=.5
	m = do.call(merge, res)
	res$tolerance=NULL

	z = sapply(res, function(x) resample(x, m, method='ngb'))
	do.call(merge, z)
}

