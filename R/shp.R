# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3


shapefile <- function(filename) {
	if (!(require(rgdal))) {
		stop('This function requires the rgdal package; please install it')
	}
	fn <- basename(filename) 
	ext(fn) <- ''
	vec <- readOGR(dirname(filename), fn) 
	return(vec)
}

