# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.1
# Licence GPL v3


shapefile <- function(filename, verbose=FALSE) {
	if (!(require(rgdal))) {
		stop('This function requires the rgdal package; please install it')
	}
	fn <- basename(filename) 
	extension(fn) <- ''
	readOGR(dirname(filename), fn, stringsAsFactors=FALSE, verbose=verbose) 
}

