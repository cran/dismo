
.circularArea <- function(p, radius, lonlat=FALSE) {
	require(rgeos)
	x <- circles(p, d=radius, lonlat=lonlat)
	if (lonlat) {
	
	} else {
		rgeos::gArea(x@polygons) / 1000000
	}
}
