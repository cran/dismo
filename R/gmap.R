# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  July 2010
# Version 1.0
# Licence GPL v3

# Based on functions in R package 'RgoogleMaps' 
# by Markus Loecher, Sense Networks <markus at sensenetworks.com>

gmap <- function (x, exp=1, type='terrain', filename='', ...) {

	if (! raster:::.requireRgdal() ) { stop('rgdal not available') }
	
	if (! type %in% c('roadmap', 'satellite', 'hybrid', 'terrain')) {
		warning("type should be: roadmap, satellite, hybrid, or terrain.") 
	}

	mxzoom <- function (latrange, lonrange, size = c(640, 640)) {
	# function from in R package 'RgoogleMaps' 
	# by Markus Loecher, Sense Networks <markus at sensenetworks.com>
		SinPhi = sin(latrange * pi/180)
		normX = lonrange/180
		normY = (0.5 * log(abs((1 + SinPhi)/(1 - SinPhi))))/pi
		MaxZoom.lon <- floor(1 + log2(abs(size[1]/256/diff(normX))))
		MaxZoom.lat <- floor(1 + log2(abs(size[2]/256/diff(normY))))
		return(c(MaxZoom.lat = MaxZoom.lat, MaxZoom.lon = MaxZoom.lon))
	}

	ll2XY <- function (lat, lon, zoom) {
	# function from in R package 'RgoogleMaps' 
	# by Markus Loecher, Sense Networks <markus at sensenetworks.com>
		SinPhi = sin(lat * pi/180)
		normX = lon/180
		normY = (0.5 * log((1 + SinPhi)/(1 - SinPhi)))/pi
		Y = (2^zoom) * ((1 - normY)/2)
		X = (2^zoom) * ((normX + 1)/2)
		x = 256 * (X - floor(X))
		y = 256 * (Y - floor(Y))
		return(list(Tile = cbind(X = floor(X), Y = floor(Y)), Coords = cbind(x = x,  y = y)))
	}

	xy2ll <- function (MyMap, X, Y) {
	# function from in R package 'RgoogleMaps' 
	# by Markus Loecher, Sense Networks <markus at sensenetworks.com>
		lat.center <- MyMap[[1]]
		lon.center <- MyMap[[2]]
		zoom <- MyMap[[3]]
		mycenter <- ll2XY(lat.center, lon.center, zoom)
		x <- mycenter$Tile[, "X"] + (X + mycenter$Coords[, "x"])/256
		y <- mycenter$Tile[, "Y"] - (Y - mycenter$Coords[, "y"])/256
		ytilde <- 1 - y/2^(zoom - 1)
		yy = (exp(2 * pi * ytilde) - 1)/(exp(2 * pi * ytilde) + 1)
		ShiftLat <- function(yy) {
			n = c(-1, 0, 1)
			lat = 2 * pi * (n) + asin(yy)
			lat <- lat[which(lat <= pi/2 & lat > -pi/2)]
			lat <- 180 * lat/pi
			return(lat)
		}
		lat <- sapply(yy, ShiftLat)
		lon = 180 * (x/2^(zoom - 1) - 1)
		return(cbind(lat = lat, lon = lon))
	}

	tile2r <- function (points, center) {
	# function from in R package 'RgoogleMaps' 
	# by Markus Loecher, Sense Networks <markus at sensenetworks.com>
		X <- 256 * (points$Tile[, "X"] - center$Tile[, "X"]) + (points$Coords[, "x"] - center$Coords[, "x"])
		Y <- -256 * (points$Tile[, "Y"] - center$Tile[, "Y"]) - (points$Coords[, "y"] - center$Coords[, "y"])
		return(list(X = X, Y = Y))
	}

	gurl <- "http://maps.google.com/staticmap?"
		
	

		prj <- projection(x, asText=TRUE)
		if ( isLonLat(prj) ) {
			x <- extent(x)
		} else {
			if ( prj == "NA" ) {
				bb <- extent(x)
				extLL <- (bb@xmin > -366 & bb@xmax < 366 & bb@ymin > -90.1 & bb@ymax < 90.1) 
				if (extLL) {
					x <- bb
				} else {
				# warning('CRS is unknown, and does not look like Lon/Lat, assuming it is Mercator')
					rad <- 6378137	
					p <- t(bbox(x)) 
					p[, 2] <- pi/2 - 2 * atan(exp(-p[, 2]/rad))
					p[, 1] <- p[, 1]/rad
					p <- p / (pi/180)
					x <- extent(p[1,1], p[2,1], p[1,2], p[2,2])
				}
			} else {
				x <- extent( projectExtent(x, "+proj=longlat +datum=WGS84") )
			}
		} 
		e <- x * exp
		e@xmin <- max(-180, e@xmin)
		e@xmax <- min(180, e@xmax)
		e@ymax <- min(89, e@ymax)
		e@ymin <- max(-89, e@ymin)
		
		lonR <- c(e@xmin, e@xmax)
		latR <- c(e@ymin, e@ymax)

		size <- c(640, 640)
		zoom <- min(mxzoom(latR, lonR, size))
		center <- c(mean(latR), mean(lonR))
 	
		ll <- ll2XY(latR[1], lonR[1], zoom)
		ur <- ll2XY(latR[2], lonR[2], zoom)
		cr <- ll2XY(center[1], center[2], zoom)
		ll.Rcoords <- tile2r(ll, cr)
		ur.Rcoords <- tile2r(ur, cr)
		size[1] <- 2 * max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X)))) +   1
		size[2] <- 2 * max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y)))) +   1

		if (length(size) < 2) {
			s <- paste(size, size, sep = "x")
		} else {
			s <- paste(size, collapse = "x")
		}

		ctr <- paste(center, collapse = ",")
	
		gurl <- paste(gurl, "center=", ctr, "&zoom=", zoom, "&size=", s, "&maptype=", type, "&format=gif", "&sensor=false", sep = "")
	
	
	if (trim(filename) == '') filename <- rasterTmpFile()
	ext(filename) <- 'gif'
	download.file(gurl, filename, mode = "wb", quiet = TRUE)
    
	MyMap <- list(lat.center = center[1], lon.center = center[2], zoom = zoom)
	bb <- list(ll = xy2ll(MyMap, X = -size[1]/2 + 0.5, Y = -size[2]/2 - 0.5), ur = xy2ll(MyMap, X = size[1]/2 +  0.5, Y = size[2]/2 - 0.5))

	r <- raster(filename)
	ext <- extent(bb$ll[2], bb$ur[2], bb$ll[1], bb$ur[1])
	p <- t(bbox(raster(ext))) *  pi/180
	rad = 6378137	
    p[,2] <- log(tan(p[, 2]) + (1/cos(p[, 2]))) 
    p <- p * rad
	extent(r) <- extent(as.vector(p))
	projection(r) = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
	worldFile(r, extension = ".gfw") 
	ext(filename) <- 'prj'
	showWKT(projection(r), filename, morphToESRI=TRUE)
    return(r)
}

#e = extent( -121.9531 , -120.3897 , 35.36 , 36.61956 )
#r = gmap(e)
#plot(r)

#projmerc <- function(p) {
#    p <- p * pi / 180
#    p[, 2] <- log(tan(p[, 2]) + (1/cos(p[, 2])))
#    return( p * 6378137 )
#}


Mercator <- function (p, inverse = FALSE) {
#author: RH
	r = 6378137
    toRad <- pi/180
    if (inverse) {
        p <- .pointsToMatrix(p, checkLonLat = FALSE)
        p[, 2] <- pi/2 - 2 * atan(exp(-p[, 2]/r))
        p[, 1] <- p[, 1]/r
        colnames(p) <- c("lon", "lat")
        return(p/toRad)
    }
    else {
        p <- .pointsToMatrix(p) * toRad
        p[, 2] <- log(tan(p[, 2]) + (1/cos(p[, 2])))
        p <- p * r
        colnames(p) <- c("x", "y")
        return(p)
    }
}

