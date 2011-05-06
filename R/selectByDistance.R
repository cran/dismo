

selectByDistance <- function(from, to, target, tr=0.1, lonlat=TRUE) {

	distHaversine <- function (p1, p2) {
		r <- 6378137
		toRad <- pi/180
		p1 <- p1 * toRad
		p2 <- p2 * toRad
		p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
		dLat <- (p[, 4] - p[, 2])
		dLon <- (p[, 3] - p[, 1])
		a <- sin(dLat/2) * sin(dLat/2) + cos(p[, 2]) * cos(p[, 4]) * sin(dLon/2) * sin(dLon/2)
		dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * r
		as.vector(dist)
	}

	distGeo <- function (x, y) {
		n <- nrow(x)
		m <- nrow(y)
		dm <- matrix(ncol = m, nrow = n)
		for (i in 1:n) {
			dm[i, ] <- distHaversine(x[i, ], y)
		}
		return(dm)
	}
	
	
	distPlane <- function (x, y) {
		dfun <- function(x, y) {
			sqrt( (x[,1] - y[,1])^2 + (x[,2] - y[,2])^2 )
		}
		n = nrow(x)
		m = nrow(y)
		dm = matrix(ncol = m, nrow = n)
		for (i in 1:n) {
			dm[i, ] = dfun(x[i, ], y)
		}
		return(dm)
	}
	
	if (lonlat) {
		distfun <- distGeo
	} else {
		distfun <- distPlane
	}
	
	ngb <- NULL
	fromd <- apply(distfun(from, target), 1, min)
	tod <- distfun(to, target)
	for (i in 1:nrow(from)) {
		d <- apply(tod, 1, min) 
		d <- abs(d - fromd[i])
		mn <- min(d)
		if (mn < (1+tr) * fromd[i]) {
			x <- which.min(d)
			ngb <- c(ngb, x)
			tod[x, ] <- Inf
		} else {
			ngb <- c(ngb, NA)
		}
	}
	return(ngb)
}


