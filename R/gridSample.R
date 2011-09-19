# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2009
# Version 1
# Licence GPL v3


gridSample <- function(xy, r, n=1, chess='') {

	if (inherits(xy, 'Spatial')) {
		xy <- coordinates(xy)
	}
    cell = cellFromXY(r, xy)
	cell = na.omit(cell)
    uc = unique(cell)
	
	chess <- trim(chess)
	if (chess != '') {
		chess = tolower(chess)
		if (chess == 'white') {
			tf <- 1:ceiling(ncell(r)/2) * 2 - 1
		} else if (chess == 'black') {
			tf <- 1:ceiling(ncell(r)/2) * 2
		} else {
			stop("The value of 'chess' should be '', 'black' or 'white'")
		}	
		uc <- uc[uc %in% tf]
	}
	
    xy = cbind(xy, cell, runif( nrow(xy)))
    xy =  xy[order(xy[,4]), ]
    pts = matrix(nrow=0, ncol=2)
    for (u in uc) {
        ss = subset(xy, xy[,3] == u)
        pts = rbind(pts, ss[1:min(n, nrow(ss)), 1:2])
    }
    return(pts)
	
}


.pwGridSample <- function(p, a, r, rowcol=10) {

	if (inherits(p, 'SpatialPoints')) p <- coordinates(p)
	if (inherits(a, 'SpatialPoints')) a <- coordinates(a)
	p <- as.matrix(p)
	a <- as.matrix(a)

	if (missing(r)) {
		if (length(rowcol)==1) {
			rowcol <- c(rowcol, rowcol)
		} else {
			rowcol <- rowcol[1:2]
		}
		e <- extent(as.vector(apply(rbind(p, a), 2, range)))
		r <- raster(e, nrow=rowcol[1], ncol=rowcol[2])
		r <- expand(r, 1)
	}
	
	fc <- cellFromXY(r, p)
	tc <- cellFromXY(r, a)
	rc <- unique(fc)
	ac <- unique(tc)
	sel <- rc[which(rc %in% ac)]
	selp <- sela <- NULL
	
	for (i in 1:length(sel)) {
		x <- which(fc == sel[i])
		if (length(x) == 1) { 
			y <- x 
		} else { 
			y <- sample(x, 1) 
		}
		selp <- c(selp, y)
		x <- which(tc == sel[i])
		if (length(x) == 1) { 	
			y <- x 
		} else { 
			y <- sample(x, 1) 
		}
		sela <- c(sela, y)
	}
	return(list(p=selp, a=sela))
}

