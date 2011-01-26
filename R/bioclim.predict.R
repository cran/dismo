# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	

setMethod('predict', signature(object='Bioclim'), 
function(object, x, tails=NULL, ext=NULL, filename='', progress='text', ...) {

	percRank <- function(x, y, tail) {
		x <- sort(as.vector(na.omit(x)))
		y <- data.frame(y)
		b <- apply(y, 1, FUN=function(z)sum(x<z))
		t <- apply(y, 1, FUN=function(z)sum(x==z))
		r <- (b + 0.5 * t)/length(x)
		i <- which(r > 0.5)
		if (tail=='both') {
			r[i] <- 1-r[i]
		} else if (tail == 'high') {
			r[which(r < 0.5)] <- 0.5
			r[i] <- 1-r[i]			
		} else { # if tail == low
			r[i] <- 0.5
		}
		r * 2
	}

	ln <- colnames(object@presence)

	if (is.null(tails) ) {
		tails <- rep('both', times=length(ln))
	} else {
		if (length(tails) != length(ln)) {
			stop('length of "tails" is: ', length(tails), '.\nThis does not match the number of variables in the model which is: ', length(ln))
		}
		test <- all(tails %in% c('low', 'high', 'both'))
		if (!test) stop('"tails" should be a character vector with values "low", "high", and/or "both"')
	}
	 
	
	if (! (extends(class(x), 'Raster')) ) {
		if (! all(colnames(object@presence) %in% colnames(x)) ) {
			stop('missing variables in x ')
		}
		bc <- matrix(ncol=length(ln), nrow=nrow(x))
		
		for (i in 1:ncol(bc)) {
			bc[,i] <- percRank(object@presence[,ln[i]], x[,ln[i]], tails[i])
		}
		return( apply(bc, 1, min) )

	} else {
		out <- raster(x)
		if (! is.null(ext)) {
			out <- crop(out, ext)
			firstrow <- rowFromY(x, yFromRow(out, 1))
			firstcol <- colFromX(x, xFromCol(out, 1))
		} else {
			firstrow <- 1
			firstcol <- 1
		}
		ncols <- ncol(out)
		
		if (! all(colnames(object@presence) %in% layerNames(x)) ) {
			stop('missing variables in Raster object')
		}
		
		if (canProcessInMemory(out, 2)) {
			inmem=TRUE
			v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
		} else {
			inmem <- FALSE
			if  (filename == '') {
				filename <- rasterTmpFile()
				if (getOption('verbose')) { cat('writing raster to:', filename)	}		
				out <- writeStart(out, filename, ...)
			}
		}

		tr <- blockSize(out, n=nlayers(x)+2)
		pb <- pbCreate(tr$n, type=progress)	
		
		mincomp <- object@min
		mincomp[tails=='high'] <- -Inf
		maxcomp <- object@max
		maxcomp[tails=='low'] <- Inf
		
		for (i in 1:tr$n) {
			rr <- firstrow + tr$row[i] - 1
			vals <- getValuesBlock(x, row=rr, nrows=tr$nrows[i], firstcol, ncols)
			bc <- matrix(0, ncol=ncol(vals), nrow=nrow(vals))
			na <- as.vector(attr(na.omit(vals), 'na.action'))
			bc[na] <- NA
			k <- (apply(t(vals) >= mincomp, 2, all) & apply(t(vals) <= maxcomp, 2, all))
			k[is.na(k)] <- FALSE
			for (j in 1:length(ln)) {
				bc[k,j] <- percRank( object@presence[ ,ln[j]], vals[k, ln[j]], tails[j] )
			}

			res <- apply(bc, 1, min)
			if (inmem) {
				res <- matrix(res, nrow=ncols)
				cols = tr$row[i]:(tr$row[i]+dim(res)[2]-1)
				v[ , cols] <- res
			} else {
				out <- writeValues(out, res, tr$row[i])
			}
			pbStep(pb, i) 
		} 
		if (inmem) {
			out <- setValues(out, as.vector(v))
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
		} else {
			out <- writeStop(out)
		}
		pbClose(pb)
		return(out)
	}
}
)


