# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 1.0
# October 2010

geocode <- function(x, oneRecord=FALSE, extent=NULL, progress='') {

	if (! require(XML)) stop('You need to install the XML package to be able use this function')

	burl <- "http://maps.google.com/maps/api/geocode/xml?address="


	res <- matrix(ncol=7, nrow=0)
	colnames(res) <- c('ID', 'lon', 'lat', 'lonmin', 'lonmax', 'latmin', 'latmax')
	
	pb <- pbCreate(length(x), progress)
	for (z in 1:length(x)) {
		r <- x[z]
		r <- gsub(', ', ',', r)
		r <- gsub(' ,', ',', r)
		r <- gsub(' ', '+', r)
		if (is.null(extent)) {
			gurl <- paste(burl, r, "&sensor=false", sep="")
		} else {
			e <- extent(extent)
			extent <- paste(e@ymin,',',e@xmin,'|',e@ymax,',',e@xmax,sep='')
			gurl <- paste(burl, r, "&bounds=", extent, "&sensor=false", sep="")			
		}
		try( doc <- xmlInternalTreeParse(gurl) )
		if (class(doc)[1] == 'try-error') {
			warning('cannot parse XML document\n')
			status <- ''
		} else { 
			status <- xmlValue(getNodeSet(doc, "//GeocodeResponse//status")[[1]])
		}
		
		if (status != "OK") {
			cat(status, ':', r, '\n')
			w <- matrix(NA, ncol=ncol(res), nrow=1)
			w[1] <- z
			res <- rbind(res, w)
			next
		}
		
		p <- xmlToList(doc)
		n <- length(p)-1
		location <- matrix(ncol=2, nrow=n)
		viewport <- matrix(ncol=4, nrow=n)
		bounds <- matrix(ncol=4, nrow=n)
		for (i in 1:n) {
			location[i,] <- as.numeric(c(p[i+1]$result$geometry$location$lng, p[i+1]$result$geometry$location$lat))
			viewport[i,] <- as.numeric(c(p[i+1]$result$geometry$viewport$southwest$lng, p[i+1]$result$geometry$viewport$northeast$lng, p[i+1]$result$geometry$viewport$southwest$lat, p[i+1]$result$geometry$viewport$northeast$lat) )
			bnds <- as.numeric(c(p[i+1]$result$geometry$bounds$southwest$lng, p[i+1]$result$geometry$bounds$northeast$lng, p[i+1]$result$geometry$bounds$southwest$lat, p[i+1]$result$geometry$bounds$northeast$lat) )
			if (length(bnds)==4) bounds[i,] <- bnds
		}

		w <- cbind(viewport, bounds)
		w[,c(1,3)] <- pmin(w[,c(1,3)], w[,c(1,3)+4], na.rm=TRUE)
		w[,c(2,4)] <- pmax(w[,c(2,4)], w[,c(2,4)+4], na.rm=TRUE)
		w <- cbind(z, location, w[,1:4,drop=FALSE])
	
		if (oneRecord & nrow(w) > 1) {
			f <- apply(w[,4:7, drop=FALSE], 2, range)
			g <- apply(w[,2:3, drop=FALSE], 2, mean)
			w <- cbind(z, g[1], g[2], f[1,1], f[2,2], f[1,3], f[2,4])
		}

		res <- rbind(res, w)
		pbStep(pb, z) 
	} 
	pbClose(pb)

	rownames(res) <- 1:nrow(res)
	x <- data.frame(ID=1:length(x), location=x)
	res <- merge(res, x, by='ID')
	return(res)	
}


