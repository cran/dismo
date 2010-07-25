# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : December 2009
# Version 0.1
# Licence GPL v3

gbif <- function(genus, species='', geo=TRUE, sp=FALSE, removeZeros=TRUE, download=TRUE, getAlt=TRUE, feedback=3) {
	
	if (! require(XML)) { stop('You need to install the XML package to use this function') }

	gbifxmlToDataFrame <- function(s) {
		# this sub-funciton was hacked from xmlToDataFrame in the XML package by Duncan Temple Lang
		doc = xmlInternalTreeParse(s)
		nodes <- getNodeSet(doc, "//to:TaxonOccurrence")
		if(length(nodes) == 0)   return(data.frame())
		varNames <- c("continent", "country", "stateProvince", "county", "locality",  "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "maximumElevationInMeters", "minimumElevationInMeters", "maximumDepthInMeters", "minimumDepthInMeters", "institutionCode", "collectionCode", "catalogNumber",  "basisOfRecordString", "collector", "earliestDateCollected", "latestDateCollected",  "gbifNotes")
		dims <- c(length(nodes), length(varNames)) 
   # create an empty data frame with as many rows and columns as needed.
		ans <- as.data.frame(replicate(dims[2], rep(as.character(NA), dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
		names(ans) <- varNames
    # Fill in the rows based on the names.
		for(i in seq(length = dims[1])) {
			ans[i,] <- xmlSApply(nodes[[i]], xmlValue)[varNames]
		}

		nodes <- getNodeSet(doc, "//to:Identification")
		varNames <- c("taxonName")
		dims = c(length(nodes), length(varNames)) 
		tax = as.data.frame(replicate(dims[2], rep(as.character(NA), dims[1]), simplify = FALSE), stringsAsFactors = FALSE)
		names(tax) = varNames
    # Fill in the rows based on the names.
		for(i in seq(length = dims[1])) {
			tax[i,] = xmlSApply(nodes[[i]], xmlValue)[varNames]
		}
		cbind(tax, ans)
	}


	if (sp) geo <- TRUE

    spec <- paste('scientificname=', trim(genus),'+', trim(species), sep='')
	if (geo) { cds <- '&coordinatestatus=true' 
	} else { cds <- '' }
    base <- 'http://data.gbif.org/ws/rest/occurrence/'
    url <- paste(base, 'count?', spec, cds, sep='')
    x <- readLines(url, warn=FALSE)
    x <- x[substr(x, 1, 20) == "<gbif:summary totalM"]
    n <- as.integer(unlist(strsplit(x, '\"'))[2])

	if (! download) { return(n) }
	
    if (n==0) {
        cat('no occurrences found\n')
        return(invisible(NULL))
    } else {
		if (feedback > 0) {
			cat(genus, species, ':', n, 'occurrences found\n')
			flush.console()
		}
	}

    iter <- n %/% 1000
	first <- TRUE
    for (group in 0:iter) {
        start <- group * 1000
		if (feedback > 1) {
			if (group == iter) { end <- n-1 } else { end <- start + 999 }
			if (group == 0) { cat('1-', end+1, sep='')  
			} else { cat('-', end+1, sep='')  }
			if ((group != 0 & group %% 20 == 0)  |  group == iter ) { cat('\n') }
			flush.console()
		}
		
        aurl <- paste(base, 'list?', spec, '&mode=processed&format=darwin&startindex=', format(start, scientific=FALSE), cds, sep='')
		zz <- gbifxmlToDataFrame(aurl)

		#s <- readLines(aurl, warn=FALSE)
        #try(zz <- gbifxmlToDataFrame(s), silent=TRUE)
		#if (class(zz) == 'try-error') {
		#	s <- sub("\002", "", s)
		#	zz <- gbifxmlToDataFrame(s)
		#}
		
		if (first) {
			z <- zz
			first <- FALSE
		} else {
			z <- rbind(z, zz)
		}
	}

	d <- as.Date(Sys.time())
	z <- cbind(z, d)
	names(z) <- c("species", "continent", "country", "adm1", "adm2", "locality", "lat", "lon", "coordUncertaintyM", "maxElevationM", "minElevationM", "maxDepthM", "minDepthM", "institution", "collection", "catalogNumber",  "basisOfRecord", "collector", "earliestDateCollected", "latestDateCollected",  "gbifNotes", "downloadDate")
	z[,'lon'] <- gsub(',', '.', z[,'lon'])
	z[,'lat'] <- gsub(',', '.', z[,'lat'])
	z[,'lon'] <- as.numeric(z[,'lon'])
	z[,'lat'] <- as.numeric(z[,'lat'])
	
	if (removeZeros) {
		i <- isTRUE(z[,'lon']== 0 & z[,'lat']==0)
		if (geo) {
			z <- z[!i,]
		} else {
			z[i,'lat'] <- NA 
			z[i,'lon'] <- NA 
		}
	}
		
	if (getAlt) {
		altfun <- function(x) {
					a <- mean(as.numeric(unlist(strsplit( gsub('-', ' ', gsub('m', '', ( gsub(",", "", gsub('\"', "", x))))),' ')), silent=TRUE), na.rm=TRUE)
					a[a==0] <- NA
					mean(a, na.rm=TRUE)
				}

		#elev <- apply(z[,c("maxElevationM", "minElevationM")], 1, FUN=altfun)
		#depth <- -1 * apply(z[,c("maxDepthM", "minDepthM")], 1, FUN=altfun)
		#alt <- apply(cbind(elev, depth), 1, FUN=function(x)mean(x, na.rm=TRUE))
		
		if (feedback<3) {
			w <- options('warn')
			options(warn=-1)
		}
		
		alt <- apply(z[,c("maxElevationM", "minElevationM", "maxDepthM", "minDepthM")], 1, FUN=altfun)
		
		if (feedback<3) options(warn=w)
	
		z <- cbind(z[,c("species", "continent", "country", "adm1", "adm2", "locality", "lat", "lon", "coordUncertaintyM")], 
		alt, 
		z[ ,c("institution", "collection", "catalogNumber",  "basisOfRecord", "collector", "earliestDateCollected", "latestDateCollected",  "gbifNotes", "downloadDate", "maxElevationM", "minElevationM", "maxDepthM", "minDepthM")])
	}
	
	if (sp) {
		i <- z[!(is.na(z[,'lon'] | is.na(z[,'lat']))), ]
		if (dim(z)[1] > 0) {
			coordinates(z) <- ~lon+lat
		}
	}

	return(z)
}

#sa <- gbif('solanum')
#sa <- gbif('solanum', '*')
#sa <- gbif('solanum', 'tuberosum')

