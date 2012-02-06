# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("predict")) {
	setGeneric("predict", function(object, ...)
		standardGeneric("predict"))
}	


setMethod('predict', signature(object='MaxEntReplicates'), 
	function(object, x, ext=NULL, filename='', args="", ...) {
		filename <- extension(trim(filename), '')
		lst <- list()
		for (i in 1:length(object@models)) {
			if (filename != '') {
				fname <- paste(fname, '_', i, sep='')
			} else {
				fname <- ''
			}
			lst[[i]] <- predict(object@models[[i]], x, ext=ext, filename=fname, args=args, ...)
		}
		return(stack(lst))
	}
)


setMethod('predict', signature(object='MaxEnt'), 
	function(object, x, ext=NULL, args="", filename='', ...) {

		args <- c(args, "")

		#if (! file.exists(object@path)) {
		#	object@path <- paste(.meTmpDir(), '/', paste(round(runif(10) * 10), collapse = ""), sep='')
		#	if (! file.exists(object@path)) {
		#		dir.create(object@path, recursive=TRUE, showWarnings=TRUE)
		#	}
		#}
		#lambdas <- paste(object@path, '/lambdas.csv', sep="")
		#write.table(object@lambdas, file=lambdas, row.names=FALSE, col.names=FALSE, quote=FALSE)
		
		lambdas <- paste(object@lambdas, collapse='\n')
		variables <- colnames(object@presence)
		
		#MEversion <- .getMeVersion()

		mxe <- .jnew("mebridge") 		
		args <- c("-z", args)
		str <- .jcall(mxe, "S", "testPredictArgs", lambdas, args) 
		if (!is.null(str)) {
			stop("args not understood:\n", str)
		}

		filename <- trim(filename)
		
		if (inherits(x, "Raster")) {
			
			if (! all(colnames(object@presence)  %in%  layerNames(x) )) {
				stop('missing layers (or wrong names)')
			}
			
			out <- raster(x)
			if (!is.null(ext)) {
				out <- crop(out, ext)
				firstrow <- rowFromY(x, yFromRow(out, 1))
				firstcol <- colFromX(x, xFromCol(out, 1))
			} else {
				firstrow <- 1
				firstcol <- 1
			}
			ncols <- ncol(out)
		
			
			if (!canProcessInMemory(out, 3) & filename == '') {
				filename <- rasterTmpFile()
			}
			
			if (filename == '') {
				v <- matrix(ncol=nrow(out), nrow=ncol(out))
				inMemory <- TRUE
			} else {
				out <- writeStart(out, filename=filename, ... )
				inMemory <- FALSE
			}

			if (raster:::.doCluster()) {
				cl <- getCluster()
				on.exit( returnCluster() )
				nodes <- min(ceiling(out@nrows/10), length(cl)) # at least 10 rows per node
				cat('Using cluster with', nodes, 'nodes\n')
				flush.console()

				tr <- blockSize(out, minblocks=nodes)
				pb <- pbCreate(tr$n, ...)

				clFun <- function(i) {
					rr <- firstrow + tr$row[i] - 1
					rowvals <- getValuesBlock(x, row=rr, nrows=tr$nrows[i], firstcol, ncols)
					rowvals <- rowvals[,variables,drop=FALSE]
					res <- rep(NA, times=nrow(rowvals))
					rowvals <- na.omit(rowvals)
					if (length(rowvals) > 0) {
						rowvals[] <- as.numeric(rowvals)
					
						mxe <- .jnew("mebridge") 		
						p <- .jcall(mxe, "[D", "predict", lambdas, .jarray(colnames(rowvals)), .jarray(rowvals), args) 

						naind <- as.vector(attr(rowvals, "na.action"))
						if (!is.null(naind)) {
							res[-naind] <- p
						} else {
							res <- p
						}
						res[res == -9999] <- NA
					}	
					return(res)	
				} 
		
				for (i in 1:nodes) {
					sendCall(cl[[i]], clFun, i, tag=i)
				}
		        
				if (inMemory) {
					for (i in 1:tr$n) {
						pbStep(pb, i)
						d <- recvOneData(cl)
						if (! d$value$success) {
							stop('cluster error')
						}
						res <- matrix(d$value$value, nrow=ncol(out))		
						cols <- tr$row[d$value$tag]:(tr$row[d$value$tag]+dim(res)[2]-1)
						v[, cols] <- res

						ni <- nodes+i
						if (ni <= tr$n) {
							sendCall(cl[[d$node]], clFun, ni, tag=ni)
						}
					}
				} else {
					for (i in 1:tr$n) {
						pbStep(pb, i)
						d <- recvOneData(cl)
						if (! d$value$success ) { stop('cluster error') }
						out <- writeValues(out, d$value$value, tr$row[d$value$tag])
						
						if ((nodes + i) <= tr$n) {
							sendCall(cl[[d$node]], clFun, nodes+i, tag=i)
						}
					}
				}

			} else {

				tr <- blockSize(out, n=nlayers(x)+2)
				pb <- pbCreate(tr$n, ...)	
			
				for (i in 1:tr$n) {
					rr <- firstrow + tr$row[i] - 1
					rowvals <- getValuesBlock(x, row=rr, nrows=tr$nrows[i], firstcol, ncols)
					rowvals <- rowvals[,variables,drop=FALSE]
					res <- rep(NA, times=nrow(rowvals))
					rowvals <- na.omit(rowvals)
					if (length(rowvals) > 0) {
						rowvals[] <- as.numeric(rowvals)
						p <- .jcall(mxe, "[D", "predict", lambdas, .jarray(colnames(rowvals)), .jarray(rowvals, dispatch=TRUE), args) 

						naind <- as.vector(attr(rowvals, "na.action"))
						if (!is.null(naind)) {
							res[-naind] <- p
						} else {
							res <- p
						}
						res[res == -9999] <- NA
					}	
				
					if (inMemory) {
						res = matrix(res, nrow=ncol(out))		
						cols = tr$row[i]:(tr$row[i]+dim(res)[2]-1)
						v[, cols] <- res
					} else {
						out <- writeValues(out, res, tr$row[i])
					}
					pbStep(pb, i) 
				} 
			}
			
			pbClose(pb)
			if (inMemory) {
				out <- setValues(out, as.vector(v))
			} else {
				out <- writeStop(out)
			}
		
		
		} else { 
		
		
			if (inherits(x, "Spatial")) {
				x <- as.data.frame(x)
			}
			
			if (! all(colnames(object@presence) %in% colnames(x))) {
				stop('missing layers (or wrong names)')
			}
			
			
			x <- x[,variables,drop=FALSE]
			if (class(x) == 'data.frame') {
				for (i in 1:ncol(x)) {
					if (class(x[,i]) == 'factor') {
						x[,i] <- as.numeric(as.character(x[,i]))
					} else if (class(x[,i]) == 'character') {
						x[,i] <- as.numeric(x[,i])
					}
				}
			}
			
			out <- rep(NA, times=nrow(x))
			
			x <- na.omit(x)
			if (nrow(x) > 0) {
				x <- as.matrix(x)
				x[] <- as.numeric(x)
				p <- .jcall(mxe, "[D", "predict", lambdas, .jarray(colnames(x)), .jarray(x, dispatch=TRUE), args) 
				p[p == -9999] <- NA
				naind <- as.vector(attr(x, "na.action"))
				if (!is.null(naind)) {
					out[-naind] <- p
				} else {
					out <- p
				}
			} 
		}
		#try( file.remove(lambdas), silent=TRUE )
		out
	}
)

