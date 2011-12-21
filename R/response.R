
if (!isGeneric("response")) {
	setGeneric("response", function(x,...)
		standardGeneric("response"))
}	


setMethod("response", signature(x='DistModel'), 
function(x, var=NULL, at=median, range='pa', expand=10, rug=TRUE, ylim=c(0,1), col='red', lwd=2, add=FALSE, ... ) {
	stopifnot(range %in% c('p', 'pa'))
	d <- x@presence
	if (range == 'pa' & x@hasabsence) {
		d <- rbind(d, x@absence)
	}
	cn <- colnames(d)
	if (is.null(var)) {
		var <- cn
	}
	if (is.numeric(var)) {
		var <- cn[var]
	}
	if (length(var)==1) {
		# ?
	}
	var <- var[var %in% cn]
	if (length(var) == 0) { stop('var not found')	}

	.doResponse(x, var, at, d, cn, expand, rug, ylim, col, lwd, add, ... )

}
)

setMethod("response", signature(x="MaxEntReplicates"), 
function(x, var=NULL, at=median, range='pa', expand=10, rug=TRUE, ylim=c(0,1), col='red', lwd=2, add=FALSE, ... ) {
	stopifnot(range %in% c('p', 'pa'))
	
	for (i in 1:length(x@models)) {
		if (i > 1) {
			add = TRUE
		}
		d <- x[[i]]@presence
		if (range == 'pa' & x@hasabsence) {
			d <- rbind(d, x@absence)
		}
		cn <- colnames(d)
		if (is.null(var)) {
			var <- cn
		}
		if (is.numeric(var)) {
			var <- cn[var]
		}
		if (length(var)==1) {
			# ?
		}
		var <- var[var %in% cn]
		if (length(var) == 0) { stop('var not found')	}

		.doResponse(x[[i]], var, at, d, cn, expand, rug, ylim, col, lwd, add, ... )
	}
}
)


setMethod("response", signature(x='ANY'), 
function(x, var=NULL, at=median, range='pa', expand=10, rug=TRUE, ylim=c(0,1), col='red', lwd=2, add=FALSE, ... ) {
	stopifnot(range %in% c('p', 'pa'))

	cn <- names(attr(x$terms, "dataClasses")[-1])
	d <- x$model
	
	if (range != 'pa') {
		warning("range='p' is ignored")
	}
	if (is.null(var)) {
		var <- cn
	}
	if (is.numeric(var)) {
		cn <- names(attr(model$terms, "dataClasses")[-1])
		var <- cn[var]
	}
	if (length(var)==1) {
		# ?
	}
#	var <- var[var %in% cn]
	if (length(var) == 0) { stop('var not found')	}

	d <- d[, var]
	.doResponse(x, var, at, d, cn, expand, rug, ylim, col, lwd, add, ... )

}
)


.doResponse <- function(x, var, at, d, cn, expand, rug, ylim, col, lwd, add, ...) {
	
	if (length(var) > 1 & !add) {
		old.par <- par(no.readonly = TRUE) 
		on.exit(par(old.par))
		xs <- floor(sqrt(length(var)))
		ys <- ceiling(length(var) / xs)
		par(mfrow=c(xs, ys))
	}

	f <- sapply(d, is.factor)
	notf <- !f
	m <- matrix(nrow=1, ncol=ncol(d))
	if (is.function(at)) {
		if (sum(notf) > 0) {
			m[notf] <- as.numeric(apply(d[,notf,drop=FALSE], 2, at))
		} 
		if (sum(f) > 0) {
			m[f] <- as.numeric(apply(d[,f,drop=FALSE], 2, modal))
		}
		m <- matrix(m, nrow=1)
		colnames(m) <- cn
	} else {
		at <- at[cn]
		m <- as.vector(at)
		m <- matrix(m, nrow=1)
		colnames(m) <- names(at)
	}
	
	for (vr in var) {
		i <- which(cn==vr)
		v <- d[,i]
		if (is.factor(v)) {
			v <- as.numeric(levels(v))
			fact <- TRUE
		} else {
			fact <- FALSE
			v <- range(v)
			expand <- round(abs(expand))
			v <- v[1] + (-expand):(100+expand) * (v[2]-v[1])/100
		}

		mm <- matrix(rep(m[,-i], length(v)), nrow=length(v), byrow=T)
		colnames(mm) <- colnames(m)[-i]
		a <- cbind(v, mm)
		colnames(a)[1] <- vr
		
		p <- predict(x, a)
		if (add) {
			if (fact) {
				points(a[,1], p, col=col, lwd=lwd, ...)
			} else {
				points(a[,1], p, col=col, lwd=lwd, type='l', ...)			
			}
		} else {
			if (fact) {
				plot(a[,1], p, xlab=vr, ylab='predicted value', col=col, lwd=lwd, ylim=ylim, ...)
			} else {
				plot(a[,1], p, xlab=vr, ylab='predicted value', col=col, lwd=lwd, ylim=ylim, type='l', ...)
			}
			if (rug) {
				if (!is.factor(d[,i])) {
					rug(quantile(d[,i], probs = seq(0, 1, 0.1)), col='blue')
				}
			}
		}
	}
	
	if (length(var) == 1) {
		return(invisible(cbind(a[,1], p)))
	}
}


