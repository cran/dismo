
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
	
	if (length(var) > 1) {
		old.par <- par(no.readonly = TRUE) 
		on.exit(par(old.par))
		xs <- floor(sqrt(length(var)))
		ys <- ceiling(length(var) / xs)
		par(mfrow=c(xs, ys))
	}
	
	for (vr in var) {
		i <- which(cn==vr)
		v <- d[,i]
		if (is.factor(v)) {
			v <- levels(v)
		} else {
			v <- range(v)
			expand <- round(abs(expand))
			v <- v[1] + (-expand):(100+expand) * (v[2]-v[1])/100
		}
		if (is.function(at)) {
			m <- as.numeric(apply(d[,-i], 2, at))
			m <- data.frame(matrix(rep(m, each=length(v)), nrow=length(v)))
			colnames(m) <- cn[-i]
		} else {
			at <- at[cn[-i]]
			m <- as.vector(at)
			m <- data.frame(matrix(rep(m, each=length(v)), nrow=length(v)))
			colnames(m) <- names(at)
		}

		a <- cbind(v, m)
		colnames(a)[1] <- vr
		p <- predict(x, a)
		if (add) {
			points(a[,1], p, type='l', col=col, lwd=lwd, ...)
		} else {
			plot(a[,1], p, type='l', xlab=vr, ylab='predicted value', col=col, lwd=lwd, ylim=ylim, ...)
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


