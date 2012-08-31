#author: Jean-Pierre Rossi <jean-pierre.rossi@supagro.inra.fr>

mess <- function(x, v, full=TRUE) {

	messi <-function(p,v) {
		niinf <- length(which(v<=p))
		f<-100 * niinf / length(v)
		if(f==0) simi <- 100*(p-min(v))/(max(v)-min(v))
		if(0<f & f<=50) simi <- 2*f
		if(50<=f & f<100) simi <- 2*(100-f)
		if(f==100) simi <- 100*(max(v)-p)/(max(v)-min(v))
		return(simi)
    }

	E <- extract(x, y=1:ncell(x))
	r_mess <- x
	for (i in 1:(dim(E)[2])) {
		e <- data.frame(E[,i])
		r_mess[[i]][] <- apply(X=e, MARGIN=1, FUN=messi, v=v[,i])
    }
	
	rmess <- r_mess[[1]]
	E <- extract(x=r_mess, y=1:ncell(r_mess[[1]]))
	
	rmess[] <- apply(X=E, MARGIN=1, FUN=min)

	if(full==TRUE) {
		out <- addLayer(r_mess,rmess)
		layerNames(out)<-c(layerNames(x),"mess")
	}
	
	if(full==FALSE) out <- rmess
	return(out)
}
