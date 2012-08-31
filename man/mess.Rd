\name{mess}
\alias{mess}

\title{Multivariate environmental similarity surfaces (MESS)}

\description{Compute multivariate environmental similarity surfaces (MESS), as described by Elith et al., 2010}

\usage{mess(x, v, full=TRUE)}

\arguments{
  \item{x}{A Raster* object}
  \item{v}{A data frame containing the reference values. Each column should correspond to one layer of the RasterStack object.}
  \item{full}{If \code{full=TRUE} \code{mess} returns a RasterStack object with n layers corresponding to the index for each individual layer of the input RasterStack and an additioanl layer with the MESS values.
If \code{full=FALSE} \code{mess} returns a raster layer with the MESS values.}
}


\details{
\code{v} can be produced using function \code{\link[raster]{extract}} .
}

\value{
Either a RasterStack object with n layers corresponding to the values for each individual layer of the input RasterStack and an additional layer with the mess values (if \code{full=TRUE}) or a single Raster object with the MESS values (if \code{full=FALSE}).
}

\references{
Elith J., M. Kearney M., and S. Phillips, 2010. The art of modelling range-shifting species. \href{http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2010.00036.x/abstract}{Methods in Ecology and Evolution 1:330-342}.
}

\author{
Jean-Pierre Rossi <jean-pierre.rossi@supagro.inra.fr>
}


\examples{
r1 <- raster(ncol=10, nrow=10) ; r1[] <- (1:ncell(r1))/10 + rnorm(ncell(r1))
r2 <- raster(ncol=10, nrow=10) ; r2[] <- (1:ncell(r1))/10 + rnorm(ncell(r2))
r3 <- raster(ncol=10, nrow=10) ; r3[] <- (1:ncell(r1))/10 + rnorm(ncell(r3))

rasters <- stack(r1,r2,r3)
plot(rasters)
xy <- expand.grid(c(10,30,50),c(10,30,50))
#plot(r1); points(xy)
refpt <- extract(rasters,xy)
mess <- mess(x=rasters, v=refpt, full=TRUE)
plot(mess)


\dontrun{
filename <- paste(system.file(package="dismo"), '/ex/bradypus.csv', sep='')
bradypus <- read.table(filename, header=TRUE, sep=',')
bradypus <- bradypus[,2:3]
files <- list.files(path=paste(system.file(package="dismo"),'/ex', sep=''), pattern='grd', full.names=TRUE )
predictors <- stack(files)
predictors <- dropLayer(x=predictors,i=9)
reference_points <- extract(predictors, bradypus)
mess.out <- mess(x=predictors, v=reference_points, full=TRUE)
plot(mess.out)
}

}

