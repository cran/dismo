\name{Circles}

\alias{circles}
\alias{circles,SpatialPoints-method}
\alias{circles,matrix-method}
\alias{circles,data.frame-method}
\alias{CirclesRange-class}


\title{Circles range}


\description{
The circles model predicts that a species is present at sites within a certain distance from a training point, and absent further away.
}


\usage{
circles(p, ...)
}
 
\arguments{
  \item{p}{point locations (presence). Two column matrix, data.frame or SpatialPoints* object}
  \item{...}{ Additional arguments. See Details}
}

\details{
The following additional arguments can be supplied to the circles function:
\tabular{rll}{
 \tab \code{d}  \tab The radius of each circle in meters. A single number of a vector with elements corresponding to rows in 'p'. If missing the diameter is computed from the inter-point distance \cr
 \tab \code{n}  \tab How many vertices in the circle? Default is 360 \cr
 \tab \code{lonlat} \tab  Are these longitude/latidue data? Default value is FALSE \cr
  \tab \code{r}  \tab  Radius of the earth. Only relevant for longitude/latitude data. Default is 6378137 m\cr
} 

}

\value{
An object of class 'CirclesRange' (inherits from \code{\link{DistModel-class}})
}

\author{
Robert J. Hijmans
}

\seealso{
\code{\link[dismo]{predict}, \link[dismo]{geoDist}, \link[dismo]{convHull},  \link[dismo]{maxent}, \link[dismo]{domain}, 
\link[dismo]{mahal}, \link[dismo]{convexHull}  }
}


\examples{
r <- raster(system.file("external/rlogo.grd", package="raster"))
#presence data
pts <- matrix(c(17, 42, 85, 70, 19, 53, 26, 84, 84, 46, 48, 85, 4, 95, 48, 54, 66,
 74, 50, 48, 28, 73, 38, 56, 43, 29, 63, 22, 46, 45, 7, 60, 46, 34, 14, 51, 70, 31, 39, 26), ncol=2)
train <- pts[1:12, ]
test <- pts[13:20, ]
				 
cc <- circles(train, lonlat=FALSE)
predict(cc, test)

plot(r)
plot(cc@polygons, border='red', lwd=2, add=TRUE)
points(train, col='red', pch=20, cex=2)
points(test, col='black', pch=20, cex=2)

pr = predict(cc, r, progress='')
plot(r, legend=FALSE)
plot(pr, add=TRUE, col='blue')
points(test, col='black', pch=20, cex=2)
points(train, col='red', pch=20, cex=2)
}

\keyword{spatial}

