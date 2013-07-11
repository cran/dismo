\name{Voronoi Hull}
\Rdversion{1.1}

\alias{voronoiHull}
\alias{voronoiHull,SpatialPoints,SpatialPoints-method}
\alias{voronoiHull,matrix,matrix-method}
\alias{voronoiHull,data.frame,data.frame-method}
\alias{VoronoiHull-class}

\title{Voronoi hull model}

\description{
Voronoi polygons for presence/absence data
}

\usage{
voronoiHull(p, a, ...)
}

\arguments{
 \item{p}{Presence points. Two column matrix, data.frame, or a SpatialPoints* object}
 \item{a}{Absence points. Must be of the same class as p}
 \item{...}{Addtional arguments}
}

\value{
A VoronoiHull object (inherits from \code{\link{DistModel-class}})
}

\note{
This function is only correct when using a planar coordinate reference system (not longitude/latitude).
}

\seealso{\code{\link[dismo]{convexHull}}}

\author{
Robert J. Hijmans. Adapted from code by Carson Farmer: \url{http://www.carsonfarmer.com/?p=455}
}

\examples{
r <- raster(system.file("external/rlogo.grd", package="raster"))
# presence points
p <- matrix(c(17, 42, 85, 70, 19, 53, 26, 84, 84, 46, 48, 85, 4, 95, 48, 54, 66, 74, 50, 48, 
      28, 73, 38, 56, 43, 29, 63, 22, 46, 45, 7, 60, 46, 34, 14, 51, 70, 31, 39, 26), ncol=2)

# absence points
a <- matrix(c(30, 23, 5, 5, 31, 33, 91, 63, 60, 88, 93, 97, 65, 68, 85, 97, 35, 32, 29, 55,
      3, 8, 19, 71, 49, 36, 69, 41, 20, 28, 18, 9, 5, 9, 25, 71, 8, 32, 46, 60), ncol=2)

v <- voronoiHull(p, a)
	  
x <- predict(r, v)

\dontrun{
plot(x)
points(p, col='black', pch=20, cex=2)
points(a, col='red', pch=20, cex=2)
}
}

\keyword{ spatial  }

