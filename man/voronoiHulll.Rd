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

\seealso{\code{\link[dismo]{convexHull}}}

\author{
Robert J. Hijmans. Adapted from code by Carson Farmer: \url{http://www.carsonfarmer.com/?p=455}
}

\keyword{ spatial  }

