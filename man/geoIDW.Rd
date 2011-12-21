\name{InvDistW}
\Rdversion{1.1}

\alias{geoIDW}
\alias{geoIDW,SpatialPoints,SpatialPoints-method}
\alias{geoIDW,matrix,matrix-method}
\alias{geoIDW,data.frame,data.frame-method}
\alias{InvDistWeightModel-class}


\title{Inverse-distance weighted model}

\description{
Inverse-distance weighted predictions for presence/absence data. Computed with the gstat function from the gstat package.
}

\usage{
geoIDW(p, a, ...)
}

\arguments{
 \item{p}{Presence points. Two column matrix, data.frame, or a SpatialPoints* object}
 \item{a}{Absence points. Must be of the same class as p}
 \item{...}{Addtional arguments. None implemented}
}

\value{
An object of class InvDistWeightModel (inherits from \code{\link{DistModel-class}})
}

\author{
Robert J. Hijmans
}

\keyword{ spatial  }

