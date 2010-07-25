\name{InvDistW}
\Rdversion{1.1}

\alias{idwModel}
\alias{idwModel,SpatialPoints,SpatialPoints-method}
\alias{idwModel,matrix,matrix-method}
\alias{idwModel,data.frame,data.frame-method}


\title{Inverse-distance weighted model}

\description{
Inverse-distance weighted predictions for presence/absence data. Computed with the gstat function from the gstat package.
}

\usage{
idwModel(p, a, ...)
}

\arguments{
 \item{p}{Presence points. Two column matrix, data.frame, or a SpatialPoints* object}
 \item{a}{Absence points. Must be of the same class as p}
 \item{...}{Addtional arguments. None implemented}
}

\value{
An object of class InvDistWeightModel
}

\author{
Robert J. Hijmans
}

\keyword{ spatial  }

