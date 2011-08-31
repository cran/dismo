\name{randomPoints}
\alias{randomPoints}

\title{
Random points
}

\description{
Generate random points that can be used to extract background values ("random-absence"). The points are sampled (without replacement) from the cells that are not '\code{NA}' in raster '\code{mask}'. 

If the coordinate reference system (of \code{mask}) is longitude/latitude, sampling is weighted by the size of the cells. That is, because cells close to the equator are larger than cells closer to the poles, equatorial cells have a higher probability of being selected.
}

\usage{
randomPoints(mask, n, p, ext=NULL, extf=1.1, excludep=TRUE, cellnumbers=FALSE, tryf=5, warn=2)
}
 
\arguments{
  \item{mask}{Raster* object; Cells with NA in this object (or first layer of the object) are excluded}
  \item{n}{Integer. Number of points}
  \item{p}{Presence points (if provided, random points won't be in the same cells (as defined by mask)}
  \item{ext}{Exent; to restrict sampling to a spatial extent}
  \item{extf}{Numeric. Multiplyer to adjust the size of extent 'ext'. The default increases of 1.1 increases the extent a little (5\% at each side of the extent)}
  \item{excludep}{Logical. If \code{TRUE}, presence points are exluded from background}
  \item{cellnumbers}{Logical. If \code{TRUE}, cell numbers for ]code{mask} are returned rather than coordinates }
  \item{tryf}{numeric > 1. Multiplyer used for initial sample size from which the requested sample size is extracted after removing NA points (outside of mask) }
  \item{warn}{integer. 2 or higher give most warnings. 0 or lower give no warnings if sample size n is not reached }
}

\value{
matrix with coordinates, or, if \code{cellnumbers=TRUE}, a vector with cell numbers.
}

\author{
Robert J. Hijmans
}


\keyword{spatial}

