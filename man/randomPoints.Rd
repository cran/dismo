\name{randomPoints}
\alias{randomPoints}

\title{
Random points
}

\description{
Generate random points that can be used to extract background values ("random-absence").
}

\usage{
randomPoints(mask, n, p, ext = NULL, extf = 1.1, excludep = TRUE, tryf=5, warn=2)
}
 
\arguments{
  \item{mask}{Raster* object; Cells with NA in this object (or first layer of the object) are excluded}
  \item{n}{Integer. Number of points}
  \item{p}{Presence points (if provided, random points won't be in the same cells (as defined by mask)}
  \item{ext}{Exent; to restrict sampling to a spatial extent}
  \item{extf}{Numeric. Multiplyer to adjust the size of extent 'ext'. The default increases of 1.1 increases the extent a little }
  \item{excludep}{Logical. If \code{TRUE}, presence points are exluded from background}
  \item{tryf}{numeric > 1. Multiplyer used for initial sample size from which the requested sample size is extracted after removing rows NA points (outside of mask) }
  \item{warn}{integer. 2 or higher give most warnings. 0 or lower give no warnings if sample size n is not reached }
}

\value{
matrix
}

\author{
Robert J. Hijmans
}


\keyword{spatial}

